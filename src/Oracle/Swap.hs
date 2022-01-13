{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import           Plutus.ChainIndex.Tx
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           Oracle.Core
import           Oracle.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- Contract.ownPubKeyHash
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, ChainIndexTxOut, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxosTxOutTxAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: (ChainIndexTxOut, ChainIndexTx) -> Maybe PubKeyHash
    f (o, tx) = do
        dh        <- txOutDatumHash $ toTxOut o
        (Datum d) <- Map.lookup dh $ _citxData tx
        PlutusTx.fromBuiltinData d

    g :: (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) -> Maybe (TxOutRef, ChainIndexTxOut, PubKeyHash)
    g (oref, (o, tx)) = do
        pkh <- f (o, tx)
        guard $ p pkh
        return (oref, o, pkh)

retrieveSwaps :: Oracle -> () -> Contract w s Text ()
retrieveSwaps oracle _ = do
    --pkh <- pubKeyHash <$> ownPubKey
    pkh <- Contract.ownPubKeyHash
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. Oracle -> () -> Contract w s Text ()
useSwap oracle _ = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, (o, tx), x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- Contract.ownPubKeyHash
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (toTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ toTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> ChainIndexTxOut -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ toTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, ChainIndexTxOut, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = h $ awaitPromise (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    --offer :: Contract (Last Value) SwapSchema Text ()
    offer = 
        --amt <- endpoint @"offer"
        --offerSwap oracle amt
        endpoint @"offer" $ offerSwap oracle

    --retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = 
        --endpoint @"retrieve"
        --retrieveSwaps oracle
        endpoint @"retrieve" $ retrieveSwaps oracle

    --use :: Contract (Last Value) SwapSchema Text ()
    use =
        --endpoint @"use"
        --useSwap oracle
        endpoint @"use" $ useSwap oracle

    --funds :: Contract (Last Value) SwapSchema Text ()
    funds = 
        --endpoint @"funds"
        --v <- ownFunds
        endpoint @"funds" funds'
        --v <- ownFunds
        --tell $ Last $ Just v
    
    funds' :: () -> Contract (Last Value) SwapSchema Text ()
    funds' _ = do
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
