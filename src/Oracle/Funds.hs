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

module Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: Contract w s Text Value
ownFunds = do
    pkh    <- ownPubKeyHash
    --ppkh    <- ownPaymentPubKeyHash
    --utxos <- utxosAt $ pubKeyAddress pk
    utxos <- utxosAt $ pubKeyHashAddress pkh
    --let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    let v = mconcat $ Map.elems $ txOutValue . toTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
