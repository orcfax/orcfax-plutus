{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Oracle.PAB
    --( OracleContracts (..)
    ( usdt
    , wallets
    , oracleParams
    , initContract
    , Empty
    , CurrencySymbol
    ) where

import           Control.Monad                       (forM_, when)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.Contract
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Wallet.Emulator.Wallet

import qualified Oracle.Core        as Oracle

--data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
 --   deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

--instance Pretty OracleContracts where
--    pretty = viaShow



wallets :: [Wallet]
wallets = [knownWallet i | i <- [1 .. 5]]

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

initContract :: Contract (Last CurrencySymbol) Empty Text ()
initContract = do
    ownPKH <- Plutus.Contract.ownPubKeyHash
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPKH [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur
        v  = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = walletPubKeyHash w
        when (pkh /= ownPKH) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ getCardanoTxId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000
