{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main, writeCostingScripts) where

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , Result (..), fromJSON, defaultOptions, Options(..))
import           Data.Default                        (def)
import           Data.Monoid                         (Last (..))
import qualified Data.OpenApi                        as OpenApi
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.Contracts.Game               as Game

import           Oracle.PAB
import qualified Oracle.Core                         as Oracle
import qualified Oracle.Swap                         as Swap

import           Plutus.Trace.Emulator.Extract       (writeScriptsTo, ScriptsConfig (..), Command (..))
import           Ledger.Index                        (ValidatorMode(..))
import           Wallet.Emulator.Wallet
import           Wallet.Types                        (ContractInstanceId (..))

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (knownWallet 1) Init
    cs       <- waitForLast (cidInit :: ContractInstanceId)
    _        <- Simulator.waitUntilFinished cidInit

    cidOracle <- Simulator.activateContract (knownWallet 1) $ Oracle cs
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    --oracle    <- waitForLast (cidOracle :: ContractInstanceId)

    -- Example of spinning up a game instance on startup
    -- void $ Simulator.activateContract (Wallet 1) GameContract
    -- You can add simulator actions here:
    -- Simulator.observableState
    -- etc.
    -- That way, the simulation gets to a predefined state and you don't have to
    -- use the HTTP API for setup.

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

-- | An example of computing the script size for a particular trace.
-- Read more: <https://plutus.readthedocs.io/en/latest/plutus/howtos/analysing-scripts.html>
writeCostingScripts :: IO ()
writeCostingScripts = do
  let config = ScriptsConfig { scPath = "/tmp/plutus-costing-outputs/", scCommand = cmd }
      cmd    = Scripts { unappliedValidators = FullyAppliedValidators }
      -- Note: Here you can use any trace you wish.
      trace  = correctGuessTrace
  (totalSize, exBudget) <- writeScriptsTo config "game" trace def
  putStrLn $ "Total size = " <> show totalSize
  putStrLn $ "ExBudget = " <> show exBudget


data StarterContracts = GameContract
                      | Init
                      | Oracle CurrencySymbol
                      -- | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass OpenApi.ToSchema

-- NOTE: Because 'StarterContracts' only has one constructor, corresponding to
-- the demo 'Game' contract, we kindly ask aeson to still encode it as if it had
-- many; this way we get to see the label of the contract in the API output!
-- If you simple have more contracts, you can just use the anyclass deriving
-- statement on 'StarterContracts' instead:
--
--    `... deriving anyclass (ToJSON, FromJSON)`
instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StarterContracts where
    pretty = viaShow

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [GameContract, Init]
    getSchema =  \case
        GameContract -> Builtin.endpointsToSchemas @Game.GameSchema
        Init         -> Builtin.endpointsToSchemas @Empty
        Oracle _     -> Builtin.endpointsToSchemas @Oracle.OracleSchema
        -- Swap _       -> Builtin.endpointsToSchemas @Swap.SwapSchema
    getContract = \case
        GameContract -> SomeBuiltin (Game.game @ContractError)
        Init         -> SomeBuiltin (initContract)
        Oracle cs    -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        -- Swap oracle  -> SomeBuiltin $ Swap.swap oracle

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)

