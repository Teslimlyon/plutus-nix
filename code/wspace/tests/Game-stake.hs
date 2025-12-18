{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data GameDatum = GameDatum
    { gdAdmin       :: PubKeyHash
    , gdPlayers     :: [PubKeyHash]
    , gdStakeAmount :: Integer
    , gdDeadline    :: POSIXTime
    , gdRewardCS    :: CurrencySymbol
    , gdRewardTN    :: TokenName
    } 
PlutusTx.unstableMakeIsData ''GameDatum

data GameAction = Stake | ClaimReward | Refund
PlutusTx.unstableMakeIsData ''GameAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE findOwnInputValue #-}
findOwnInputValue :: ScriptContext -> Value
findOwnInputValue ctx =
    case findOwnInput ctx of
        Nothing -> traceError "no script input"
        Just i  -> txOutValue (txInInfoResolved i)

{-# INLINABLE playerSigned #-}
playerSigned :: [PubKeyHash] -> TxInfo -> Bool
playerSigned [] _     = False
playerSigned (p:ps) i = txSignedBy i p || playerSigned ps i

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum -> GameAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      Stake ->
          traceIfFalse "stake too small" (stakeEnough stakeVal) &&
          traceIfFalse "stake not locked" lockedBeforeDeadline
      ClaimReward ->
          traceIfFalse "winner not signed" (playerSigned (gdPlayers dat) info) &&
          traceIfFalse "deadline not passed" afterDeadline &&
          traceIfFalse "reward missing" playerGetsReward
      Refund ->
          traceIfFalse "not admin" (txSignedBy info (gdAdmin dat)) &&
          traceIfFalse "too early" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    stakeVal :: Value
    stakeVal = findOwnInputValue ctx

    stakeEnough :: Value -> Bool
    stakeEnough v = valueOf v adaSymbol adaToken >= gdStakeAmount dat

    lockedBeforeDeadline :: Bool
    lockedBeforeDeadline = Interval.contains (Interval.to (gdDeadline dat)) txRange

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (gdDeadline dat + 1)) txRange

    playerGetsReward :: Bool
    playerGetsReward =
      let vPaid = mconcat [ valuePaidTo info p | p <- gdPlayers dat ]
      in valueOf vPaid (gdRewardCS dat) (gdRewardTN dat) >= 1

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @GameDatum d
        red = unsafeFromBuiltinData @GameAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "gamestake.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- GameStake DAO Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "------------------------------------"
    putStrLn "GameStake DAO validator generated successfully."
