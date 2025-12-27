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
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data CroxVaultDatum = CroxVaultDatum
    { cvUser        :: PubKeyHash
    , cvTokenSymbol :: CurrencySymbol
    , cvTokenName   :: TokenName
    , cvAmount      :: Integer
    , cvLockStart   :: POSIXTime
    , cvLockPeriod  :: POSIXTime
    , cvYieldRate   :: Integer        -- yield rate updated by AI oracle
    , cvAdmin       :: PubKeyHash     -- CroxVault admin (AI oracle authority)
    }
PlutusTx.unstableMakeIsData ''CroxVaultDatum

data CroxVaultAction = Deposit | Withdraw | UpdateYield
PlutusTx.unstableMakeIsData ''CroxVaultAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE isAfterLock #-}
isAfterLock :: CroxVaultDatum -> POSIXTimeRange -> Bool
isAfterLock dat range =
    let unlockTime = cvLockStart dat + cvLockPeriod dat
    in Interval.contains (Interval.from unlockTime) range

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: CroxVaultDatum -> CroxVaultAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      Deposit ->
          traceIfFalse "deposit not signed by user" (txSignedBy info (cvUser dat)) &&
          traceIfFalse "invalid deposit amount"      (inputAmount >= cvAmount dat)

      Withdraw ->
          traceIfFalse "withdraw not signed by user" (txSignedBy info (cvUser dat)) &&
          traceIfFalse "lock period not over"        (isAfterLock dat txRange) &&
          traceIfFalse "insufficient withdrawal"     (outputAmount >= expectedReturn)

      UpdateYield ->
          traceIfFalse "update not signed by admin"  (txSignedBy info (cvAdmin dat))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    inputValue :: Value
    inputValue = case findOwnInput ctx of
        Nothing -> traceError "no input"
        Just i  -> txOutValue (txInInfoResolved i)

    outputValue :: Value
    outputValue =
        case getContinuingOutputs ctx of
          [o] -> txOutValue o
          _   -> traceError "expected exactly one continuing output"

    cs :: CurrencySymbol
    cs = cvTokenSymbol dat

    tn :: TokenName
    tn = cvTokenName dat

    inputAmount :: Integer
    inputAmount = valueOf inputValue cs tn

    outputAmount :: Integer
    outputAmount = valueOf outputValue cs tn

    -- Simple reward computation (could be AI oracle-based off-chain)
    expectedReturn :: Integer
    expectedReturn = cvAmount dat + (cvAmount dat * cvYieldRate dat `divide` 100)

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @CroxVaultDatum d
        red = unsafeFromBuiltinData @CroxVaultAction r
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

    writeValidator "croxvault.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- CroxVault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "CroxVault validator generated successfully."
