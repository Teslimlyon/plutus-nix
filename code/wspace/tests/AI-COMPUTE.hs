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

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

-- AI compute job escrow
data JobDatum = JobDatum
    { jdWorker    :: PubKeyHash
    , jdVerifier  :: PubKeyHash
    , jdReward    :: Integer
    , jdDeadline  :: POSIXTime
    , jdSpecHash  :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''JobDatum

-- Data labeling bounty
data LabelTaskDatum = LabelTaskDatum
    { ldWorker      :: PubKeyHash
    , ldItems       :: Integer
    , ldRewardPer   :: Integer
    , ldQAThreshold :: Integer
    }
PlutusTx.unstableMakeIsData ''LabelTaskDatum

-- Redeemer actions
data AIAction = SubmitJobResult BuiltinByteString
              | VerifyJobResult Bool
              | SubmitLabels Integer
              | QAAdjust Integer
PlutusTx.unstableMakeIsData ''AIAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE withinDeadline #-}
withinDeadline :: POSIXTime -> ScriptContext -> Bool
withinDeadline dl ctx =
    let info = scriptContextTxInfo ctx
    in Interval.contains (Interval.to dl) (txInfoValidRange info)

{-# INLINABLE payoutSufficient #-}
payoutSufficient :: PubKeyHash -> Integer -> ScriptContext -> Bool
payoutSufficient to amt ctx =
    let v = valuePaidTo (scriptContextTxInfo ctx) to
    in valueOf v adaSymbol adaToken >= amt

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkAIValidator #-}
mkAIValidator :: (JobDatum, LabelTaskDatum) -> AIAction -> ScriptContext -> Bool
mkAIValidator (job, label) action ctx =
    case action of
        -- Worker submits result hash
        SubmitJobResult resultHash ->
            traceIfFalse "not signed by worker" (signedBy (jdWorker job) ctx) &&
            traceIfFalse "submission after deadline" (withinDeadline (jdDeadline job) ctx)
        -- Verifier approves/rejects job
        VerifyJobResult approved ->
            traceIfFalse "not signed by verifier" (signedBy (jdVerifier job) ctx) &&
            if approved
               then traceIfFalse "worker not paid" (payoutSufficient (jdWorker job) (jdReward job) ctx)
               else True
        -- Worker submits labels
        SubmitLabels n ->
            traceIfFalse "not signed by worker" (signedBy (ldWorker label) ctx) &&
            traceIfFalse "submitted more labels than task" (n <= ldItems label)
        -- QA adjusts reward based on quality
        QAAdjust n ->
            traceIfFalse "QA adjustment below threshold" (n <= ldQAThreshold label)

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkAIValidatorUntyped #-}
mkAIValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAIValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @(JobDatum, LabelTaskDatum) d
        red = unsafeFromBuiltinData @AIAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkAIValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkAIValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin = Builtins.toBuiltin strictBS
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
    writeValidator "ai_validator.plutus" validator
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
    putStrLn "\n--- AI Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "AI compute + labeling validator generated successfully."