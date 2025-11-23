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

-- Vesting schedule for beneficiary
data VestingDatum = VestingDatum
    { vdBeneficiary :: PubKeyHash
    , vdStart       :: POSIXTime
    , vdCliff       :: POSIXTime
    , vdEnd         :: POSIXTime
    , vdTotal       :: Integer
    }
PlutusTx.unstableMakeIsData ''VestingDatum

-- Compliance reference data
data ComplianceDatum = ComplianceDatum
    { cdKYC         :: BuiltinByteString
    , cdWhitelisted :: [PubKeyHash]
    }
PlutusTx.unstableMakeIsData ''ComplianceDatum

-- Redeemer actions
data TokenAction = MintToken Integer | BurnToken Integer | ClaimVested | TransferToken PubKeyHash
PlutusTx.unstableMakeIsData ''TokenAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE inVestingPeriod #-}
inVestingPeriod :: VestingDatum -> ScriptContext -> Bool
inVestingPeriod vd ctx =
    let info = scriptContextTxInfo ctx
        txRange = txInfoValidRange info
        vestRange = Interval.interval (vdCliff vd) (vdEnd vd)
    in Interval.overlaps txRange vestRange

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE isWhitelisted #-}
isWhitelisted :: PubKeyHash -> ComplianceDatum -> Bool
isWhitelisted pkh cd = elem pkh (cdWhitelisted cd)

{-# INLINABLE bondingPrice #-}
bondingPrice :: Integer -> Integer
bondingPrice amount = amount * 1000  -- Simplified linear curve: 1 token = 1000 lovelace

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkTokenValidator #-}
mkTokenValidator :: (VestingDatum, ComplianceDatum) -> TokenAction -> ScriptContext -> Bool
mkTokenValidator (vd, cd) action ctx =
    case action of
        MintToken amt ->
            traceIfFalse "must be signed by beneficiary" (signedBy (vdBeneficiary vd) ctx) &&
            traceIfFalse "amount must be positive" (amt > 0)
        BurnToken amt ->
            traceIfFalse "must be signed by beneficiary" (signedBy (vdBeneficiary vd) ctx) &&
            traceIfFalse "amount must be positive" (amt > 0)
        ClaimVested ->
            traceIfFalse "not in vesting period" (inVestingPeriod vd ctx) &&
            traceIfFalse "claim not signed by beneficiary" (signedBy (vdBeneficiary vd) ctx)
        TransferToken to ->
            traceIfFalse "recipient not whitelisted" (isWhitelisted to cd)

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkTokenValidatorUntyped #-}
mkTokenValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTokenValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @(VestingDatum, ComplianceDatum) d
        red = unsafeFromBuiltinData @TokenAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkTokenValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkTokenValidatorUntyped ||])

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
    writeValidator "token_validator.plutus" validator
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
    putStrLn "\n--- Token Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Token validator (curves, vesting, compliance) generated successfully."