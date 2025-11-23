{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (Value, valueOf, adaSymbol, adaToken)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API (for Bech32 addresses)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum Types
------------------------------------------------------------------------

data PartNFT = PartNFT
    { partNSN      :: BuiltinByteString
    , partLot      :: BuiltinByteString
    , partCompliance :: BuiltinByteString
    , partOwner    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''PartNFT

data RoleSBT = RoleSBT
    { sbtUnit      :: BuiltinByteString
    , sbtRole      :: BuiltinByteString
    , sbtExpiry    :: POSIXTime
    , sbtOwner     :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''RoleSBT

data MilitaryAction = TransferPart | CheckAccess
PlutusTx.unstableMakeIsData ''MilitaryAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: TxInfo -> PubKeyHash -> Bool
signedBy info pkh = txSignedBy info pkh

{-# INLINABLE getCurrentTime #-}
getCurrentTime :: POSIXTimeRange -> POSIXTime
getCurrentTime range = case ivTo range of
    UpperBound (Finite t) _ -> t
    _ -> traceError "invalid time bound"

{-# INLINABLE hasValidSBT #-}
hasValidSBT :: TxOut -> PubKeyHash -> BuiltinByteString -> POSIXTime -> Bool
hasValidSBT o pkh role now =
    case txOutDatum o of
        OutputDatum (Datum d) ->
            let sbt = unsafeFromBuiltinData @RoleSBT d
            in sbtOwner sbt == pkh &&
               sbtRole sbt == role &&
               sbtExpiry sbt >= now
        _ -> False

{-# INLINABLE pubKeyHashFromTxInfo #-}
pubKeyHashFromTxInfo :: TxInfo -> PubKeyHash
pubKeyHashFromTxInfo info =
    case txInfoSignatories info of
        (x:_) -> x
        _     -> traceError "no signatory"

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkMilitaryValidator #-}
mkMilitaryValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkMilitaryValidator d r c =
    let info = unsafeFromBuiltinData @ScriptContext c
        action = unsafeFromBuiltinData @MilitaryAction r
        txInfo = scriptContextTxInfo info
        now = getCurrentTime (txInfoValidRange txInfo)
    in case action of
        TransferPart ->
            any (\i ->
                case txOutDatum (txInInfoResolved i) of
                    OutputDatum (Datum dat) ->
                        let part = unsafeFromBuiltinData @PartNFT dat
                        in traceIfFalse "not signed by owner" (signedBy txInfo (partOwner part))
                    _ -> False
            ) (txInfoInputs txInfo)
        CheckAccess ->
            any (\o -> hasValidSBT o (pubKeyHashFromTxInfo txInfo) "OFFICER" now) (txInfoOutputs txInfo)

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkMilitaryValidator d r c
    then ()
    else traceError "Validator failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        strict   = LBS.toStrict bytes
        builtin  = Builtins.toBuiltin strict
    in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (CS.PlutusScript CS.PlutusScriptV2 plutusScript)
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

    writeValidator "military_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Military Procurement Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "Military procurement validator generated successfully."
