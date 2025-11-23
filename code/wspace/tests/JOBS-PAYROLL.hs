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
import GHC.Generics (Generic)

-- Plutus Core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (Value, valueOf, adaSymbol, adaToken)
import qualified PlutusTx.Builtins.Class as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API (for Bech32 addresses)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum Types
------------------------------------------------------------------------

data StreamDatum = StreamDatum
    { sEmployee :: PubKeyHash
    , sEmployer :: PubKeyHash
    , sRate     :: Integer
    , sStart    :: POSIXTime
    , sEnd      :: POSIXTime
    , sClaimed  :: Integer
    }
PlutusTx.unstableMakeIsData ''StreamDatum

data GigDatum = GigDatum
    { gPayer      :: PubKeyHash
    , gWorker     :: PubKeyHash
    , gMilestones :: [(Integer, Bool)]
    , gArbiter    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''GigDatum

data ReferralDatum = ReferralDatum
    { rReferrer  :: PubKeyHash
    , rCandidate :: PubKeyHash
    , rBonus     :: Integer
    , rCliff     :: POSIXTime
    , rClaimed   :: Bool
    }
PlutusTx.unstableMakeIsData ''ReferralDatum

data JobAction = ClaimStream | PayMilestone Integer | ClaimReferral
PlutusTx.unstableMakeIsData ''JobAction

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
    _                       -> traceError "invalid time bound"

{-# INLINABLE posixToInteger #-}
posixToInteger :: POSIXTime -> Integer
posixToInteger (POSIXTime t) = t

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkJobValidator #-}
mkJobValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkJobValidator d r c =
    let info = unsafeFromBuiltinData @ScriptContext c
        txInfo = scriptContextTxInfo info
    in case unsafeFromBuiltinData @JobAction r of

        ClaimStream ->
            let dat = unsafeFromBuiltinData @StreamDatum d
                currentTime = getCurrentTime (txInfoValidRange txInfo)

                elapsed =
                    if currentTime <= sStart dat then 0
                    else posixToInteger currentTime - posixToInteger (sStart dat)

                cappedElapsed =
                    if currentTime > sEnd dat
                        then posixToInteger (sEnd dat) - posixToInteger (sStart dat)
                        else elapsed

                totalOwed = (cappedElapsed * sRate dat) - sClaimed dat
                paid = valueOf (valuePaidTo txInfo (sEmployee dat)) adaSymbol adaToken
            in if signedBy txInfo (sEmployee dat) && paid >= totalOwed
               then ()
               else error ()

        PayMilestone idx ->
            let dat = unsafeFromBuiltinData @GigDatum d
                (amount, completed) = gMilestones dat !! idx
                paid = valueOf (valuePaidTo txInfo (gWorker dat)) adaSymbol adaToken
            in if signedBy txInfo (gPayer dat) && (not completed) && paid >= amount
               then ()
               else error ()

        ClaimReferral ->
            let dat = unsafeFromBuiltinData @ReferralDatum d
                currentTime = getCurrentTime (txInfoValidRange txInfo)
                paid = valueOf (valuePaidTo txInfo (rReferrer dat)) adaSymbol adaToken
            in if signedBy txInfo (rReferrer dat)
               && posixToInteger currentTime >= posixToInteger (rCliff dat)
               && not (rClaimed dat)
               && paid >= rBonus dat
               then ()
               else error ()

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkJobValidator ||])

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes   = Serialise.serialise val
        strict  = LBS.toStrict bytes
        builtin = Builtins.toBuiltin strict
    in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------
-- Off-chain Bech32
------------------------------------------------------------------------

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
    writeValidator "jobs_validator.plutus" validator

    putStrLn "\n--- Jobs Smart Contract ---"
    putStrLn $ "Validator Hash: " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Script Address: " <> P.show plutusScriptAddress
    putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
    putStrLn "----------------------------"
