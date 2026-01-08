{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum & Redeemer
------------------------------------------------------------------------

data StudentDatum = StudentDatum
    { student      :: PubKeyHash
    , stakeAmount  :: Integer
    , lessonsDone  :: Integer
    , totalLessons :: Integer
    , rewardAmount :: Integer
    }
PlutusTx.unstableMakeIsData ''StudentDatum

data StudentAction
    = Stake Integer        -- amount staked
    | CompleteLesson
    | WithdrawReward
PlutusTx.unstableMakeIsData ''StudentAction

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkEduFiValidator #-}
mkEduFiValidator :: StudentDatum -> StudentAction -> ScriptContext -> Bool
mkEduFiValidator dat action ctx =
    case action of

        -- Stake tokens to enroll
        Stake amt ->
            traceIfFalse "Stake must be positive" (amt > 0)

        -- Complete a lesson
        CompleteLesson ->
            traceIfFalse "Not a registered student" (signedBy (student dat) ctx) &&
            traceIfFalse "All lessons already completed" (lessonsDone dat < totalLessons dat)

        -- Withdraw unlocked reward
        WithdrawReward ->
            traceIfFalse "Not a registered student" (signedBy (student dat) ctx) &&
            traceIfFalse "No reward available yet" (lessonsDone dat > 0)

------------------------------------------------------------------------
-- Untyped Wrapper
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkEduFiValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash & Script Address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------
-- Bech32 Script Address (Off-chain)
------------------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- CBOR HEX
------------------------------------------------------------------------

validatorToCBORHex :: Validator -> String
validatorToCBORHex val =
    let bytes = LBS.toStrict $ Serialise.serialise val
    in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
        let hi = P.fromIntegral b `P.div` 16
            lo = P.fromIntegral b `P.mod` 16
        in [ hexChars P.!! hi, hexChars P.!! lo ]

------------------------------------------------------------------------
-- File Writer
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "CBOR hex written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "edufi_pool.plutus" validator
    writeCBOR      "edufi_pool.cbor"   validator

    let vh      = plutusValidatorHash validator
        addr    = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
        cborHex = validatorToCBORHex validator

    putStrLn "\n--- EduFi Learn-to-Earn Pool ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."
    putStrLn "--------------------------------"
