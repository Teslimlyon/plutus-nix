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

import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

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

data EduDatum = EduDatum
    { student         :: PubKeyHash           -- Student’s wallet
    , stakeAmount     :: Integer              -- Staked ADA
    , lessonsDone     :: Integer              -- Lessons completed (0–10)
    , totalLessons    :: Integer              -- Total lessons (fixed: 10)
    , rewardPerLesson :: Integer              -- Reward per lesson (0.5 ADA)
    , lessonNotes     :: [BuiltinByteString]  -- Lesson URLs / IPFS hashes
    }
PlutusTx.unstableMakeIsData ''EduDatum

data EduAction
    = EnrollStudent
    | CompleteLesson
    | ClaimAll
PlutusTx.unstableMakeIsData ''EduAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE adaPaidTo #-}
adaPaidTo :: ScriptContext -> PubKeyHash -> Integer
adaPaidTo ctx pkh =
    let v = valuePaidTo (scriptContextTxInfo ctx) pkh
    in valueOf v adaSymbol adaToken

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkEduFiValidator #-}
mkEduFiValidator :: EduDatum -> EduAction -> ScriptContext -> Bool
mkEduFiValidator dat action ctx =
    case action of

        -- Student stakes ADA to enroll
        EnrollStudent ->
            traceIfFalse "stake must be positive" (stakeAmount dat > 0) &&
            traceIfFalse "invalid lesson count" (totalLessons dat == 10) &&
            traceIfFalse "student signature missing" (signedBy (student dat) ctx)

        -- Student marks a lesson as completed
        CompleteLesson ->
            traceIfFalse "student signature missing" (signedBy (student dat) ctx) &&
            traceIfFalse "all lessons already completed" (lessonsDone dat < totalLessons dat)

        -- Student claims total rewards and stake
        ClaimAll ->
            traceIfFalse "student signature missing" (signedBy (student dat) ctx) &&
            traceIfFalse "not all lessons completed" (lessonsDone dat == totalLessons dat) &&
            traceIfFalse "insufficient payout to student" paidCorrectAmount
          where
            totalReward :: Integer
            totalReward = rewardPerLesson dat * totalLessons dat

            expectedTotal :: Integer
            expectedTotal = stakeAmount dat + totalReward

            paidCorrectAmount :: Bool
            paidCorrectAmount =
                adaPaidTo ctx (student dat) >= expectedTotal

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

    writeValidator "edufi_validator_nosig.plutus" validator
    writeCBOR      "edufi_validator_nosig.cbor"   validator

    let vh      = plutusValidatorHash validator
        addr    = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
        cborHex = validatorToCBORHex validator

    putStrLn "\n--- EduFi Learn-to-Earn Smart Contract (No Educator Sig) ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."
    putStrLn "--------------------------------------------------------------"
    putStrLn "EduFi validator (self-learning mode) compiled successfully."
