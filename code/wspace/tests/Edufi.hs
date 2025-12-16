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
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as T

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data EduDatum = EduDatum
    { edStudent      :: PubKeyHash
    , edStakeAmount  :: Integer
    , edLessonIndex  :: Integer
    , edReward       :: Integer
    , edDeadline     :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''EduDatum

data EduAction = ClaimReward
PlutusTx.unstableMakeIsData ''EduAction

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkEduValidator #-}
mkEduValidator :: EduDatum -> EduAction -> ScriptContext -> Bool
mkEduValidator dat action ctx =
    case action of
        ClaimReward ->
            traceIfFalse "student signature missing" (txSignedBy info (edStudent dat)) &&
            traceIfFalse "too early to claim" afterDeadline &&
            traceIfFalse "reward not paid" rewardPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange

    rewardPaid :: Bool
    rewardPaid =
      let v = valuePaidTo info (edStudent dat)
      in valueOf v adaSymbol adaToken >= edReward dat

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkEduValidatorUntyped #-}
mkEduValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkEduValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EduDatum d
        red = unsafeFromBuiltinData @EduAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkEduValidator dat red ctx then () else error ()

eduValidator :: Validator
eduValidator = mkValidatorScript $$(PlutusTx.compile [|| mkEduValidatorUntyped ||])

------------------------------------------------------------------------
-- Serialization to CBOR Hex
------------------------------------------------------------------------

validatorToCborHex :: Validator -> String
validatorToCborHex val =
    let bytes  = Serialise.serialise val
        strict = LBS.toStrict bytes
        hex    = Base16.encode strict
    in T.unpack (T.decodeUtf8 hex)

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
    Address (ScriptCredential (plutusValidatorHash eduValidator)) Nothing

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
-- File Writing
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

    writeValidator "eduValidator.plutus" eduValidator

    let vh      = plutusValidatorHash eduValidator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network eduValidator
        cborHex = validatorToCborHex eduValidator

    putStrLn "\n--- EduFi Learn-to-Earn Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "CBOR Hex:"
    putStrLn cborHex
    putStrLn "---------------------------------"
    putStrLn "EduFi Learn-to-Earn validator generated successfully."
