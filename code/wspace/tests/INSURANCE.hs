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
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-- Datums & Redeemers
data PolicyType = Parametric | Mutual
PlutusTx.unstableMakeIsData ''PolicyType

data PolicyDatum = PolicyDatum
    { pdOwner   :: PubKeyHash
    , pdTrigger :: BuiltinByteString -- oracle feed identifier
    , pdPayout  :: Integer           -- payout in lovelace
    , pdExpiry  :: POSIXTime
    , pdType    :: PolicyType
    }
PlutusTx.unstableMakeIsData ''PolicyDatum

data MutualDatum = MutualDatum
    { mdPool   :: Value
    , mdShares :: [(PubKeyHash, Integer)]
    }
PlutusTx.unstableMakeIsData ''MutualDatum

data ClaimDatum = ClaimDatum
    { cdClaimant :: PubKeyHash
    , cdEvidence :: BuiltinByteString
    , cdStatus   :: BuiltinByteString -- "Pending", "Approved", "Rejected"
    }
PlutusTx.unstableMakeIsData ''ClaimDatum

data InsuranceAction = TriggerPayout | VoteClaim Bool
PlutusTx.unstableMakeIsData ''InsuranceAction

-- Helpers
{-# INLINABLE hasDeadlinePassed #-}
hasDeadlinePassed :: POSIXTime -> ScriptContext -> Bool
hasDeadlinePassed t ctx =
    let info = scriptContextTxInfo ctx
        range = txInfoValidRange info
    in Interval.contains (Interval.from (t + 1)) range

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE valuePaidToPubKey #-}
valuePaidToPubKey :: TxInfo -> PubKeyHash -> Value
valuePaidToPubKey info pkh = valuePaidTo info pkh

-- Validator
{-# INLINABLE mkInsuranceValidator #-}
mkInsuranceValidator :: PolicyDatum -> InsuranceAction -> ScriptContext -> Bool
mkInsuranceValidator dat action ctx =
    case action of
        TriggerPayout ->
            traceIfFalse "Deadline not reached" (hasDeadlinePassed (pdExpiry dat) ctx) &&
            traceIfFalse "Oracle trigger missing" oracleFired &&
            traceIfFalse "Owner not paid" ownerPaid
        VoteClaim approved ->
            traceIfFalse "Claim not signed by owner" (signedBy (pdOwner dat) ctx) &&
            traceIfFalse "Mutual pool insufficient" poolHasFunds &&
            traceIfFalse "Vote mismatch" (voteRespected approved)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Placeholder: in real contract, fetch oracle datum to verify trigger
    oracleFired :: Bool
    oracleFired = True

    ownerPaid :: Bool
    ownerPaid =
        let v = valuePaidToPubKey info (pdOwner dat)
        in valueOf v adaSymbol adaToken >= pdPayout dat

    poolHasFunds :: Bool
    poolHasFunds =
        case pdType dat of
            Mutual -> True  -- Replace with real pool value check
            _      -> True

    {-# INLINABLE voteRespected #-}
    voteRespected :: Bool -> Bool
    voteRespected approved =
        case pdType dat of
            Mutual -> approved
            _      -> True

-- Untyped Validator Boilerplate
{-# INLINABLE mkInsuranceValidatorUntyped #-}
mkInsuranceValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkInsuranceValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @PolicyDatum d
        red = unsafeFromBuiltinData @InsuranceAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkInsuranceValidator dat red ctx then () else error ()

insuranceValidator :: Validator
insuranceValidator = mkValidatorScript $$(PlutusTx.compile [|| mkInsuranceValidatorUntyped ||])

-- Hash & Address
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

insuranceScriptAddress :: Address
insuranceScriptAddress = Address (ScriptCredential (plutusValidatorHash insuranceValidator)) Nothing

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

-- Write validator to file
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

-- Main
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "insurance_validator.plutus" insuranceValidator
    let vh      = plutusValidatorHash insuranceValidator
        onchain = insuranceScriptAddress
        bech32  = toBech32ScriptAddress network insuranceValidator
    putStrLn "\n--- Insurance Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Insurance validator generated successfully."