{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where
import qualified Data.Text.Encoding as TE
import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Data.ByteString.Base16 as Base16
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------
-- Datum & Actions
------------------------------------------------------
data Level = Admin | Developer | Facilitator | Marketing
PlutusTx.unstableMakeIsData ''Level

data StreamDatum = StreamDatum
  { sdEmployee   :: PubKeyHash
  , sdLevel      :: Level
  , sdStart      :: POSIXTime
  , sdEnd        :: POSIXTime
  , sdClaimed    :: Integer
  , sdResumption :: POSIXTime
  }
PlutusTx.unstableMakeIsData ''StreamDatum

data GigDatum = GigDatum
  { gdEmployee :: PubKeyHash
  , gdMilestone :: Integer
  , gdArbiter :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''GigDatum

data ReferralDatum = ReferralDatum
  { rdCandidate :: PubKeyHash
  , rdBonus :: Integer
  , rdStart :: POSIXTime
  , rdCliff :: POSIXTime
  }
PlutusTx.unstableMakeIsData ''ReferralDatum

data PayrollAction = ClaimMonthly | ClaimGig | ClaimReferral
PlutusTx.unstableMakeIsData ''PayrollAction

------------------------------------------------------
-- Helpers
------------------------------------------------------
{-# INLINABLE levelAmount #-}
levelAmount :: Level -> Integer
levelAmount lvl = case lvl of
    Admin       -> 1000 * 1000000
    Developer   -> 800  * 1000000
    Facilitator -> 600  * 1000000
    Marketing   -> 500  * 1000000

{-# INLINABLE retirementDeduction #-}
retirementDeduction :: Integer -> Integer
retirementDeduction amt = (amt * 80) `divide` 100

{-# INLINABLE scriptInputContainsAda #-}
scriptInputContainsAda :: ScriptContext -> Integer -> Bool
scriptInputContainsAda ctx amt =
  case findOwnInput ctx of
    Nothing -> traceError "No script input"
    Just i  -> let v = txOutValue $ txInInfoResolved i
               in valueOf v adaSymbol adaToken >= amt

------------------------------------------------------
-- Validator
------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator d r c =
  let action = unsafeFromBuiltinData @PayrollAction r
      ctx = unsafeFromBuiltinData @ScriptContext c
      info = scriptContextTxInfo ctx
  in case action of
      ClaimMonthly ->
        let sd = unsafeFromBuiltinData @StreamDatum d
            gross = levelAmount (sdLevel sd)
            net   = retirementDeduction gross
        in if txSignedBy info (sdEmployee sd) &&
              scriptInputContainsAda ctx net
           then () else traceError "Monthly claim failed"

      ClaimGig ->
        let gd = unsafeFromBuiltinData @GigDatum d
        in if txSignedBy info (gdEmployee gd)
              && txSignedBy info (gdArbiter gd)
              && scriptInputContainsAda ctx (gdMilestone gd)
           then () else traceError "Gig claim failed"

      ClaimReferral ->
        let rd = unsafeFromBuiltinData @ReferralDatum d
            now = case txInfoValidRange info of
                    Interval.Interval (LowerBound (Finite t) _) _ -> t
                    _ -> rdStart rd
        in if txSignedBy info (rdCandidate rd)
              && now >= rdCliff rd
              && scriptInputContainsAda ctx (rdBonus rd)
           then () else traceError "Referral claim failed"

------------------------------------------------------
-- Boilerplate
------------------------------------------------------
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
  let bytes = Serialise.serialise val
      short = SBS.toShort (LBS.toStrict bytes)
      strictBS = SBS.fromShort short
      builtin = Builtins.toBuiltin strictBS
  in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential $ plutusValidatorHash validator) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
  let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
      plutusScript :: C.PlutusScript C.PlutusScriptV2
      plutusScript = CS.PlutusScriptSerialised serialised
      scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
      shelleyAddr :: C.AddressInEra C.BabbageEra
      shelleyAddr = C.makeShelleyAddressInEra
        network
        (C.PaymentCredentialByScript scriptHash)
        C.NoStakeAddress
  in T.unpack (C.serialiseAddress shelleyAddr)

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
  LBS.writeFile path (Serialise.serialise val)
  putStrLn $ "Validator written to: " <> path

main :: IO ()
main = do
  let network = C.Testnet (C.NetworkMagic 1)
      serialised = LBS.toStrict $ Serialise.serialise validator
      cborHex = Base16.encode serialised

  -- Write full .plutus file
  writeValidator "coxygen_payroll.plutus" validator

  -- Also write hex file
  LBS.writeFile "coxygen_payroll.cborhex" (LBS.fromStrict cborHex)

  putStrLn "----------------------------------"
  putStrLn "  Payroll validator generated!"
  putStrLn "----------------------------------"
  putStrLn "Bech32 Script Address:"
  putStrLn $ toBech32ScriptAddress network validator
  putStrLn "----------------------------------"
  putStrLn "CBOR Hex:"
  putStrLn (T.unpack $ TE.decodeUtf8 cborHex)
  putStrLn "----------------------------------"
