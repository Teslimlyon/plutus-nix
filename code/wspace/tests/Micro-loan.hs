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

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

-- Microloan states: Requested, Funded, Repaid, Defaulted
data LoanDatum = LoanDatum
    { ldLender     :: PubKeyHash
    , ldBorrower   :: PubKeyHash
    , ldAmount     :: Integer
    , ldDeadline   :: POSIXTime
    , ldInterest   :: Integer
    , ldReputation :: CurrencySymbol -- Reputation NFT
    , ldToken      :: TokenName      -- Reputation NFT token
    , ldFunded     :: Bool
    , ldRepaid     :: Bool
    }
PlutusTx.unstableMakeIsData ''LoanDatum

data LoanAction = FundLoan | RepayLoan | ClaimDefault
PlutusTx.unstableMakeIsData ''LoanAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE hasReputationNFT #-}
hasReputationNFT :: ScriptContext -> CurrencySymbol -> TokenName -> PubKeyHash -> Bool
hasReputationNFT ctx cs tn pkh =
    let v = mconcat [txOutValue o | o <- txInfoOutputs $ scriptContextTxInfo ctx, txOutAddress o == pubKeyHashAddress pkh]
    in valueOf v cs tn >= 1

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LoanDatum -> LoanAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      FundLoan ->
           traceIfFalse "loan already funded" (not $ ldFunded dat) &&
           traceIfFalse "lender signature missing" (txSignedBy info (ldLender dat)) &&
           traceIfFalse "borrower lacks reputation NFT" (hasReputationNFT ctx (ldReputation dat) (ldToken dat) (ldBorrower dat))
      RepayLoan ->
           traceIfFalse "loan not funded yet" (ldFunded dat) &&
           traceIfFalse "loan already repaid" (not $ ldRepaid dat) &&
           traceIfFalse "borrower signature missing" (txSignedBy info (ldBorrower dat)) &&
           traceIfFalse "lender not paid" lenderPaid
      ClaimDefault ->
           traceIfFalse "loan not funded" (ldFunded dat) &&
           traceIfFalse "loan already repaid" (not $ ldRepaid dat) &&
           traceIfFalse "too early to claim default" afterDeadline &&
           traceIfFalse "lender not paid" lenderPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (ldDeadline dat + 1)) txRange

    lenderPaid :: Bool
    lenderPaid =
      let v = valuePaidTo info (ldLender dat)
      in valueOf v adaSymbol adaToken >= (ldAmount dat + ldInterest dat)

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @LoanDatum d
        red = unsafeFromBuiltinData @LoanAction r
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
        shelleyAddr = C.makeShelleyAddressInEra
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

    writeValidator "microloan_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- MicroLoan Mesh Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "MicroLoan Mesh validator generated successfully."
