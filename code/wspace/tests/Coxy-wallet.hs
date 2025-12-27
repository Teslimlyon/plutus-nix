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

--------------------------------------------------------------------------------
-- Datum and Redeemer
--------------------------------------------------------------------------------

-- Datum supports multi-token payments
data EscrowDatum = EscrowDatum
    { edBuyer     :: PubKeyHash
    , edSeller    :: PubKeyHash
    , edAmountADA :: Integer
    , edDeadline  :: POSIXTime
    , edTokens    :: [(CurrencySymbol, TokenName, Integer)] -- token list
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction = PaySeller | RefundSeller
PlutusTx.unstableMakeIsData ''EscrowAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsTokens #-}
scriptInputContainsTokens :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)] -> Bool
scriptInputContainsTokens ctx tokens =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in all (\(cs, tn, amt) -> valueOf v cs tn >= amt) tokens

{-# INLINABLE tokensPaidTo #-}
tokensPaidTo :: TxInfo -> PubKeyHash -> [(CurrencySymbol, TokenName, Integer)] -> Bool
tokensPaidTo info pkh tokens =
    let v = valuePaidTo info pkh
    in all (\(cs, tn, amt) -> valueOf v cs tn >= amt) tokens

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
           traceIfFalse "script input missing tokens" (scriptInputContainsTokens ctx (edTokens dat)) &&
           traceIfFalse "buyer signature missing"   (txSignedBy info (edBuyer dat)) &&
           traceIfFalse "seller not paid ADA"       (valueOf (valuePaidTo info (edSeller dat)) adaSymbol adaToken >= edAmountADA dat) &&
           traceIfFalse "buyer did not receive tokens" (tokensPaidTo info (edBuyer dat) (edTokens dat))
      RefundSeller ->
           traceIfFalse "script input missing tokens" (scriptInputContainsTokens ctx (edTokens dat)) &&
           traceIfFalse "seller signature missing" (txSignedBy info (edSeller dat)) &&
           traceIfFalse "too early for refund"     afterDeadline &&
           traceIfFalse "seller did not receive tokens" (tokensPaidTo info (edSeller dat) (edTokens dat))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EscrowDatum d
        red = unsafeFromBuiltinData @EscrowAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Validator Hash + Addresses
--------------------------------------------------------------------------------

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
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

--------------------------------------------------------------------------------
-- File writing
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "coxy-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- COXY Wallet Escrow Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------------"
    putStrLn "COXY Escrow validator generated successfully."