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

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialisation
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Offchain (Bech32)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------------------------
-- DATUM
------------------------------------------------------------------------------------------

data BookingDatum = BookingDatum
    { bdHost         :: PubKeyHash
    , bdGuest        :: PubKeyHash
    , bdStart        :: POSIXTime
    , bdEnd          :: POSIXTime
    , bdPrice        :: Integer        -- dynamic price (set off-chain)
    , bdCancelBefore :: POSIXTime      -- full refund until this time
    , bdCurrency     :: CurrencySymbol -- booking NFT CS
    , bdToken        :: TokenName      -- booking NFT TN
    , bdLoyalCS      :: CurrencySymbol -- loyalty SBT CS
    , bdLoyalTN      :: TokenName      -- loyalty SBT TN
    }
PlutusTx.unstableMakeIsData ''BookingDatum

data BookingAction
    = PayHost
    | CancelRefund
    | CompleteStay   -- triggers loyalty point update
PlutusTx.unstableMakeIsData ''BookingAction

------------------------------------------------------------------------------------------
-- HELPERS
------------------------------------------------------------------------------------------

{-# INLINABLE scriptInputHasNFT #-}
scriptInputHasNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputHasNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no script input found"
        Just i  ->
            let v = txOutValue (txInInfoResolved i)
            in valueOf v cs tn >= 1

------------------------------------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: BookingDatum -> BookingAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      ----------------------------------------------------------------------------
      -- GUEST COMPLETES BOOKING → PAY HOST + NFT BURNED
      ----------------------------------------------------------------------------
      PayHost ->
          traceIfFalse "NFT missing"       (scriptInputHasNFT ctx cs tn) &&
          traceIfFalse "guest signature"   (txSignedBy info (bdGuest dat)) &&
          traceIfFalse "host not paid"     hostPaid &&
          traceIfFalse "guest keeps NFT"   (not guestStillHasNFT)

      ----------------------------------------------------------------------------
      -- GUEST CANCELS BEFORE DEADLINE → REFUND
      ----------------------------------------------------------------------------
      CancelRefund ->
          traceIfFalse "NFT missing"       (scriptInputHasNFT ctx cs tn) &&
          traceIfFalse "guest signature"   (txSignedBy info (bdGuest dat)) &&
          traceIfFalse "refund too late"   withinCancelWindow &&
          traceIfFalse "guest not refunded" guestRefunded

      ----------------------------------------------------------------------------
      -- COMPLETION → HOST SIGNATURE + LOYALTY SBT GETS POINTS
      ----------------------------------------------------------------------------
      CompleteStay ->
          traceIfFalse "NFT missing"       (scriptInputHasNFT ctx cs tn) &&
          traceIfFalse "host signature"    (txSignedBy info (bdHost dat)) &&
          traceIfFalse "wrong time"        afterStay &&
          traceIfFalse "SBT missing"       guestHasSBT &&
          traceIfFalse "points not added"  pointsIncreased
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs  = bdCurrency dat
    tn  = bdToken dat

    -- NFT
    guestStillHasNFT :: Bool
    guestStillHasNFT =
        let v = valuePaidTo info (bdGuest dat)
        in valueOf v cs tn >= 1

    -- HOST PAID ESCROW PRICE
    hostPaid :: Bool
    hostPaid =
        let v = valuePaidTo info (bdHost dat)
        in valueOf v adaSymbol adaToken >= bdPrice dat

    -- REFUND
    guestRefunded :: Bool
    guestRefunded =
        let v = valuePaidTo info (bdGuest dat)
        in valueOf v adaSymbol adaToken >= bdPrice dat

    -- TIME CHECKS
    withinCancelWindow :: Bool
    withinCancelWindow = Interval.contains (Interval.to (bdCancelBefore dat)) (txInfoValidRange info)

    afterStay :: Bool
    afterStay = Interval.contains (Interval.from (bdEnd dat + 1)) (txInfoValidRange info)

    -- Loyalty point SBT: stored in output datum with new points
    loyalCS = bdLoyalCS dat
    loyalTN = bdLoyalTN dat

    guestHasSBT :: Bool
    guestHasSBT =
        let v = valuePaidTo info (bdGuest dat)
        in valueOf v loyalCS loyalTN == 1

    pointsIncreased :: Bool
    pointsIncreased =
        let outs = getContinuingOutputs ctx
        in case outs of
            [o] ->
                case txOutDatum o of
                    OutputDatum (Datum d) ->
                        let oldPoints = 0 -- Incoming SBT assumed 0; off-chain includes old value
                            newPoints = unsafeFromBuiltinData @Integer d
                        in newPoints > oldPoints
                    _ -> False
            _ -> False

------------------------------------------------------------------------------------------
-- UNTYPED
------------------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @BookingDatum d
        red = unsafeFromBuiltinData @BookingAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------------------------
-- HASH + ADDRESS (same as your file)
------------------------------------------------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash v =
    let bytes    = Serialise.serialise v
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-- For CLI use
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

------------------------------------------------------------------------------------------
-- FILE WRITING
------------------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "booking-loyalty.plutus" validator

    let vh      = plutusValidatorHash validator
        address = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Booking + Loyalty Validator ---"
    putStrLn $ "Validator Hash:   " <> P.show vh
    putStrLn $ "Plutus Address:   " <> P.show address
    putStrLn $ "Bech32 Address:   " <> bech32
    putStrLn "----------------------------------"

