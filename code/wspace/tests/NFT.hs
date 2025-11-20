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

--------------------------------------------------------------------------------------
-- DATUM + REDEEMER
--------------------------------------------------------------------------------------

data TicketDatum = TicketDatum
    { tdBuyer      :: PubKeyHash
    , tdSeller     :: PubKeyHash
    , tdTier       :: Integer
    , tdPrice      :: Integer
    , tdCurrency   :: CurrencySymbol
    , tdToken      :: TokenName
    , tdRefundWin  :: POSIXTime
    , tdRefundCause:: Integer
    , tdSponsors   :: [PubKeyHash]
    , tdSplits     :: [Integer]
    }
PlutusTx.unstableMakeIsData ''TicketDatum

data TicketAction = BuyTicket | ScanAndSettle | RequestRefund
PlutusTx.unstableMakeIsData ''TicketAction

--------------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------------

{-# INLINABLE inputHasNFT #-}
inputHasNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
inputHasNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no script input"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

{-# INLINABLE sponsorsPaid #-}
sponsorsPaid :: TxInfo -> [PubKeyHash] -> [Integer] -> Integer -> Bool
sponsorsPaid info sponsors splits total =
    let zipped = zip sponsors splits
    in all (\(p,pct) ->
            let expected = (total * pct) `divide` 100
                v = valuePaidTo info p
            in valueOf v adaSymbol adaToken >= expected
        ) zipped

--------------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: TicketDatum -> TicketAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      BuyTicket ->
           traceIfFalse "NFT missing"       (inputHasNFT ctx cs tn) &&
           traceIfFalse "buyer sig missing" (txSignedBy info buyer) &&
           traceIfFalse "seller not paid"   (sellerPaid expectedPrice) &&
           traceIfFalse "buyer missing NFT" (paidNFT buyer)

      ScanAndSettle ->
           traceIfFalse "NFT missing"       (inputHasNFT ctx cs tn) &&
           traceIfFalse "seller sig missing" (txSignedBy info seller) &&
           traceIfFalse "sponsor payments invalid"
                (sponsorsPaid info (tdSponsors dat) (tdSplits dat) expectedPrice)

      RequestRefund ->
           traceIfFalse "NFT missing"        (inputHasNFT ctx cs tn) &&
           traceIfFalse "buyer sig missing"  (txSignedBy info buyer) &&
           traceIfFalse "refund window closed"
                withinRefundWindow &&
           traceIfFalse "refund not paid"    (buyerPaid expectedPrice)

  where
    info = scriptContextTxInfo ctx
    cs  = tdCurrency dat
    tn  = tdToken dat
    buyer = tdBuyer dat
    seller = tdSeller dat
    expectedPrice = tdPrice dat

    txRange = txInfoValidRange info

    withinRefundWindow =
        Interval.contains (Interval.to (tdRefundWin dat)) txRange

    sellerPaid amt =
        let v = valuePaidTo info seller
        in valueOf v adaSymbol adaToken >= amt

    buyerPaid amt =
        let v = valuePaidTo info buyer
        in valueOf v adaSymbol adaToken >= amt

    paidNFT pkh =
        let v = valuePaidTo info pkh
        in valueOf v cs tn >= 1

--------------------------------------------------------------------------------------
-- UNTYPED
--------------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @TicketDatum d
        red = unsafeFromBuiltinData @TicketAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------------
-- HASH + ADDRESS
--------------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes    = Serialise.serialise v
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
        plutusScript =
            CS.PlutusScriptSerialised serialised :: C.PlutusScript C.PlutusScriptV2
        scriptHash =
            C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        addr :: C.AddressInEra C.BabbageEra
        addr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress

    in T.unpack (C.serialiseAddress addr)


--------------------------------------------------------------------------------------
-- WRITE + MAIN
--------------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "event_ticket.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Event Ticket Validator ---"
    putStrLn $ "Validator Hash:    " <> P.show vh
    putStrLn $ "Plutus Address:    " <> P.show onchain
    putStrLn $ "Bech32 Address:    " <> bech32
    putStrLn "--------------------------------"
