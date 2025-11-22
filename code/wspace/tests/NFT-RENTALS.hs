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

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

-- Rental lease datum
data LeaseDatum = LeaseDatum
    { ldNFT      :: (CurrencySymbol, TokenName)
    , ldRenter   :: PubKeyHash
    , ldExpiry   :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''LeaseDatum

-- Fractional vault datum
data VaultDatum = VaultDatum
    { vdNFT      :: (CurrencySymbol, TokenName)
    , vdShares   :: [(PubKeyHash, Integer)]  -- Shareholders
    }
PlutusTx.unstableMakeIsData ''VaultDatum

-- Royalty datum
data RoyaltyDatum = RoyaltyDatum
    { rdCreator  :: PubKeyHash
    , rdSplit    :: [(PubKeyHash, Integer)] -- Split percentages in 0..100
    }
PlutusTx.unstableMakeIsData ''RoyaltyDatum

-- Redeemer actions
data NFTAction = LeaseNFT | ReturnNFT | MintFraction Integer | RedeemFraction Integer | DistributeRoyalty
PlutusTx.unstableMakeIsData ''NFTAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsNFT #-}
scriptInputContainsNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

{-# INLINABLE hasExpired #-}
hasExpired :: POSIXTime -> ScriptContext -> Bool
hasExpired expiry ctx =
    let range = txInfoValidRange $ scriptContextTxInfo ctx
    in Interval.contains (Interval.from (expiry + 1)) range

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE valuePaidToPubKey #-}
valuePaidToPubKey :: TxInfo -> PubKeyHash -> Value
valuePaidToPubKey info pkh = valuePaidTo info pkh

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkNFTValidator #-}
mkNFTValidator :: (LeaseDatum, VaultDatum, RoyaltyDatum) -> NFTAction -> ScriptContext -> Bool
mkNFTValidator (leaseDat, vaultDat, royaltyDat) action ctx =
    case action of
        LeaseNFT ->
            traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx nftCS nftTN) &&
            traceIfFalse "renter signature missing" (signedBy (ldRenter leaseDat) ctx)
        ReturnNFT ->
            traceIfFalse "too early to return" (hasExpired (ldExpiry leaseDat) ctx)
        MintFraction amt ->
            traceIfFalse "must own NFT to mint fractions" (scriptInputContainsNFT ctx nftCS nftTN) &&
            traceIfFalse "amount must be positive" (amt > 0)
        RedeemFraction amt ->
            traceIfFalse "must hold fractions to redeem" (amt > 0) -- simplified
        DistributeRoyalty ->
            traceIfFalse "must distribute to splits" (all (>0) (map snd (rdSplit royaltyDat)))
  where
    nftCS = fst (ldNFT leaseDat)
    nftTN = snd (ldNFT leaseDat)

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkNFTValidatorUntyped #-}
mkNFTValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkNFTValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @(LeaseDatum, VaultDatum, RoyaltyDatum) d
        red = unsafeFromBuiltinData @NFTAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkNFTValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkNFTValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes = Serialise.serialise val
        short = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin = Builtins.toBuiltin strictBS
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
    writeValidator "nft_validator.plutus" validator
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
    putStrLn "\n--- NFT Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "NFT validator (rental, fractional, royalties) generated successfully."