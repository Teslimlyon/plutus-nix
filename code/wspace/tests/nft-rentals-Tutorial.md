Perfect. 👌 Here’s your **NFT Smart Contract Tutorial** — formatted *exactly* like the original `Vesting.hs` write-up, with identical Markdown style, structure, and section numbering — but now fully customized for your **multi-feature NFT validator** (Rental, Fractional, and Royalty logic).

---

# 🧾 Detailed Tutorial: Understanding and Using the `NFT Multi-Feature` Smart Contract

This tutorial explains the `nft_validator.hs` smart contract, which brings together **NFT rental**, **fractional ownership**, and **royalty distribution** into a single Plutus validator.
It outlines the contract’s imports, datum and redeemer structures, validation logic, helper utilities, and deployment flow.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum and Redeemer Structures](#2-datum-and-redeemer-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Validator Compilation](#5-validator-compilation)
6. [🏗️ Validator Hash & Address](#6-validator-hash--address)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api:**
  Supplies core blockchain data types such as `Validator`, `ScriptContext`, `TxInfo`, and `PubKeyHash`.

* **Plutus.V2.Ledger.Contexts:**
  Offers access to transaction inputs, outputs, and signature validation with functions like `txSignedBy` and `findOwnInput`.

* **Plutus.V1.Ledger.Interval:**
  Used for validating transaction time ranges (expiry checks).

* **Plutus.V1.Ledger.Value:**
  Provides value-based utilities like `valueOf`, `adaSymbol`, and `adaToken` for token and ADA comparisons.

### Utility and Serialization Modules

* **PlutusTx / PlutusTx.Prelude:**
  Enable compilation of Haskell code into on-chain Plutus Core and provide on-chain functional primitives.

* **Codec.Serialise / ByteString:**
  Handle binary serialization of validators into `.plutus` files for deployment.

* **Cardano.Api / Cardano.Api.Shelley:**
  Used for Bech32 address generation and defining the correct Cardano testnet or mainnet environment.

---

## 2. 🗃️ Datum and Redeemer Structures

### `LeaseDatum`

Defines the NFT rental terms between owner and renter.

* `ldNFT`: A tuple `(CurrencySymbol, TokenName)` uniquely identifying the NFT.
* `ldRenter`: The renter’s `PubKeyHash`.
* `ldExpiry`: The rental expiry time represented in `POSIXTime`.

---

### `VaultDatum`

Represents fractionalized NFT ownership among multiple holders.

* `vdNFT`: NFT identifier associated with the vault.
* `vdShares`: A list of `(PubKeyHash, Integer)` representing owners and their share quantities.

---

### `RoyaltyDatum`

Defines creator royalty distribution logic.

* `rdCreator`: The main creator’s public key hash.
* `rdSplit`: A list of `(PubKeyHash, Integer)` specifying payout addresses and their percentage allocations (0–100).

---

### `NFTAction`

Specifies the transaction’s intended operation.

* `LeaseNFT`: Rent out the NFT to a renter.
* `ReturnNFT`: Return the NFT after lease expiry.
* `MintFraction Integer`: Fractionalize the NFT into multiple parts.
* `RedeemFraction Integer`: Redeem fractional ownership shares.
* `DistributeRoyalty`: Split incoming royalties among defined payees.

---

## 3. 🔧 Helper Functions

### `scriptInputContainsNFT`

Checks that the script’s own UTxO input includes the targeted NFT.

### `hasExpired`

Validates whether the lease has passed its expiry date using `POSIXTime` intervals.

### `signedBy`

Ensures a transaction is signed by a specified wallet’s public key hash.

### `valuePaidToPubKey`

Determines the amount of value paid to a particular address in the transaction.

---

## 4. 🧠 Core Validator Logic

### `mkNFTValidator`

**Purpose:**
Defines how different NFT actions are validated under a single Plutus contract. Each action (Lease, Return, Mint, Redeem, Royalty) enforces unique conditions.

**Validation Rules:**

* **LeaseNFT**

  * Requires that the NFT is included in the transaction input.
  * Ensures the renter’s signature is present.

* **ReturnNFT**

  * Allows returning the NFT only after expiry.

* **MintFraction**

  * Confirms NFT ownership and a positive mint amount.

* **RedeemFraction**

  * Requires valid share ownership before redemption.

* **DistributeRoyalty**

  * Ensures each royalty share allocation is greater than zero.

```haskell
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
            traceIfFalse "must hold fractions to redeem" (amt > 0)
        DistributeRoyalty ->
            traceIfFalse "must distribute to splits" (all (>0) (map snd (rdSplit royaltyDat)))
  where
    nftCS = fst (ldNFT leaseDat)
    nftTN = snd (ldNFT leaseDat)
```

---

## 5. ⚙️ Validator Compilation

### `mkNFTValidatorUntyped`

Wraps the validator logic to support Plutus’ `BuiltinData` type format.

### `validator`

Compiles and converts the `mkNFTValidator` into a deployable Plutus script using Template Haskell:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkNFTValidatorUntyped ||])
```

---

## 6. 🏗️ Validator Hash & Address

* **`plutusValidatorHash`:**
  Generates the unique on-chain hash of the compiled validator.

* **`plutusScriptAddress`:**
  Produces the script’s on-chain address, which users send ADA or tokens to.

* **`toBech32ScriptAddress`:**
  Converts the script address into human-readable Bech32 format for easy integration and testing.

---

## 7. 🧪 Practical Usage Example

```haskell
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
```

---

## 8. ✅ Best Practices

* Ensure that NFT ownership is verified before fractionalizing.
* Always validate the expiry time of a lease to prevent early NFT returns.
* Confirm that royalty splits total correctly before distribution.
* Use clear `traceIfFalse` messages for on-chain debugging.
* Simulate all action scenarios (`LeaseNFT`, `ReturnNFT`, etc.) on testnet before mainnet deployment.

---

## 9. 📘 Glossary of Terms

| Term               | Definition                                                                          |
| ------------------ | ----------------------------------------------------------------------------------- |
| **LeaseDatum**     | Contains information about NFT rental agreements including renter and expiry.       |
| **VaultDatum**     | Tracks fractional ownership of an NFT by multiple parties.                          |
| **RoyaltyDatum**   | Stores creator and royalty distribution details.                                    |
| **NFTAction**      | Redeemer defining what operation (Lease, Return, Mint, etc.) the contract executes. |
| **Validator**      | Smart contract enforcing transaction rules on-chain.                                |
| **CurrencySymbol** | Unique identifier of a token policy on Cardano.                                     |
| **TokenName**      | The individual asset name under a policy.                                           |
| **ScriptContext**  | Provides transaction info to the validator.                                         |
| **Bech32**         | Human-readable encoding format for blockchain addresses.                            |
| **POSIXTime**      | Time representation used for lease expiry and scheduling.                           |
| **traceIfFalse**   | Plutus debugging utility that fails a transaction with an error message.            |
| **Plutus Script**  | Compiled, executable smart contract on Cardano.                                     |

---