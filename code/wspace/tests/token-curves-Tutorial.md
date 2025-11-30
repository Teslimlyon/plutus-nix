Perfect ✅ — here’s the **Markdown-style tutorial** for your new **`TokenValidator.hs`** smart contract, written exactly like your `Vesting.hs` tutorial (same tone, formatting, and structure).

Everything is aligned section by section: imports, data structures, validator logic, examples, and glossary.

---

# 🧾 Detailed Tutorial: Understanding and Using `TokenValidator.hs`

This tutorial covers the `TokenValidator.hs` module, which extends vesting contracts by introducing **token minting**, **burning**, **vesting claims**, and **compliance-based transfers**.
It highlights its purpose, important imports, validator logic, and on-chain/off-chain deployment procedures for Cardano smart contracts.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Script Compilation](#5-script-compilation)
6. [🏗️ Validator Hash and Address](#6-validator-hash-and-address)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api:**
  Provides essential on-chain types such as `Validator`, `ScriptContext`, `TxInfo`, and `POSIXTime`.

* **Plutus.V2.Ledger.Contexts:**
  Allows access to transaction information and signature verification using `txSignedBy`.

* **Plutus.V1.Ledger.Interval:**
  Used for time-related conditions such as vesting periods and token claim windows.

* **Plutus.V1.Ledger.Value:**
  Provides utilities to inspect and compare ADA or token quantities (`valueOf`, `adaSymbol`, `adaToken`).

### Utility and Serialization Modules

* **PlutusTx / PlutusTx.Prelude:**
  Compiles Haskell code into Plutus Core for on-chain logic.

* **Codec.Serialise / ByteString:**
  Handles serialization of validators into `.plutus` files for deployment.

* **Cardano.Api / Shelley:**
  Generates testnet/mainnet-compatible Bech32 addresses for smart contracts.

---

## 2. 🗃️ Data Structures

### `VestingDatum`

Defines the vesting schedule for a specific beneficiary.

* `vdBeneficiary` — Public key hash of the beneficiary authorized to claim tokens.
* `vdStart` — Vesting start time (`POSIXTime`).
* `vdCliff` — The earliest claimable time during vesting.
* `vdEnd` — End of vesting schedule.
* `vdTotal` — Total number of tokens or value allocated for vesting.

---

### `ComplianceDatum`

Holds regulatory or compliance-related data required for token transfer validation.

* `cdKYC` — A KYC identifier stored as a `BuiltinByteString`.
* `cdWhitelisted` — A list of public key hashes allowed to receive tokens (whitelisted).

---

### `TokenAction`

Represents all possible actions this smart contract can perform.

* `MintToken Integer` — Mint a specified quantity of tokens.
* `BurnToken Integer` — Burn a specified quantity of tokens.
* `ClaimVested` — Claim vested tokens during the vesting window.
* `TransferToken PubKeyHash` — Transfer tokens to a whitelisted recipient.

---

## 3. 🔧 Helper Functions

These utility functions simplify core logic checks and improve code readability.

* **`inVestingPeriod`** — Checks if the transaction time falls within the vesting window (`cliff` to `end`).
* **`signedBy`** — Validates if a transaction was signed by a given public key hash.
* **`isWhitelisted`** — Confirms that a target address is listed in the compliance whitelist.
* **`bondingPrice`** — Demonstrates a basic linear bonding curve: `1 token = 1000 lovelace`.

---

## 4. 🧠 Core Validator Logic

### `mkTokenValidator`

Defines the rules for each supported token operation: minting, burning, vesting claims, and transfers.
It ensures that every action follows correct authorization, timing, and compliance rules.

**Validation Rules:**

#### 🟢 MintToken

* Must be signed by the beneficiary.
* Mint amount must be greater than zero.

#### 🔴 BurnToken

* Must be signed by the beneficiary.
* Burn amount must be greater than zero.

#### 🟡 ClaimVested

* Transaction must occur within the vesting period.
* Must be signed by the beneficiary.

#### 🟣 TransferToken

* The receiver must be whitelisted in the `ComplianceDatum`.

```haskell
mkTokenValidator :: (VestingDatum, ComplianceDatum) -> TokenAction -> ScriptContext -> Bool
mkTokenValidator (vd, cd) action ctx =
    case action of
        MintToken amt ->
            traceIfFalse "must be signed by beneficiary" (signedBy (vdBeneficiary vd) ctx) &&
            traceIfFalse "amount must be positive" (amt > 0)
        BurnToken amt ->
            traceIfFalse "must be signed by beneficiary" (signedBy (vdBeneficiary vd) ctx) &&
            traceIfFalse "amount must be positive" (amt > 0)
        ClaimVested ->
            traceIfFalse "not in vesting period" (inVestingPeriod vd ctx) &&
            traceIfFalse "claim not signed by beneficiary" (signedBy (vdBeneficiary vd) ctx)
        TransferToken to ->
            traceIfFalse "recipient not whitelisted" (isWhitelisted to cd)
```

---

## 5. ⚙️ Script Compilation

### `mkTokenValidatorUntyped`

Converts the strongly-typed validator logic into an untyped format using `BuiltinData` for compatibility with Plutus Core.

### `validator`

Compiles the untyped validator into an executable Plutus script.

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkTokenValidatorUntyped ||])
```

This output can be serialized and deployed on the Cardano blockchain.

---

## 6. 🏗️ Validator Hash and Address

### `plutusValidatorHash`

Generates the on-chain hash uniquely identifying this validator.

### `plutusScriptAddress`

Derives the validator’s Plutus script address for receiving tokens or ADA.

### `toBech32ScriptAddress`

Converts the raw script address into a human-readable Bech32 address for use on testnet or mainnet.

---

## 7. 🧪 Practical Usage Example

Below is the `main` function demonstrating how to compile, write, and retrieve on-chain information about the validator:

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "token_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Token Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Token validator (curves, vesting, compliance) generated successfully."
```

**Output:**

* Validator hash
* Plutus on-chain address
* Bech32-readable address

---

## 8. ✅ Best Practices

* Always confirm the correct `POSIXTime` interval before allowing vesting claims.
* Keep whitelists and compliance data updated to prevent unauthorized transfers.
* Use `traceIfFalse` for descriptive debugging messages.
* Test minting, burning, and vesting boundaries extensively on **testnet**.
* Review KYC and whitelist management procedures before deploying on mainnet.
* Maintain strict signature validation to prevent unauthorized token issuance.

---

## 9. 📘 Glossary of Terms

| Term                | Definition                                                                          |
| ------------------- | ----------------------------------------------------------------------------------- |
| **VestingDatum**    | Defines the beneficiary’s vesting schedule and total token allocation.              |
| **ComplianceDatum** | Holds whitelisted addresses and compliance-related metadata (like KYC).             |
| **TokenAction**     | Redeemer type defining all possible contract actions (mint, burn, claim, transfer). |
| **Validator**       | On-chain logic that enforces contract rules.                                        |
| **POSIXTime**       | A time format used by Plutus for on-chain intervals.                                |
| **PubKeyHash**      | Unique hash identifier of a wallet’s public key.                                    |
| **txSignedBy**      | Function to verify transaction signatures.                                          |
| **Interval**        | Time range utility for checking valid vesting windows.                              |
| **Bech32**          | Human-readable format for Cardano addresses.                                        |
| **BuiltinData**     | Serialized data type for on-chain execution compatibility.                          |
| **Testnet**         | A public Cardano testing environment before mainnet deployment.                     |
