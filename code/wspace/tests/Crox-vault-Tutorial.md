

# 🏦 Detailed Tutorial: Understanding and Using `CroxVault.hs`

This tutorial explains the `CroxVault.hs` Plutus smart contract, designed for token vaulting and yield management. It introduces a secure, time-based lock mechanism and integrates an AI oracle-controlled yield update system.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Validator Compilation](#5-validator-compilation)
6. [🏗️ Validator Address Generation](#6-validator-address-generation)
7. [🗂️ File Writing](#7-file-writing)
8. [🧪 Practical Usage Example](#8-practical-usage-example)
9. [🧷 Testing Strategy](#9-testing-strategy)
10. [✅ Best Practices](#10-best-practices)
11. [📘 Glossary of Terms](#11-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api**
  Provides fundamental blockchain types such as `Validator`, `PubKeyHash`, `POSIXTime`, and `ScriptContext`.

* **Plutus.V2.Ledger.Contexts**
  Offers utility functions for validation logic like `txSignedBy`, `findOwnInput`, and `getContinuingOutputs`.

* **Plutus.V1.Ledger.Interval**
  Supplies interval operations like `contains` and `from` for time-based validation.

* **Plutus.V1.Ledger.Value**
  Exposes the `valueOf` function for token amount handling in UTXOs.

### Utility and Prelude Modules

* **PlutusTx / PlutusTx.Prelude**
  Enables on-chain compilation, data serialization, and provides Plutus-specific math and logic primitives.

* **Codec.Serialise**
  Used for serializing and writing the compiled validator to `.plutus` format.

* **Cardano.Api / Shelley**
  Provides functionality to generate Bech32 addresses for testnet and mainnet.

---

## 2. 🗃️ Data Structures

### `CroxVaultDatum`

Defines the state data stored on-chain for each locked vault entry:

* `cvUser`: The wallet owner authorized to deposit or withdraw.
* `cvTokenSymbol`: The token’s policy identifier.
* `cvTokenName`: The specific token name under the policy.
* `cvAmount`: The locked token amount.
* `cvLockStart`: Start time of the lock period.
* `cvLockPeriod`: Duration (in POSIX time) for which tokens remain locked.
* `cvYieldRate`: Yield percentage determined by an AI oracle.
* `cvAdmin`: Admin (AI oracle authority) permitted to update the yield rate.

### `CroxVaultAction`

Defines user and admin actions recognized by the validator:

* `Deposit`: A user locks tokens into the vault.
* `Withdraw`: A user withdraws funds after the lock period.
* `UpdateYield`: The admin updates the yield rate.

---

## 3. 🔧 Helper Functions

### `isAfterLock`

Checks if the current transaction occurs **after the vault lock period**:

```haskell
isAfterLock :: CroxVaultDatum -> POSIXTimeRange -> Bool
isAfterLock dat range =
    let unlockTime = cvLockStart dat + cvLockPeriod dat
    in Interval.contains (Interval.from unlockTime) range
```

This ensures withdrawals are only valid after the lock has matured.

---

## 4. 🧠 Core Validator Logic

### `mkValidator`

The core validation function that enforces the vault’s security and operational rules.

**Behavior by Action Type:**

* **Deposit**

  * Must be signed by the vault owner (`cvUser`).
  * The input amount must be at least the defined `cvAmount`.

* **Withdraw**

  * Must be signed by the vault owner (`cvUser`).
  * The lock period must have passed (`isAfterLock`).
  * The withdrawal must meet or exceed the expected return amount.

* **UpdateYield**

  * Must be signed by the admin (`cvAdmin`).

**Yield Computation:**

```haskell
expectedReturn = cvAmount dat + (cvAmount dat * cvYieldRate dat `divide` 100)
```

This logic calculates the new amount including yield. The rate may later be updated dynamically through an AI oracle.

---

## 5. ⚙️ Validator Compilation

### `mkValidatorUntyped`

Converts the typed validator (`CroxVaultDatum`, `CroxVaultAction`, `ScriptContext`) into the untyped `BuiltinData` format required for on-chain execution.

### `validator`

Compiles the validator into Plutus Core bytecode using Template Haskell:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

This output can be serialized and deployed to the Cardano blockchain.

---

## 6. 🏗️ Validator Address Generation

The script includes helper functions for computing and displaying on-chain addresses.

### `plutusValidatorHash`

Generates the validator hash from the compiled Plutus script.

### `plutusScriptAddress`

Creates a native on-chain `Address` representation from the validator hash.

### `toBech32ScriptAddress`

Converts the compiled script to a **Bech32-encoded address** for human readability and wallet integration:

```haskell
toBech32ScriptAddress network val =
    ...
    T.unpack (C.serialiseAddress shelleyAddr)
```

---

## 7. 🗂️ File Writing

The validator can be serialized and saved to a file using:

```haskell
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path
```

This creates the deployable `.plutus` file.

---

## 8. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "croxvault.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- CroxVault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "CroxVault validator generated successfully."
```

**Expected Output Example:**

```
--- CroxVault Validator Info ---
Validator Hash (Plutus): <hash>
Plutus Script Address:    <script address>
Bech32 Script Address:    addr_test1w...
---------------------------------
CroxVault validator generated successfully.
```

---

## 9. 🧷 Testing Strategy

* ✅ Validate deposits from authorized users only.
* ✅ Test withdrawals both before and after the lock period.
* ✅ Simulate yield updates by the admin to verify AI oracle authority.
* ✅ Confirm `expectedReturn` is computed correctly for different yield rates.
* ✅ Ensure the validator enforces single continuing output per vault state.

---

## 10. ✅ Best Practices

* Use **trace messages** for detailed transaction debugging.
* Test your contract across **preprod**, **preview**, and **mainnet** environments.
* Maintain **clear separation** between user and admin permissions.
* Always validate **POSIX time intervals** and **token amounts** carefully.
* Keep yield calculations **auditable** and **deterministic**.

---

## 11. 📘 Glossary of Terms

| Term               | Definition                                                                                  |
| ------------------ | ------------------------------------------------------------------------------------------- |
| **CroxVault**      | A token vault smart contract that enforces time-based locks and AI oracle yield management. |
| **Datum**          | On-chain state data defining ownership, lock time, and token info.                          |
| **Redeemer**       | Specifies the transaction action (Deposit, Withdraw, UpdateYield).                          |
| **Validator**      | The Plutus smart contract that approves or rejects transactions.                            |
| **POSIXTime**      | Timestamp format used in Plutus scripts for deadlines and durations.                        |
| **CurrencySymbol** | Token policy ID that identifies a unique asset on Cardano.                                  |
| **TokenName**      | The specific token name under a given policy.                                               |
| **txSignedBy**     | Checks whether a transaction was signed by a specific public key.                           |
| **AI Oracle**      | External authority that can update vault yield rates.                                       |
| **Bech32**         | Human-readable format for Cardano script addresses.                                         |
| **Testnet**        | A blockchain environment for testing smart contracts before mainnet deployment.             |

---

