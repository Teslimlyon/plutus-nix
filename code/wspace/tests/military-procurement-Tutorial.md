Perfect — below is your **Military Procurement Smart Contract tutorial**, written in the **same structure, tone, and formatting style** as your `Vesting.hs` tutorial (the Markdown version you provided).

Everything — from the Table of Contents to the Glossary — matches your layout, but the content now corresponds to your **Military Procurement validator** code.

---

# 🧾 Detailed Tutorial: Understanding and Using the `Military Procurement` Smart Contract

This tutorial explains the `Military Procurement` smart contract, which secures the transfer, verification, and access control of military parts on the Cardano blockchain using NFTs and Soulbound Tokens (SBTs).
It highlights the module’s structure, data types, validator logic, helper utilities, and deployment process.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
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
  Provides core blockchain types such as `ScriptContext`, `TxInfo`, `PubKeyHash`, and `POSIXTime`.

* **Plutus.V2.Ledger.Contexts:**
  Offers functions for transaction context validation, including `txSignedBy`.

* **Plutus.V1.Ledger.Interval:**
  Used to verify transaction validity intervals (POSIX time checks).

* **Plutus.V1.Ledger.Value:**
  Includes utilities for token handling (`valueOf`, `adaSymbol`, `adaToken`).

### Utility and Serialization Modules

* **PlutusTx & PlutusTx.Prelude:**
  Enable compiling Haskell code into on-chain Plutus Core.

* **Codec.Serialise, ByteString:**
  Handle serialization of Plutus scripts into `.plutus` files.

* **Cardano.Api & Cardano.Api.Shelley:**
  Used to create Bech32 script addresses and manage Cardano network configurations.

---

## 2. 🗃️ Data Structures

### `PartNFT`

Represents a serialized and traceable on-chain record for each military part.

* `partNSN`: The NATO Stock Number identifier.
* `partLot`: Manufacturing or batch lot number.
* `partCompliance`: Certification or compliance hash string.
* `partOwner`: The current owner’s public key hash.

---

### `RoleSBT`

Defines an individual's assigned access role and expiration time.

* `sbtUnit`: The military unit or department of the holder.
* `sbtRole`: Role assigned (e.g., `"OFFICER"`, `"INSPECTOR"`).
* `sbtExpiry`: POSIX timestamp for access expiration.
* `sbtOwner`: PubKeyHash of the SBT holder.

---

### `MilitaryAction`

Specifies the type of operation to be executed by the validator.

* `TransferPart`: Authorizes the owner to transfer a part to another address.
* `CheckAccess`: Validates the SBT credentials of the actor for viewing or verification rights.

---

## 3. 🔧 Helper Functions

### `signedBy`

Checks if a transaction is signed by a specified `PubKeyHash`.

### `getCurrentTime`

Extracts the transaction’s current `POSIXTime` from its validity range.

### `hasValidSBT`

Ensures that an SBT output:

* Belongs to the signer.
* Matches the required `role`.
* Has not expired at the current time.

### `pubKeyHashFromTxInfo`

Retrieves the first signing key from the transaction’s signature list.

---

## 4. 🧠 Core Validator Logic

### `mkMilitaryValidator`

**Purpose:**
Defines on-chain rules that control:

1. Authorized part ownership transfers.
2. Role-based access permissions through SBTs.

**Action Conditions:**

* **TransferPart**

  * Validates that the transaction is signed by the current owner of the `PartNFT`.
  * Ensures the part datum is correctly structured.

* **CheckAccess**

  * Scans outputs for a valid `RoleSBT`.
  * Confirms the signatory’s role equals `"OFFICER"` and is still within its expiry period.

**Validator Type:**

```haskell
mkMilitaryValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
```

**Error Handling:**
If validation fails, the transaction triggers a `traceError "Validator failed"` message.

---

## 5. ⚙️ Validator Compilation

### `mkValidatorUntyped`

Wraps the validator function into the untyped format required by Plutus scripts.

### `validator`

Compiles the `mkValidatorUntyped` into a deployable on-chain script:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

---

## 6. 🏗️ Validator Hash & Address

### `plutusValidatorHash`

Generates the `ValidatorHash` by serializing the script into a Plutus-compatible binary form.

### `plutusScriptAddress`

Creates the script address for on-chain reference and fund locking.

### `toBech32ScriptAddress`

Converts the script address into a Bech32 human-readable format for easier testnet or mainnet deployment.

---

## 7. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "military_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Military Procurement Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "Military procurement validator generated successfully."
```

---

## 8. ✅ Best Practices

* Always validate the role and expiry of `RoleSBT` before approving any part transfer.
* Ensure that every `PartNFT` includes immutable compliance metadata.
* Use detailed trace messages for easy on-chain debugging.
* Test multiple scenarios — including expired SBTs and unauthorized transfers — before deployment.

---

## 9. 📘 Glossary of Terms

| Term               | Definition                                                    |
| ------------------ | ------------------------------------------------------------- |
| **PartNFT**        | Tokenized record of a physical or digital military component. |
| **RoleSBT**        | Soulbound token defining a holder’s role and access rights.   |
| **MilitaryAction** | Redeemer specifying the intended on-chain operation.          |
| **TransferPart**   | Action for transferring ownership of a part.                  |
| **CheckAccess**    | Action validating SBT authorization for access.               |
| **Validator**      | Plutus smart contract that enforces transaction rules.        |
| **POSIXTime**      | Blockchain time representation for scheduling and expiry.     |
| **PubKeyHash**     | Cryptographic hash representing a wallet’s public key.        |
| **traceError**     | Debugging output that halts script execution on failure.      |
| **Bech32**         | Human-readable Cardano address format.                        |
| **Plutus Script**  | Compiled executable code of a smart contract.                 |
| **Testnet**        | Simulated blockchain environment for testing.                 |

---