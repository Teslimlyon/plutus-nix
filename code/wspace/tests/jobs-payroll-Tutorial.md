Absolutely! Here’s a **complete Markdown-style tutorial** for your **Jobs Smart Contract**, modeled exactly like your `Vesting.hs` tutorial, including Table of Contents, sections for datum types, validator logic, helpers, boilerplate, off-chain functions, practical usage, and a glossary.

---

# 🧾 Detailed Tutorial: Understanding and Using the `Jobs` Smart Contract

This tutorial explains the Jobs smart contract, which manages **salary streams**, **gig milestone payments**, and **referral bonuses** on Cardano. It highlights datum types, redeemers, validator logic, helper functions, and practical usage.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum Types](#2-datum-types)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Validator Logic](#4-validator-logic)
5. [⚙️ Boilerplate Compilation](#5-boilerplate-compilation)
6. [🏗️ Validator Hash & Address](#6-validator-hash--address)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [📘 Glossary of Terms](#8-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API & Ledger

* **Plutus.V2.Ledger.Api:** Core types like `ScriptContext`, `TxInfo`, `POSIXTime`, and `PubKeyHash`.
* **Plutus.V2.Ledger.Contexts:** Transaction helpers (`txSignedBy`).
* **Plutus.V1.Ledger.Interval:** Handles POSIXTime ranges for deadlines and streams.
* **Plutus.V1.Ledger.Value:** Token and ADA operations (`valueOf`, `adaSymbol`, `adaToken`).

### Utilities & Serialization

* **PlutusTx / PlutusTx.Prelude:** Script compilation and basic Plutus functions.
* **Codec.Serialise / ByteString:** Serialize scripts to `.plutus` files.
* **Cardano.Api / Shelley:** Generate Bech32 addresses and interact with Cardano networks.

---

## 2. 🗃️ Datum Types

### `StreamDatum`

Defines a continuous salary stream:

* `sEmployee`: Employee PubKeyHash.
* `sEmployer`: Employer PubKeyHash.
* `sRate`: Payment rate per POSIX interval.
* `sStart` / `sEnd`: Stream start and end POSIXTime.
* `sClaimed`: Amount already claimed.

### `GigDatum`

Tracks gig milestone payments:

* `gPayer`: Who pays for the gig.
* `gWorker`: Worker receiving payment.
* `gMilestones`: List of milestones with completion flags.
* `gArbiter`: Arbiter PubKeyHash for disputes.

### `ReferralDatum`

Handles referral bonuses:

* `rReferrer`: Referrer PubKeyHash.
* `rCandidate`: Candidate PubKeyHash.
* `rBonus`: Bonus in lovelace.
* `rCliff`: Earliest claim time.
* `rClaimed`: Whether bonus has been claimed.

### `JobAction`

Redeemer actions:

* `ClaimStream`: Claim salary stream.
* `PayMilestone Integer`: Pay a specific milestone.
* `ClaimReferral`: Claim referral bonus.

---

## 3. 🔧 Helper Functions

* `signedBy`: Checks if a transaction is signed by a given PubKeyHash.
* `getCurrentTime`: Extracts the current POSIX time from a transaction interval.
* `posixToInteger`: Converts `POSIXTime` to `Integer`.

---

## 4. 🧠 Validator Logic

The validator ensures payments are correct:

### ClaimStream

* Checks elapsed time since `sStart`.
* Calculates `totalOwed` based on elapsed time and rate.
* Verifies the employee is paid at least `totalOwed` and signed.

### PayMilestone

* Ensures milestone is unpaid.
* Validates payer signature and payment of milestone amount.

### ClaimReferral

* Checks cliff time and that the bonus is not already claimed.
* Validates referrer signature and payment of bonus.

```haskell
mkJobValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkJobValidator d r c = ...
```

---

## 5. ⚙️ Boilerplate Compilation

* `validator`: Compiles the typed validator into a Plutus script.
* `mkValidatorScript`: Produces on-chain script compatible with Plutus.

---

## 6. 🏗️ Validator Hash & Address

* `plutusValidatorHash`: Generates the validator hash.
* `plutusScriptAddress`: On-chain address derived from the validator hash.
* `toBech32ScriptAddress`: Converts validator to a human-readable Bech32 address.

---

## 7. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "jobs_validator.plutus" validator

    putStrLn "\n--- Jobs Smart Contract ---"
    putStrLn $ "Validator Hash: " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Script Address: " <> P.show plutusScriptAddress
    putStrLn $ "Bech32 Address: " <> toBech32ScriptAddress network validator
    putStrLn "----------------------------"
```

---

## 8. 📘 Glossary of Terms

| Term              | Definition                                                            |
| ----------------- | --------------------------------------------------------------------- |
| **StreamDatum**   | Represents a continuous salary stream for an employee.                |
| **GigDatum**      | Tracks milestone payments and dispute arbiter information.            |
| **ReferralDatum** | Handles referral bonuses with cliff time and claimed status.          |
| **JobAction**     | Redeemer action specifying type of claim or payment.                  |
| **POSIXTime**     | Time representation for deadlines and payment schedules.              |
| **PubKeyHash**    | Cryptographic hash of a wallet public key.                            |
| **Validator**     | On-chain Plutus script enforcing payment rules.                       |
| **Bech32**        | Human-readable Cardano address format.                                |
| **txSignedBy**    | Function to check if a transaction is signed by a given PubKeyHash.   |
| **valuePaidTo**   | Helper to calculate the amount of ADA or tokens paid to a PubKeyHash. |
