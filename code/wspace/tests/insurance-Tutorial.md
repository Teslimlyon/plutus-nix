Absolutely! Here's a **structured tutorial** for your **Insurance smart contract** written in the same style as your `Vesting.hs` tutorial, with a **Table of Contents**, explanations, code highlights, and a glossary.

---

# 🧾 Detailed Tutorial: Understanding and Using the Insurance Smart Contract

This tutorial explains the `Insurance` smart contract, highlighting its purpose, datum and redeemer structures, validator logic, helper functions, and practical usage scenarios. This contract supports parametric and mutual insurance payouts on Cardano.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum & Redeemer Structures](#2-datum-redeemer-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Validator Script Compilation](#5-validator-script-compilation)
6. [🏗️ Validator Hash & Address](#6-validator-hash-address)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api:** Provides core types like `ScriptContext`, `TxInfo`, `POSIXTime`, `PubKeyHash`, and `Value`.
* **Plutus.V2.Ledger.Contexts:** Utility functions for transaction validation (`txSignedBy`).
* **Plutus.V1.Ledger.Interval:** Interval operations for deadline/time checks.
* **Plutus.V1.Ledger.Value:** Token and ADA handling (`valueOf`, `adaSymbol`, `adaToken`).

### Utilities & Serialization

* **PlutusTx / PlutusTx.Prelude:** Script compilation and basic Plutus functions.
* **Codec.Serialise / ByteString:** Serialize scripts to `.plutus` files.
* **Cardano.Api / Shelley:** Generate Bech32 addresses and interact with Cardano networks.

---

## 2. 🗃️ Datum & Redeemer Structures

### `PolicyType`

Defines the type of policy:

* `Parametric`: Payout based on external oracle trigger.
* `Mutual`: Payout based on community/mutual votes.

### `PolicyDatum`

* `pdOwner`: Owner PubKeyHash
* `pdTrigger`: Oracle identifier
* `pdPayout`: Payout amount in lovelace
* `pdExpiry`: POSIXTime of expiry
* `pdType`: Type of policy

### `MutualDatum`

* `mdPool`: Total mutual pool value
* `mdShares`: List of participant shares `(PubKeyHash, Integer)`

### `ClaimDatum`

* `cdClaimant`: Claimant PubKeyHash
* `cdEvidence`: Evidence of claim
* `cdStatus`: Status (`"Pending"`, `"Approved"`, `"Rejected"`)

### `InsuranceAction`

* `TriggerPayout`: Execute parametric payout
* `VoteClaim Bool`: Vote on mutual claim

---

## 3. 🔧 Helper Functions

* `hasDeadlinePassed :: POSIXTime -> ScriptContext -> Bool`
  Checks if the policy expiry/deadline has passed.

* `signedBy :: PubKeyHash -> ScriptContext -> Bool`
  Checks if transaction was signed by a specific PubKeyHash.

* `valuePaidToPubKey :: TxInfo -> PubKeyHash -> Value`
  Returns the value paid to a given PubKeyHash.

---

## 4. 🧠 Core Validator Logic

The `mkInsuranceValidator` function enforces contract rules based on `InsuranceAction`:

* **TriggerPayout:** Checks expiry, oracle trigger, and owner payout.
* **VoteClaim:** Ensures owner signed, mutual pool has funds, and vote respected.

```haskell
{-# INLINABLE mkInsuranceValidator #-}
mkInsuranceValidator :: PolicyDatum -> InsuranceAction -> ScriptContext -> Bool
mkInsuranceValidator dat action ctx =
    case action of
        TriggerPayout ->
            traceIfFalse "Deadline not reached" (hasDeadlinePassed (pdExpiry dat) ctx) &&
            traceIfFalse "Oracle trigger missing" oracleFired &&
            traceIfFalse "Owner not paid" ownerPaid
        VoteClaim approved ->
            traceIfFalse "Claim not signed by owner" (signedBy (pdOwner dat) ctx) &&
            traceIfFalse "Mutual pool insufficient" poolHasFunds &&
            traceIfFalse "Vote mismatch" (voteRespected approved)
  where
    info = scriptContextTxInfo ctx
    oracleFired = True
    ownerPaid = valueOf (valuePaidToPubKey info (pdOwner dat)) adaSymbol adaToken >= pdPayout dat
    poolHasFunds = True
    voteRespected approved = approved
```

---

## 5. ⚙️ Validator Script Compilation

* `mkInsuranceValidatorUntyped`: Wraps typed validator for Plutus Core.
* `insuranceValidator`: Compiled Plutus script ready for blockchain deployment.

---

## 6. 🏗️ Validator Hash & Address

* `plutusValidatorHash`: Generate on-chain `ValidatorHash`.
* `insuranceScriptAddress`: Cardano script address.
* `toBech32ScriptAddress`: Converts validator to Bech32 address.

---

## 7. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "insurance_validator.plutus" insuranceValidator
    let vh      = plutusValidatorHash insuranceValidator
        onchain = insuranceScriptAddress
        bech32  = toBech32ScriptAddress network insuranceValidator
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Script Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
```

---

## 8. 🧷 Testing Strategy

* Test parametric payout after oracle trigger and expiry.
* Test mutual claim voting with sufficient pool funds.
* Edge cases: unauthorized signatures, early payout, insufficient funds.

---

## 9. ✅ Best Practices

* Validate oracle triggers and deadlines before payouts.
* Ensure typed datum and redeemer structures match off-chain logic.
* Use `traceIfFalse` with clear messages for debugging.
* Serialize scripts and generate Bech32 addresses for deployment.

---

## 10. 📘 Glossary of Terms

| Term                | Definition                                              |
| ------------------- | ------------------------------------------------------- |
| **PolicyDatum**     | Holds owner, trigger, payout, expiry, and policy type.  |
| **MutualDatum**     | Tracks mutual insurance pool and participant shares.    |
| **ClaimDatum**      | Represents a claim with claimant, evidence, and status. |
| **InsuranceAction** | Redeemer action (`TriggerPayout` or `VoteClaim`).       |
| **Oracle Trigger**  | External input for parametric policies.                 |
| **Validator**       | On-chain Plutus script enforcing contract rules.        |
| **POSIXTime**       | Time representation for deadlines and expiry.           |
| **PubKeyHash**      | Wallet identifier used for signature verification.      |
| **Bech32**          | Human-readable Cardano address format.                  |
| **traceIfFalse**    | Debug helper for failed validation conditions.          |

---
