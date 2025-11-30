Here’s a **fully structured tutorial** for your **Booking + Loyalty smart contract**, written in the same style as your `Vesting.hs` tutorial, with Table of Contents, detailed sections, and a glossary. It mirrors your original formatting but adapted fully for your contract.

---

# 🧾 Detailed Tutorial: Booking + Loyalty Smart Contract

This tutorial explains the `Booking + Loyalty` smart contract, highlighting datum & redeemer structures, validator logic, helper functions, addresses, compilation, and practical usage. It is designed for creating secure booking escrow contracts with loyalty point updates on Cardano.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum & Redeemer Structures](#2-datum-redeemer-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Script Compilation & Untyped Validator](#5-script-compilation)
6. [🏗️ Validator Hash & Address](#6-validator-address)
7. [🧪 Practical Usage Example](#7-practical-usage)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api & Contexts:** Provides `ScriptContext`, `TxInfo`, `POSIXTime`, `PubKeyHash`, `CurrencySymbol`, `TokenName`, and transaction validation functions.
* **Plutus.V1.Ledger.Interval:** Time interval checks (`contains`, `to`, `from`).
* **Plutus.V1.Ledger.Value:** ADA and token handling (`valueOf`, `adaSymbol`, `adaToken`).

### Utilities & Serialization

* **PlutusTx / PlutusTx.Prelude:** Compile Haskell to Plutus Core and basic scripting functions.
* **Codec.Serialise / Data.ByteString:** Serialize validator scripts to `.plutus` format.
* **Cardano.Api / Shelley:** Generate Bech32 addresses and interact with Cardano networks.

---

## 2. 🗃️ Datum & Redeemer Structures

### `BookingDatum`

Holds all critical booking data:

* `bdHost`: Host wallet PubKeyHash.
* `bdGuest`: Guest wallet PubKeyHash.
* `bdStart` / `bdEnd`: POSIXTime of stay.
* `bdPrice`: Price in ADA.
* `bdCancelBefore`: Deadline for full refund.
* `bdCurrency` / `bdToken`: Booking NFT currency and token.
* `bdLoyalCS` / `bdLoyalTN`: Loyalty SBT currency and token.

### `BookingAction` (Redeemer)

| Action       | Description                                    |
| ------------ | ---------------------------------------------- |
| PayHost      | Guest completes booking → pay host + burn NFT  |
| CancelRefund | Guest cancels before deadline → refund         |
| CompleteStay | Host marks completion → loyalty points updated |

---

## 3. 🔧 Helper Functions

* **`scriptInputHasNFT`:** Checks that the script input contains the booking NFT.

---

## 4. 🧠 Core Validator Logic

**`mkValidator`** enforces rules based on `BookingAction`:

* **PayHost:** Guest signature, NFT present, host paid, NFT burned.
* **CancelRefund:** Guest signature, NFT present, within cancel window, guest refunded.
* **CompleteStay:** Host signature, NFT present, after stay, guest has loyalty SBT, points increased.

Additional checks:

* Time intervals: `withinCancelWindow`, `afterStay`.
* Loyalty SBT validation: `guestHasSBT`, `pointsIncreased`.

---

## 5. ⚙️ Script Compilation & Untyped Validator

* **`mkValidatorUntyped`:** Wraps typed validator for Plutus Core.
* **`validator`:** Compiled Plutus Core script ready for deployment.

---

## 6. 🏗️ Validator Hash & Address

* **`plutusValidatorHash`:** Generates on-chain `ValidatorHash`.
* **`plutusScriptAddress`:** Creates the corresponding Cardano script `Address`.
* **`toBech32ScriptAddress`:** Converts validator to human-readable Bech32 address.

---

## 7. 🧪 Practical Usage Example

```haskell
-- Save the compiled validator
writeValidator "booking-loyalty.plutus" validator

let network = C.Testnet (C.NetworkMagic 1)
let vh      = plutusValidatorHash validator
let address = plutusScriptAddress
let bech32  = toBech32ScriptAddress network validator

putStrLn $ "Validator Hash: " <> P.show vh
putStrLn $ "Plutus Address: " <> P.show address
putStrLn $ "Bech32 Address: " <> bech32
```

---

## 8. 🧷 Testing Strategy

* Test booking completion: host receives ADA, NFT burned.
* Test guest cancellation: full refund before deadline.
* Test loyalty points increment after completion.
* Edge cases: missing NFT, wrong signatures, time violations.

---

## 9. ✅ Best Practices

* Always validate NFT presence before payments.
* Use `traceIfFalse` with descriptive messages for debugging.
* Ensure datum & redeemer structures match off-chain data.
* Serialize scripts properly for deployment.

---

## 10. 📘 Glossary of Terms

| Term          | Definition                                                        |
| ------------- | ----------------------------------------------------------------- |
| BookingDatum  | Defines host, guest, booking times, price, NFT, and loyalty info. |
| BookingAction | Actions to trigger payments, refunds, or loyalty updates.         |
| NFT           | Non-fungible token representing a booking.                        |
| SBT           | Soulbound token representing loyalty points.                      |
| Validator     | Plutus script enforcing contract rules.                           |
| POSIXTime     | Time representation for deadlines and booking periods.            |
| PubKeyHash    | Wallet identifier used on-chain.                                  |
| Bech32        | Human-readable Cardano address format.                            |
| traceIfFalse  | Debug helper for failed validation conditions.                    |
