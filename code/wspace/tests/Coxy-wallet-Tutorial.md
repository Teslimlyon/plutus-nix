
# 💼 Detailed Tutorial: Understanding and Using `CoxyEscrow.hs`

This tutorial explains the `CoxyEscrow.hs` Plutus smart contract.
It enables **secure token and ADA escrow transactions** between a **buyer and seller**, supporting **multi-token payments**, **refunds**, and **time-locked releases**.

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

### Plutus API Modules

* **Plutus.V2.Ledger.Api**
  Provides core types like `Validator`, `PubKeyHash`, `POSIXTime`, and `ScriptContext`.

* **Plutus.V2.Ledger.Contexts**
  Includes functions such as `findOwnInput`, `txSignedBy`, and `valuePaidTo` for transaction validation.

* **Plutus.V1.Ledger.Interval**
  Supplies the `contains` and `from` functions for time-based constraints.

* **Plutus.V1.Ledger.Value**
  Exposes functions such as `valueOf`, `adaSymbol`, and `adaToken` for multi-token and ADA management.

### Utility and Serialization Modules

* **PlutusTx / PlutusTx.Prelude**
  Enables compilation to Plutus Core and provides functional primitives for on-chain code.

* **Codec.Serialise**
  Handles script serialization into deployable `.plutus` files.

* **Cardano.Api / Shelley**
  Used to derive **Bech32** addresses for validator deployment on mainnet or testnet.

---

## 2. 🗃️ Data Structures

### `EscrowDatum`

Defines the state data stored on-chain for each escrow contract:

* `edBuyer`: Public key hash of the buyer.
* `edSeller`: Public key hash of the seller.
* `edAmountADA`: The ADA amount to be paid to the seller.
* `edDeadline`: The time limit for escrow resolution (in POSIX time).
* `edTokens`: A list of tuples `(CurrencySymbol, TokenName, Integer)` representing the token assets involved in the escrow.

This design supports **multi-token transactions**, allowing ADA and native assets to coexist within one escrow state.

---

### `EscrowAction`

Defines the possible actions allowed on the escrow:

* **`PaySeller`** — Buyer confirms delivery, releasing ADA and tokens to both parties.
* **`RefundSeller`** — Refunds tokens to the seller if the deadline passes without payment confirmation.

---

## 3. 🔧 Helper Functions

### `scriptInputContainsTokens`

Ensures that the input from the escrow UTXO contains the expected tokens.

```haskell
scriptInputContainsTokens :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)] -> Bool
scriptInputContainsTokens ctx tokens =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in all (\(cs, tn, amt) -> valueOf v cs tn >= amt) tokens
```

### `tokensPaidTo`

Checks that a recipient (buyer or seller) has received the correct amount of each token.

```haskell
tokensPaidTo :: TxInfo -> PubKeyHash -> [(CurrencySymbol, TokenName, Integer)] -> Bool
tokensPaidTo info pkh tokens =
    let v = valuePaidTo info pkh
    in all (\(cs, tn, amt) -> valueOf v cs tn >= amt) tokens
```

---

## 4. 🧠 Core Validator Logic

### `mkValidator`

The main validator function defines the smart contract’s on-chain logic.
It checks signatures, token distribution, and timing constraints.

#### **Action: `PaySeller`**

* The script input must contain the expected tokens.
* The buyer must sign the transaction.
* The seller must receive the correct ADA amount.
* The buyer must receive their agreed tokens.

#### **Action: `RefundSeller`**

* The script input must contain the expected tokens.
* The seller must sign the transaction.
* The current time must be **after the deadline**.
* The seller must receive their tokens back.

#### **Code Example:**

```haskell
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
           traceIfFalse "script input missing tokens" (scriptInputContainsTokens ctx (edTokens dat)) &&
           traceIfFalse "buyer signature missing"   (txSignedBy info (edBuyer dat)) &&
           traceIfFalse "seller not paid ADA"       (valueOf (valuePaidTo info (edSeller dat)) adaSymbol adaToken >= edAmountADA dat) &&
           traceIfFalse "buyer did not receive tokens" (tokensPaidTo info (edBuyer dat) (edTokens dat))
      RefundSeller ->
           traceIfFalse "script input missing tokens" (scriptInputContainsTokens ctx (edTokens dat)) &&
           traceIfFalse "seller signature missing" (txSignedBy info (edSeller dat)) &&
           traceIfFalse "too early for refund"     afterDeadline &&
           traceIfFalse "seller did not receive tokens" (tokensPaidTo info (edSeller dat) (edTokens dat))
```

#### **Deadline Logic:**

```haskell
afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange
```

This ensures refunds are only possible **after** the escrow deadline.

---

## 5. ⚙️ Validator Compilation

### `mkValidatorUntyped`

Wraps the validator logic into Plutus’ `BuiltinData` representation for on-chain execution:

```haskell
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
```

### `validator`

Compiles the contract into Plutus Core bytecode for deployment:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

---

## 6. 🏗️ Validator Address Generation

Helper functions are included to compute script hashes and addresses.

* **`plutusValidatorHash`** — Derives the on-chain hash of the validator.
* **`plutusScriptAddress`** — Constructs a native on-chain script address.
* **`toBech32ScriptAddress`** — Converts the address into Bech32 format for easier wallet integration.

---

## 7. 🗂️ File Writing

The validator is serialized to a `.plutus` file using:

```haskell
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path
```

This allows you to deploy the contract on-chain through tools like `cardano-cli` or Lucid.

---

## 8. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "coxy-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- COXY Wallet Escrow Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------------"
    putStrLn "COXY Escrow validator generated successfully."
```

**Example Output:**

```
--- COXY Wallet Escrow Validator Info ---
Validator Hash (Plutus): <hash>
Plutus Script Address:    <address>
Bech32 Script Address:    addr_test1w...
---------------------------------------
COXY Escrow validator generated successfully.
```

---

## 9. 🧷 Testing Strategy

* ✅ Simulate buyer and seller interactions under different deadlines.
* ✅ Ensure refund transactions fail before the deadline.
* ✅ Validate that ADA and tokens are distributed correctly.
* ✅ Test incorrect signatures and invalid token amounts.
* ✅ Test both single-token and multi-token scenarios.

---

## 10. ✅ Best Practices

* Always **trace** errors with descriptive messages for better debugging.
* Use **multi-token tests** to confirm correctness of token lists.
* Enforce **time constraints** using `Interval.contains` accurately.
* Double-check **buyer/seller roles** in each branch to avoid reversal errors.
* Always verify **Bech32 addresses** before deployment on mainnet.

---

## 11. 📘 Glossary of Terms

| Term               | Definition                                                                   |
| ------------------ | ---------------------------------------------------------------------------- |
| **Escrow**         | A contract that holds funds or assets until certain conditions are met.      |
| **Datum**          | On-chain data defining participants, amounts, and deadlines.                 |
| **Redeemer**       | The action performed on a script (e.g., PaySeller or RefundSeller).          |
| **Validator**      | The Plutus smart contract that validates transactions.                       |
| **POSIXTime**      | Standard time format used in Plutus contracts.                               |
| **CurrencySymbol** | Token policy ID for native assets on Cardano.                                |
| **TokenName**      | Name of the token within a given policy.                                     |
| **txSignedBy**     | Checks if a transaction is signed by a specific public key.                  |
| **valuePaidTo**    | Retrieves the total value sent to a given address in a transaction.          |
| **Bech32**         | Human-readable address format for Cardano networks.                          |
| **Testnet**        | A sandbox environment for testing smart contracts before mainnet deployment. |

---
