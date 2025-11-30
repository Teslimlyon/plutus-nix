Perfect — you want the same clean **Markdown-style tutorial** (not HTML this time), written exactly in the structure and tone of your `Vesting.hs` version, but for your **Event Ticket smart contract**.

Here it is, preserving your format — clear, structured, consistent, and detailed.

---

# 🧾 Detailed Tutorial: Understanding and Using `EventTicket.hs`

This tutorial covers the `EventTicket.hs` module, highlighting its purpose, critical imports, datum/redeemer logic, helper utilities, and on-chain validator behavior.
This contract implements **NFT-based event ticketing**, allowing users to **buy**, **settle**, and **refund** event tickets securely on the Cardano blockchain.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum and Redeemer](#2-datum-and-redeemer)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Untyped and Script Compilation](#5-untyped-and-script-compilation)
6. [🏗️ Validator Hash and Address](#6-validator-hash-and-address)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api:**
  Defines on-chain types such as `Validator`, `ScriptContext`, and `PubKeyHash`.

* **Plutus.V2.Ledger.Contexts:**
  Used to extract transaction details and validate signatures.

* **Plutus.V1.Ledger.Interval:**
  Provides functions like `contains` and `to` for time-based validations (refund window logic).

* **Plutus.V1.Ledger.Value:**
  Supports checking token values and ADA amounts through `valueOf`, `adaSymbol`, and `adaToken`.

### Utility and Prelude Modules

* **PlutusTx / PlutusTx.Prelude:**
  Provides core functionality and enables Haskell code to compile into on-chain Plutus Core.

* **Codec.Serialise / ByteString:**
  Handles serialization of compiled scripts into `.plutus` files.

* **Cardano.Api / Shelley:**
  Used to generate Bech32 addresses for testnet/mainnet deployment.

---

## 2. 🗃️ Datum and Redeemer

### `TicketDatum`

Defines the on-chain data structure for ticket sales and management.

**Fields:**

* `tdBuyer` – Public key hash of the buyer purchasing the ticket.
* `tdSeller` – Public key hash of the event organizer or vendor.
* `tdTier` – The ticket’s category or tier (e.g., Regular, VIP).
* `tdPrice` – Ticket price in lovelace.
* `tdCurrency` – NFT currency symbol identifying the ticket.
* `tdToken` – Token name of the NFT ticket.
* `tdRefundWin` – POSIXTime until which refunds are valid.
* `tdRefundCause` – Refund reason code (e.g., 0 = Event cancelled).
* `tdSponsors` – List of sponsor public key hashes to receive a portion of revenue.
* `tdSplits` – Matching list of percentage splits for each sponsor.

---

### `TicketAction`

Specifies which action the validator should enforce during transaction execution.

**Possible Actions:**

* `BuyTicket` – Buyer purchases the NFT ticket.
* `ScanAndSettle` – Event organizer validates attendance and settles sponsor payouts.
* `RequestRefund` – Buyer requests refund within a valid refund window.

---

## 3. 🔧 Helper Functions

Helper utilities used across the contract to simplify on-chain checks.

### `inputHasNFT`

Checks whether the transaction input includes the NFT corresponding to the ticket.

### `sponsorsPaid`

Ensures that sponsors receive the correct percentage share of funds distributed from the ticket sale.

### `sellerPaid` and `buyerPaid`

Verify whether the seller or buyer received the correct ADA payment amount.

### `paidNFT`

Ensures that the NFT has been transferred to the correct recipient (buyer).

---

## 4. 🧠 Core Validator Logic

### `mkValidator`

The main validator function defining the rules for each ticketing action.
Every transaction interacting with the contract must satisfy these rules based on its `TicketAction`.

**Validation Rules:**

#### 🟩 BuyTicket

* NFT must exist in the transaction input.
* Buyer’s signature must be present.
* Seller must be paid the ticket price.
* Buyer must receive the NFT.

#### 🟨 ScanAndSettle

* NFT must be in the input.
* Seller must sign the transaction.
* Sponsors must receive their respective payout shares.

#### 🟥 RequestRefund

* NFT must exist in the input.
* Buyer must sign the transaction.
* Refund must occur before the refund window closes.
* Buyer must receive the refund amount.

```haskell
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
```

---

## 5. ⚙️ Untyped and Script Compilation

### `mkValidatorUntyped`

Converts the strongly-typed validator into an untyped version for use in the Plutus runtime using `BuiltinData`.

### `validator`

Compiles the validator logic into a Plutus Core executable script:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

---

## 6. 🏗️ Validator Hash and Address

### `plutusValidatorHash`

Generates the unique hash of the validator used to identify it on-chain.

### `plutusScriptAddress`

Derives the Plutus script address based on the validator hash.

### `toBech32ScriptAddress`

Converts the validator’s on-chain address into a Bech32 human-readable format for the Cardano network (testnet or mainnet).

---

## 7. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    -- Write compiled validator to file
    writeValidator "event_ticket.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Event Ticket Validator ---"
    putStrLn $ "Validator Hash:    " <> P.show vh
    putStrLn $ "Plutus Address:    " <> P.show onchain
    putStrLn $ "Bech32 Address:    " <> bech32
    putStrLn "--------------------------------"
```

This main function outputs:

* The **validator hash** for blockchain identification.
* The **Plutus script address** for sending ADA/tokens.
* The **Bech32 address** for off-chain integration.

---

## 8. ✅ Best Practices

* Always confirm refund logic is correctly time-locked with `POSIXTime`.
* Test sponsor percentage payouts to ensure correct splits before deployment.
* Validate buyer/seller signatures in all relevant actions.
* Include detailed `traceIfFalse` messages for clarity and debugging.
* Use unique NFTs (`CurrencySymbol`, `TokenName`) to prevent double sales.
* Perform dry-run transactions on **testnet** before deploying to mainnet.

---

## 9. 📘 Glossary of Terms

| Term               | Definition                                                                |
| ------------------ | ------------------------------------------------------------------------- |
| **TicketDatum**    | On-chain data containing buyer, seller, ticket info, and refund rules.    |
| **TicketAction**   | Redeemer specifying which ticket-related operation to perform.            |
| **Validator**      | The smart contract that enforces transaction rules on-chain.              |
| **PubKeyHash**     | A hashed wallet address used for signing and validation.                  |
| **CurrencySymbol** | Identifies a token policy under which NFTs or tokens are minted.          |
| **TokenName**      | The name of an asset within its policy.                                   |
| **POSIXTime**      | Time format used in Plutus for on-chain scheduling and expiry.            |
| **traceIfFalse**   | A Plutus function that fails a transaction with an error message.         |
| **Bech32**         | A readable format used for Cardano addresses.                             |
| **UTxO**           | Unspent Transaction Output – Cardano’s model for handling on-chain value. |
| **Plutus Script**  | The compiled executable smart contract deployed on-chain.                 |
