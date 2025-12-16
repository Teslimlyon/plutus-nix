import { Lucid, Blockfrost, Data, Constr } from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

// ------------------------------------
// 1️⃣ Setup Lucid with Blockfrost
// ------------------------------------
const BLOCKFROST_PROJECT_ID = "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip";
const NETWORK = "Preprod";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    BLOCKFROST_PROJECT_ID
  ),
  NETWORK
);

// ------------------------------------
// 2️⃣ Hard-coded CBOR Hex (from Haskell output)
// ------------------------------------
// Replace the below line with your actual cbor hex from coxygen_payroll.cborhex
const PAYROLL_CBOR_HEX = `
4e4d01000033222220051200120011...
`.trim(); // ⚠️ Replace the dots (...) with your full hex

// Create validator object
const payrollValidator = { type: "PlutusV2", script: PAYROLL_CBOR_HEX };

// Derive script address from validator
const SCRIPT_ADDRESS = lucid.utils.validatorToAddress(payrollValidator);
console.log("Payroll Script Address:", SCRIPT_ADDRESS);

// ------------------------------------
// 3️⃣ UI Elements
// ------------------------------------
const connectBtn = document.getElementById("connect-btn");
const claimBtn = document.getElementById("claim-btn");
const amountInput = document.getElementById("claim-amount");
const statusDiv = document.getElementById("status");

let connectedAddress = null;

// ------------------------------------
// 4️⃣ Connect Wallet
// ------------------------------------
async function connectWallet() {
  if (!window.cardano || !window.cardano.lace) {
    alert("⚠️ Lace wallet not found!");
    return;
  }

  try {
    const api = await window.cardano.lace.enable();
    lucid.selectWallet(api);
    connectedAddress = await lucid.wallet.address();
    showStatus(`✅ Connected: ${connectedAddress}`);
    claimBtn.disabled = false;
  } catch (err) {
    showStatus(`❌ Wallet connection failed: ${err.message}`, true);
  }
}

connectBtn.addEventListener("click", connectWallet);

// ------------------------------------
// 5️⃣ Claim Monthly Pay
// ------------------------------------
claimBtn.addEventListener("click", async () => {
  if (!connectedAddress) {
    showStatus("⚠️ Connect your wallet first.", true);
    return;
  }

  const amount = parseFloat(amountInput.value);
  if (!amount || amount <= 0) {
    showStatus("⚠️ Enter a valid ADA amount.", true);
    return;
  }

  try {
    showStatus(`🔄 Building transaction to claim ${amount} ADA...`);

    // Get user payment credential
    const { paymentCredential } = lucid.utils.getAddressDetails(connectedAddress);
    const employeePKH = paymentCredential.hash;

    // Build datum (StreamDatum) and redeemer (ClaimMonthly)
    const datum = new Constr(0, [
      Data.to(employeePKH),              // sdEmployee
      Data.to(0),                        // sdLevel (Admin=0)
      Data.to(BigInt(Date.now())),       // sdStart
      Data.to(BigInt(Date.now() + 3600_000)), // sdEnd
      Data.to(0),                        // sdClaimed
      Data.to(BigInt(Date.now()))        // sdResumption
    ]);

    const redeemer = new Constr(0, []);  // ClaimMonthly

    const userUtxos = await lucid.wallet.getUtxos();

    // Create TX: Lock ADA under the payroll script
    const tx = await lucid
      .newTx()
      .collectFrom(userUtxos, Data.void())
      .payToContract(
        SCRIPT_ADDRESS,
        { inline: Data.to(datum) },
        { lovelace: BigInt(Math.floor(amount * 1_000_000)) }
      )
      .attachSpendingValidator(payrollValidator)
      .complete({ changeAddress: connectedAddress });

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    showStatus(`✅ Successfully claimed ${amount} ADA! TxHash: ${txHash}`);
  } catch (err) {
    console.error(err);
    showStatus(`❌ Transaction failed: ${err.message}`, true);
  }
});

// ------------------------------------
// 6️⃣ Helper
// ------------------------------------
function showStatus(msg, isError = false) {
  statusDiv.textContent = msg;
  statusDiv.style.color = isError ? "red" : "green";
}
