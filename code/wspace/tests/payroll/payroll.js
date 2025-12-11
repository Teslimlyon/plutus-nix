import {
    Lucid,
    Blockfrost,
    Data,
    Constr,
    fromText,
  } from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";
  
  // -------------------------------------
  // Lucid Setup
  // -------------------------------------
  const BLOCKFROST_PROJECT_ID = "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip";
  const NETWORK = "Preprod";
  
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      BLOCKFROST_PROJECT_ID
    ),
    NETWORK
  );
  
  // -------------------------------------
  // Hardcoded CBOR HEX of your Plutus Validator
  // Replace with your generated hex
  // -------------------------------------
  const PAYROLL_CBOR_HEX =
    "5909d85909d5010000323232323232323232323232222232323232323232323232323232323232323232323232323232323232323232323232323222222222222232323232323222222232323232222222222323232323232222323232323232323232323232323232323232323232323232323232323232323232323232323232323232323222222222232332323232323222323232222323232223232222323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232"; // <-- example placeholder CBOR
  
  // Create script object from CBOR
  const PAYROLL_SCRIPT = {
    type: "PlutusV2",
    script: PAYROLL_CBOR_HEX,
  };
  
  // Derive script address automatically
  const SCRIPT_ADDRESS = lucid.utils.validatorToAddress(PAYROLL_SCRIPT);
  
  // -------------------------------------
  // UI Elements
  // -------------------------------------
  const connectBtn = document.getElementById("connect-btn");
  const lockBtn = document.getElementById("lock-btn");
  const claimBtn = document.getElementById("claim-btn");
  const amountInput = document.getElementById("amount");
  const statusDiv = document.getElementById("status");
  
  let connectedAddress = null;
  
  // -------------------------------------
  // Connect Wallet
  // -------------------------------------
  async function connectWallet() {
    if (!window.cardano || !window.cardano.lace) {
      alert("⚠️ Lace wallet not found!");
      return;
    }
  
    try {
      const api = await window.cardano.lace.enable();
      lucid.selectWallet(api);
      connectedAddress = await lucid.wallet.address();
      showStatus(`✅ Connected to Lace wallet: ${connectedAddress}`);
      lockBtn.disabled = false;
      claimBtn.disabled = false;
    } catch (err) {
      showStatus(`❌ Wallet connection failed: ${err}`, true);
    }
  }
  connectBtn.addEventListener("click", connectWallet);
  
  // -------------------------------------
  // Lock ADA to Payroll Script
  // -------------------------------------
  lockBtn.addEventListener("click", async () => {
    const amount = parseFloat(amountInput.value);
    if (!amount || amount <= 0) {
      showStatus("⚠️ Enter a valid ADA amount.", true);
      return;
    }
  
    try {
      showStatus(`🔄 Preparing transaction to lock ${amount} ADA...`);
  
      const { paymentCredential } = lucid.utils.getAddressDetails(connectedAddress);
      const employeePkh = paymentCredential.hash;
  
      // Construct Datum for ClaimMonthly Stream
      const datum = new Constr(0, [
        Data.to(employeePkh),     // sdEmployee
        Data.to(0),               // sdLevel: Admin = 0
        Data.to(BigInt(Date.now())),
        Data.to(BigInt(Date.now() + 3600_000)), // 1h stream period
        Data.to(0n),              // sdClaimed
        Data.to(BigInt(Date.now())) // sdResumption
      ]);
  
      const tx = await lucid
        .newTx()
        .payToContract(
          SCRIPT_ADDRESS,
          { inline: Data.to(datum) },
          { lovelace: BigInt(amount * 1_000_000) }
        )
        .complete({ changeAddress: connectedAddress });
  
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
  
      showStatus(`✅ Locked ${amount} ADA into Payroll script! TxHash: ${txHash}`);
    } catch (err) {
      console.error(err);
      showStatus(`❌ Lock failed: ${err.message || err}`, true);
    }
  });
  
  // -------------------------------------
  // Claim from Script (ClaimMonthly)
  // -------------------------------------
  claimBtn.addEventListener("click", async () => {
    try {
      showStatus("🔄 Preparing claim transaction...");
  
      const scriptUtxos = await lucid.utxosAt(SCRIPT_ADDRESS);
      if (scriptUtxos.length === 0) {
        showStatus("⚠️ No funds found at script address.", true);
        return;
      }
  
      const redeemer = new Constr(0, []); // ClaimMonthly = Constr 0
      const tx = await lucid
        .newTx()
        .collectFrom(scriptUtxos, Data.to(redeemer))
        .attachSpendingValidator(PAYROLL_SCRIPT)
        .addSigner(await lucid.wallet.address())
        .complete({ changeAddress: connectedAddress });
  
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
  
      showStatus(`✅ Claim submitted successfully! TxHash: ${txHash}`);
    } catch (err) {
      console.error(err);
      showStatus(`❌ Claim failed: ${err.message || err}`, true);
    }
  });
  
  // -------------------------------------
  // Helper Function
  // -------------------------------------
  function showStatus(msg, isError = false) {
    statusDiv.textContent = msg;
    statusDiv.style.color = isError ? "red" : "green";
  }
  