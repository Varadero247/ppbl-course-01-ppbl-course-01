# Plutus PPBL Project 2: Minting a Token
## 2.1: Three Ways to Mint a Token

## Previous Sections
1. Mint a token on `cardano-cli` with a Mary-era policy
2. Mint a token in GameChanger

## 3: Mint a token on `cardano-cli` with a Plutus-era policy

### `/project-02/src/Project02`
- `SimplePlutusMintingScript.hs` is the most basic template for minting Plutus tokens.
- `SimplePlutusMintingCompiler.hs` is a copy of `MyFirstPlutusCompiler.hs` from Project #1. Only the imports and file names are changed. We can do this because just like in Project #1, we're passing a Validator from `SimplePlutusMintingScript.hs` to `SimplePlutusMintingCompiler.hs`

### Step 1: Compile to `simple-minting-script.plutus`:

We will follow the same process

### Step 2: Use `cardano-cli` to create a Policy ID:
```
cardano-cli transaction policyid --script-file ppbl-course-01/project-02/output/simple-minting-script.plutus > simple-minting-script.id
```

### Step 3 (if necessary): Make a Collateral TX

You'll need a local cardano node synced up with Testnet to proceed.

Assumes some variables have been set in bash:

```
SENDER=$(cat ...path-to/base.addr)
SENDERKEY= path to `payment.skey` for this address
TXIN= get the TxID and TxHash for a sufficient TXIN. (note that if you already have a UTxO with a little bit of ada and no additional assets, this can serve as your collateral.)
AMOUNT
```

```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN \
--tx-out $SENDER+$AMOUNT \
--change-address $SENDER \
--testnet-magic 1097911063 \
--out-file make-collateral.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file make-collateral.raw \
--out-file make-collateral.signed

cardano-cli transaction submit \
--tx-file make-collateral.signed \
--testnet-magic 1097911063
```

### Step 4: Now Mint

Set variables in bash:
```
SENDER=$(cat ...path-to/base.addr)
SENDERKEY= path to `payment.skey` for this address
TXIN= get the TxID and TxHash for a sufficient TXIN.
COLLATERAL=get the TxID and TxHash for a sufficient TXIN.
POLICYID=from Step 2 above
TOKENNAME=can be any hex string
MINTAMOUNT=can be any positive integer
```

```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $SENDER+2000000+"$MINTAMOUNT $POLICYID.$TOKENNAME" \
--change-address $SENDER \
--mint="1 $POLICYID.$TOKENNAME" \
--mint-redeemer-value 1 \
--mint-script-file mint-nft.plutus \
--tx-in-collateral $COLLATERAL \
--protocol-params-file protocol.json \
--out-file check-amount.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file check-amount.raw \
--out-file check-amount.signed

cardano-cli transaction submit \
--tx-file check-amount.signed \
--testnet-magic 1097911063 \
`