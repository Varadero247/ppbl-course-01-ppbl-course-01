# Plutus PPBL Project 2: Minting a Token
## 2.1: Three Ways to Mint a Token

## Previous Sections
1. [Mint a token on `cardano-cli` with a Mary-era policy]() -- add link(s) from Piotr
2. [Mint a token in GameChanger]() -- add link(s) from Abdelkrim

## 3: Mint a token on `cardano-cli` with a Plutus-era policy

### `/project-02/src/Project02`
- `SimplePlutusMintingScript.hs` is the most basic template for minting Plutus tokens.
- `SimplePlutusMintingCompiler.hs` is a copy of `MyFirstPlutusCompiler.hs` from Project #1. Only the imports and file names are changed. We can do this because just like in Project #1, we're passing a Validator from `SimplePlutusMintingScript.hs` to `SimplePlutusMintingCompiler.hs`

### Step 1: Compile to `simple-minting-script.plutus`:

We will follow the same process as in [Project #1](https://gitlab.com/gimbalabs/ppbl/ppbl-course-01/-/tree/master/project-01).

1. Add an `/output` folder to `/ppbl-course-01/project-02`
2. Start Plutus-Apps
    - In `/ppbl-course-01/project-01/cabal.project`, look for the expected tag for `plutus-apps`
    - Change directory to `/plutus-apps` that was cloned during Project #1.
    - Run `git status` to check which branch is checked out. If you're not sure, you can use `git checkout main` to return to the main branch.
    - (If necessary): In `/plutus-apps`, run `git checkout 4edc082309c882736e9dec0132a3c936fe63b4ea`.
    - in `/plutus-apps` run `nix-shell`
3. Compile `simple-minting-script.plutus`
    - In `nix-shell`, change directory to `/ppbl-course-01/project-02`
    - Run `cabal update` (this may take a while the first time)
    - Run `cabal repl` (this may take a while the first time)
4. If you can see `Prelude Project02.SimplePlutusMintingCompiler>`...
    - That means you're "in the repl"
    - Run `writeSimpleMintingScript` and look that you now have `/output/simple-minting-script.plutus`

### Step 2: Use `cardano-cli` to create a Policy ID:
```
cardano-cli transaction policyid --script-file ppbl-course-01/project-02/output/simple-minting-script.plutus > simple-minting-script.id
```
### Step 3: Make a directory for transactions

Make a transactions folder in `/ppbl-course-01/project-02/`:
```
mkdir transactions
cd transactions
```
Get Protocol Parameters
```
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json
```

### Step 3a (if necessary): Make a Collateral TX

Note: To proceed, you will need a local cardano node synced to Testnet.

#### What should be "in" a Collateral UTxO?
A collateral UTxO can be any UTxO that contains only Lovelace, and does not contain native assets. Make sure that you have a UTxO with approximately 5000000 Lovelace (5 ADA), and just do not spend it. It's really convenient to just use the same Collateral UTxO over and over again.

Set variables in bash:

```
SENDER=$(cat ...path-to/base.addr)
SENDERKEY= path to `payment.skey` for this address
TXIN= get the TxID and TxHash for a sufficient TXIN. (note that if you already have a UTxO with a little bit of ada and no additional assets, this can serve as your collateral.)
AMOUNT=number of lovelaces to place in collateral utxo
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

### Step 4: Now Mint!

Set variables in bash:
```
SENDER=$(cat ...path-to/base.addr)
SENDERKEY= path to `payment.skey` for this address
TXIN= get the TxID and TxHash for a sufficient TXIN.
COLLATERAL=from Step 3 above
POLICYID=from Step 2 above
TOKENNAME=can be any hex string
MINTAMOUNT=can be any positive integer
SCRIPTFILE= .../path/to/simple-minting-script.plutus
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
--mint-script-file $SCRIPTFILE \
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