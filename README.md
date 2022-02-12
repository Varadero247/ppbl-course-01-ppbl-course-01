# Plutus Project Based Learning
## Course #1 | Gimbalabs | February - April 2022

To participate in help and discussions about this project, you must be enrolled in Plutus Project-Based Learning course at Gimbalabs. If you are enrolled and have questions, please access the discussion boards on Canvas.

## Project #1: Prepare Your Plutus Environment + Compile a Plutus Script

### Start by cloning this repository and Plutus-Apps
```
git clone https://gitlab.com/gimbalabs/ppbl/ppbl-course-01
git clone https://github.com/input-output-hk/plutus-apps
```
Then create an `/output` folder in `/ppbl-course-01/project-01`, because our Plutus compiler will expect this directory to exist
- `cd ppbl-course-01/project-01`
- `mkdir output`

## Task 1: Install Nix
- Start here: https://nixos.org/download.html
- Use Discussions in Canvas if you get stuck.
- (Optional) Review: https://nixos.org/guides/how-nix-works.html

#### You will know you are successful if:
- `nix-env --version` (https://nixos.org/download.html#nix-verify-installation)

## Task 2: Start Plutus-Apps
- In `/ppbl-course-01/project-01/cabal.project`, look for the expected tag for `plutus-apps`
- Change directory to `/plutus-apps` that was cloned earlier.
- In `/plutus-apps`, run `git checkout 4edc082309c882736e9dec0132a3c936fe63b4ea`.
- in `/plutus-apps` run `nix-shell`

#### You will know you are successful if:
- You can see the nix command line `[nix-shell:~/.../ppbl-course-01/project-01]$`

## Task 3: Compile your first Plutus Script
- In `nix-shell`, change directory to `/ppbl-course-01/project-01`
- Run `cabal update` (this may take a while the first time)
- Run `cabal repl` (this may take a while the first time)

#### You will know you are successful if:
You can run `cabal repl` and see that MyFirstPlutusCompiler is loaded:
```
Prelude Project01.MyFirstPlutusCompiler>
```
...and you can run `writeMyFirstValidatorScript` and get
```
Right ()
```
Now look in `/ppbl-course-01/project-01/output`. You should see your first compiled Plutus Script - way to go!


## Bonus Task:
If you have `cardano-cli` installed, you can create a Contract Address from this script.

```
cardano-cli address build \
--payment-script-file /ppbl-course-01/project-01/output/my-first-script.plutus
--testnet-magic 1097911063 \
--out-file my-first-script.addr
```

Look at this address on https://testnet.cardanoscan.io/. What do you notice?