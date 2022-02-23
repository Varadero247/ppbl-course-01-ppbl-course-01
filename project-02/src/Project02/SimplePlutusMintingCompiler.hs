{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Project02.SimplePlutusMintingCompiler
    ( writeSimpleMintingScript
    , writeMintingRedeemerA
    , writeMintingRedeemerB
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import           Project02.SimplePlutusMintingScript
import           Project02.MintRedeemerA
import           Project02.MintRedeemerB

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeSimpleMintingScript :: IO (Either (FileError ()) ())
writeSimpleMintingScript = writeValidator "output/simple-minting-script.plutus" Project02.SimplePlutusMintingScript.validator

writeMintingRedeemerA :: IO (Either (FileError ()) ())
writeMintingRedeemerA = writeValidator "output/redeemer-minting-script-a.plutus" Project02.MintRedeemerA.validator

writeMintingRedeemerB :: IO (Either (FileError ()) ())
writeMintingRedeemerB = writeValidator "output/redeemer-minting-script-b.plutus" Project02.MintRedeemerB.validator