module CardanoNodeInfo where

newtype CardanoNodeType = TestNet { _testNetMagic  :: Int }

data CardanoNodeInfo = CardanoNodeInfo
  { _nodeType :: CardanoNodeType,
    _nodeSocket :: FilePath
  }

devnetDefaultInfo :: CardanoNodeInfo
devnetDefaultInfo = CardanoNodeInfo (TestNet 42) "devnet/node.socket"