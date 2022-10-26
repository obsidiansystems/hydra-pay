module CardanoNodeInfo where

newtype CardanoNodeType = TestNet { _testNetMagic  :: Int }

data CardanoNodeInfo = CardanoNodeInfo
  { _nodeType :: CardanoNodeType,
    _nodeSocket :: FilePath
  }

