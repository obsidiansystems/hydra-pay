{
  deps
, lib
, hydra-poc
, pkgs ? import <nixpkgs> {}
}:
let
  haskellLib = pkgs.haskell.lib;

  hedgehog-extras = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "hedgehog-extras";
    rev = "967d79533c21e33387d0227a5f6cc185203fe658";
    sha256 = "0rbqb7a64aya1qizlr3im06hdydg9zr6sl3i8bvqqlf7kpa647sd";
  };

  optparse-applicative-fork = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "optparse-applicative";
    rev = "7497a29cb998721a9068d5725d49461f2bba0e7a";
    sha256 = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
  };

  goblins = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "goblins";
    rev = "cde90a2b27f79187ca8310b6549331e59595e7ba";
    sha256 = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
  };

  typed-protocols = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "typed-protocols";
    rev = "181601bc3d9e9d21a671ce01e0b481348b3ca104";
    sha256 = "1lr97b2z7l0rpsmmz92rsv27qzd5vavz10cf7n25svya4kkiysp5";
  };

  quickcheck-dynamic = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "quickcheck-dynamic";
    rev = "c272906361471d684440f76c297e29ab760f6a1e";
    sha256 = "1b9ppgavqad78a2z1zxv7v4jasjz6zz0mxkr0zx0bbcd0i00jajf";
  };

  cardano-api = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-node";
    rev = "aed8e71339cf6c92847fff83fbd92be61e468174";
    sha256 = "1j8lkrg8xz6gjaq8grx7vc4cynlz5x6n3cd9q4y5w3kzsd89072a";
  };

  cardano-base = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-base";
    rev = "a3c13fb11bc41fedff7885ca70a3b33f61fef4b5";
    sha256 = "0h492cz9mvzbsl5yzvp3iq40c0z0j5hmrifdrnnqzzk02g9j9c4b";
  };

  cardano-ledger = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-ledger";
    rev = "f49879a79098d9372d63baa13b94a941a56eda34";
    sha256 = "0i9x66yqkrvx2w79dy6lzlya82yxc8567rgjj828vc2d46d6nvx6";
  };

  cardano-crypto = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-crypto";
    rev = "f73079303f663e028288f9f4a9e08bcca39a923e";
    sha256 = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
  };

  io-sim = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "io-sim";
    rev = "f4183f274d88d0ad15817c7052df3a6a8b40e6dc";
    sha256 = "0vb2pd9hl89v2y5hrhrsm69yx0jf98vppjmfncj2fraxr3p3lldw";
  };

  cardano-prelude = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-prelude";
    rev = "6ea36cf2247ac0bc33e08c327abec34dfd05bd99";
    sha256 = "0z2y3wzppc12bpn9bl48776ms3nszw8j58xfsdxf97nzjgrmd62g";
  };

  plutus = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "plutus";
    rev = "f680ac6979e069fcc013e4389ee607ff5fa6672f";
    sha256 = "180jq8hd0jlg48ya7b5yw3bnd2d5czy0b1agy9ng3mgnzpyq747i";
  };

  ouroboros-network = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "ouroboros-network";
    rev = "c764553561bed8978d2c6753d1608dc65449617a";
    sha256 = "0hdh7xdrvxw943r6qr0xr4kwszindh5mnsn1lww6qdnxnmn7wcsc";
  };

  iohk-monitoring-framework = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-monitoring-framework";
    rev = "066f7002aac5a0efc20e49643fea45454f226caa";
    sha256 = "0s6x4in11k5ba7nl7la896g28sznf9185xlqg9c604jqz58vj9nj";
  };

  Win32-network = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "Win32-network";
    rev = "3825d3abf75f83f406c1f7161883c438dac7277d";
    sha256 = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
  };
in
self: super: {
  # hydra-poc
  hydra-cluster = self.callCabal2nix "hydra-cluster" (hydra-poc + "/hydra-cluster") {};
  hydra-node = self.callCabal2nix "hydra-node" (hydra-poc + "/hydra-node") {};
  hydra-plutus = self.callCabal2nix "hydra-plutus" (hydra-poc + "/hydra-plutus") {};
  hydra-prelude = self.callCabal2nix "hydra-prelude" (hydra-poc + "/hydra-prelude") {};
  hydra-test-utils = self.callCabal2nix "hydra-test-utils" (hydra-poc + "/hydra-test-utils") {};
  hydra-tui = self.callCabal2nix "hydra-tui" (hydra-poc + "/hydra-tui") {};
  hydra-cardano-api = self.callCabal2nix "hydra-cardano-api" (hydra-poc + "/hydra-cardano-api") {};
  plutus-cbor = self.callCabal2nix "plutus-cbor" (hydra-poc + "/plutus-cbor") {};
  plutus-merkle-tree = self.callCabal2nix "plutus-merkle-tree" (hydra-poc + "/plutus-merkle-tree") {};
  hydraw = self.callCabal2nix "hydraw" (hydra-poc + "/hydraw") {};

  # iohk-monitoring-framework
  contra-tracer = self.callCabal2nix "contra-tracer" (iohk-monitoring-framework + "/contra-tracer") {};
  iohk-monitoring = self.callCabal2nix "iohk-monitoring" (iohk-monitoring-framework + "/iohk-monitoring") {};
  tracer-transformers = self.callCabal2nix "tracer-transformers" (iohk-monitoring-framework + "/tracer-transformers") {};

  # cardano-api
  # cardano-api = self.callCabal2nix "cardano-api" (cardano-api + "/cardano-api") {};
  # cardano-ledger-byron-test = null;
  cardano-api = haskellLib.overrideCabal (self.callCabal2nix "cardano-api" (cardano-api + "/cardano-api") {}) (drv: {
    doCheck = false;
    preConfigure = ''
                    substituteInPlace cardano-api.cabal \
                      --replace ", cardano-ledger-byron-test" ""

                    substituteInPlace gen/Gen/Cardano/Api/Typed.hs \
                      --replace "import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)" "" \
                      --replace "genVKWitness " "undefined "

                  '';
  });

  # cardano-base
  cardano-crypto-class = self.callCabal2nix "cardano-crypto-class" (cardano-base + "/cardano-crypto-class") {};
  cardano-crypto-tests = self.callCabal2nix "cardano-crypto-tests" (cardano-base + "/cardano-crypto-tests") {};
  cardano-crypto-praos = self.callCabal2nix "cardano-crypto-praos" (cardano-base + "/cardano-crypto-praos") {};
  cardano-binary = self.callCabal2nix "cardano-binary" (cardano-base + "/binary") {};
  cardano-binary-test = self.callCabal2nix "cardano-binary-test" (cardano-base + "/binary/test") {};
  base-deriving-via = self.callCabal2nix "base-deriving-via" (cardano-base + "/base-deriving-via") {};
  cardano-slotting = self.callCabal2nix "cardano-slotting" (cardano-base + "/slotting") {};
  measures = self.callCabal2nix "measures" (cardano-base + "/measures") {};
  orphans-deriving-via = self.callCabal2nix "orphans-deriving-via" (cardano-base + "/orphans-deriving-via") {};
  strict-containers = self.callCabal2nix "strict-containers" (cardano-base + "/strict-containers") {};

  # cardano-prelude
  cardano-prelude = self.callCabal2nix "cardano-prelude" (cardano-prelude + "/cardano-prelude") {};
  cardano-prelude-test = self.callCabal2nix "cardano-prelude-test" (cardano-prelude + "/cardano-prelude-test") {};

  # cardano-ledger

  # Why is this named like this?
  cardano-crypto-test = self.callCabal2nix "cardano-crypto-test" (cardano-ledger + "/eras/byron/crypto/test") {};
  cardano-ledger-babbage = self.callCabal2nix "cardano-ledger-babbage" (cardano-ledger + "/eras/babbage/impl") {};
  cardano-ledger-babbage-test = self.callCabal2nix "cardano-ledger-babbage-test" (cardano-ledger + "/eras/babbage/test-suite") {};

  cardano-ledger-alonzo = self.callCabal2nix "cardano-ledger-alonzo" (cardano-ledger + "/eras/alonzo/impl") {};
  cardano-ledger-alonzo-test = self.callCabal2nix "cardano-ledger-alonzo-test" (cardano-ledger + "/eras/alonzo/test-suite") {};

  cardano-ledger-shelley = self.callCabal2nix "cardano-ledger-shelley" (cardano-ledger + "/eras/shelley/impl") {};
  cardano-ledger-shelley-test = self.callCabal2nix "cardano-ledger-shelley-test" (cardano-ledger + "/eras/shelley/test-suite") {};

  cardano-ledger-byron = self.callCabal2nix "cardano-ledger-byron" (cardano-ledger + "/eras/byron/ledger/impl") {};
  cardano-ledger-byron-test = self.callCabal2nix "cardano-ledger-byron-test" (cardano-ledger + "/eras/byron/ledger/impl/test") {};
  byron-spec-chain = self.callCabal2nix "byron-spec-chain" (cardano-ledger + "/eras/byron/chain/executable-spec") {};
  byron-spec-ledger = self.callCabal2nix "byron-spec-ledger" (cardano-ledger + "/eras/byron/ledger/executable-spec") {};

  cardano-ledger-shelley-ma = self.callCabal2nix "cardano-ledger-shelley-ma" (cardano-ledger + "/eras/shelley-ma/impl") {};
  cardano-ledger-shelley-ma-test = self.callCabal2nix "cardano-ledger-shelley-ma-test" (cardano-ledger + "/eras/shelley-ma/test-suite") {};

  cardano-protocol-tpraos = self.callCabal2nix "cardano-protocol-tpraos" (cardano-ledger + "/libs/cardano-protocol-tpraos") {};
  cardano-ledger-core = self.callCabal2nix "cardano-ledger-core" (cardano-ledger + "/libs/cardano-ledger-core") {};
  cardano-ledger-pretty = self.callCabal2nix "cardano-ledger-pretty" (cardano-ledger + "/libs/cardano-ledger-pretty") {};
  cardano-ledger-test = self.callCabal2nix "cardano-ledger-test" (cardano-ledger + "/libs/cardano-ledger-test") {};
  cardano-data = self.callCabal2nix "cardano-data" (cardano-ledger + "/libs/cardano-data") {};
  set-algebra = self.callCabal2nix "set-algebra" (cardano-ledger + "/libs/set-algebra") {};
  small-steps = self.callCabal2nix "small-steps" (cardano-ledger + "/libs/small-steps") {};
  small-steps-test = self.callCabal2nix "small-steps-test" (cardano-ledger + "/libs/small-steps-test") {};
  non-integral = self.callCabal2nix "non-integral" (cardano-ledger + "/libs/non-integral") {};
  vector-map = self.callCabal2nix "vector-map" (cardano-ledger + "/libs/vector-map") {};


  cardano-crypto-wrapper = haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-wrapper" (cardano-ledger + "/eras/byron/crypto") {});

  # cardano-crypto
  cardano-crypto = self.callCabal2nix "cardano-crypto" (cardano-crypto) {};

  # io-sim
  io-classes = self.callCabal2nix "io-classes" (io-sim + "/io-classes") {};
  io-sim = self.callCabal2nix "io-sim" (io-sim + "/io-sim") {};
  strict-stm = self.callCabal2nix "strict-stm" (io-sim + "/strict-stm") {};

  # plutus
  plutus-core = self.callCabal2nix "plutus-core" (plutus + "/plutus-core") {};
  plutus-ledger-api = self.callCabal2nix "plutus-ledger-api" (plutus + "/plutus-ledger-api") {};
  plutus-tx = self.callCabal2nix "plutus-tx" (plutus + "/plutus-tx") {};
  plutus-tx-plugin = self.callCabal2nix "plutus-tx-plugin" (plutus + "/plutus-tx-plugin") {};
  prettyprinter-configurable = self.callCabal2nix "prettyprinter-configurable" (plutus + "/prettyprinter-configurable") {};
  plutus-ghc-stub = self.callCabal2nix "plutus-ghc-stub" (plutus + "/stubs/plutus-ghc-stub") {};
  word-array = self.callCabal2nix "word-array" (plutus + "/word-array") {};

  # ouroboros-network
  monoidal-synchronisation = self.callCabal2nix "monoidal-synchronisation" (ouroboros-network + "/monoidal-synchronisation") {};
  network-mux = self.callCabal2nix "network-mux" (ouroboros-network + "/network-mux") {};
  ouroboros-consensus = self.callCabal2nix "ouroboros-consensus" (ouroboros-network + "/ouroboros-consensus") {};
  ouroboros-consensus-byron = self.callCabal2nix "ouroboros-consensus-byron" (ouroboros-network + "/ouroboros-consensus-byron") {};
  ouroboros-consensus-byronspec = self.callCabal2nix "ouroboros-consensus-byronspec" (ouroboros-network + "/ouroboros-consensus-byronspec") {};
  ouroboros-consensus-byron-test = self.callCabal2nix "ouroboros-consensus-byron-test" (ouroboros-network + "/ouroboros-consensus-byron-test") {};
  ouroboros-consensus-cardano = self.callCabal2nix "ouroboros-consensus-cardano" (ouroboros-network + "/ouroboros-consensus-cardano") {};
  ouroboros-consensus-cardano-test = self.callCabal2nix "ouroboros-consensus-cardano-test" (ouroboros-network + "/ouroboros-consensus-cardano-test") {};
  ouroboros-consensus-protocol = self.callCabal2nix "ouroboros-consensus-protocol" (ouroboros-network + "/ouroboros-consensus-protocol") {};
  ouroboros-consensus-shelley = self.callCabal2nix "ouroboros-consensus-shelley" (ouroboros-network + "/ouroboros-consensus-shelley") {};
  ouroboros-consensus-shelley-test = self.callCabal2nix "ouroboros-consensus-shelley-test" (ouroboros-network + "/ouroboros-consensus-shelley-test") {};
  ouroboros-consensus-test = self.callCabal2nix "ouroboros-consensus-test" (ouroboros-network + "/ouroboros-consensus-test") {};
  ouroboros-consensus-mock = self.callCabal2nix "ouroboros-consensus-mock" (ouroboros-network + "/ouroboros-consensus-mock") {};
  ouroboros-network = self.callCabal2nix "ouroboros-network" (ouroboros-network + "/ouroboros-network") {};
  ouroboros-network-framework = self.callCabal2nix "ouroboros-network-framework" (ouroboros-network + "/ouroboros-network-framework") {};
  ouroboros-network-testing = self.callCabal2nix "ouroboros-network-testing" (ouroboros-network + "/ouroboros-network-testing") {};

  # quickcheck-dynamic
  quickcheck-dynamic = self.callCabal2nix "quickcheck-dynamic" (quickcheck-dynamic) {};

  # typed-protocols
  typed-protocols = self.callCabal2nix "typed-protocols" (typed-protocols + "/typed-protocols") {};
  typed-protocols-cborg = self.callCabal2nix "typed-protocols-cborg" (typed-protocols + "/typed-protocols-cborg") {};
  typed-protocols-examples = self.callCabal2nix "typed-protocols-examples" (typed-protocols + "/typed-protocols-examples") {};

  # broken packages
  secp256k1 = null;
  secp256k1-haskell = haskellLib.overrideCabal (haskellLib.dontCheck (haskellLib.markUnbroken (self.callCabal2nix "secp256k1-haskell" deps.secp256k1-haskell {}))) (drv : {
    prePatch = "hpack --force";
    librarySystemDepends = [pkgs.secp256k1];
  });
  # secp256k1-haskell = haskellLib.dontCheck (haskellLib.markUnbroken (self.callCabal2nix "secp256k1" deps.secp256k1-haskell {}));
    # haskellLib.markUnbroken super.secp256k1-haskell;
  canonical-json = haskellLib.dontCheck (haskellLib.doJailbreak (haskellLib.markUnbroken super.canonical-json));
  bech32 = haskellLib.dontCheck (haskellLib.markUnbroken super.bech32);
  bech32-th = haskellLib.dontCheck super.bech32-th;
  gray-code = haskellLib.overrideCabal (haskellLib.markUnbroken super.gray-code) {
    preCompileBuildDriver = "rm Setup.hs";
  };

  base16 = haskellLib.dontCheck (haskellLib.markUnbroken super.base16);
  hs-rqlite = null; # haskellLib.doJailbreak (haskellLib.markUnbroken super.hs-rqlite);
  async-timer = haskellLib.markUnbroken super.async-timer;

  # goblins
  goblins = self.callCabal2nix "goblins" (goblins) {};

  # hedgehog-extras
  hedgehog-extras = self.callCabal2nix "hedgehog-extras" (hedgehog-extras) {};

  # optparse-applicative-fork
  optparse-applicative-fork = haskellLib.dontCheck (self.callCabal2nix "optparse-applicative-fork" (optparse-applicative-fork) {});

  # Win32-network
  Win32-network = self.callCabal2nix "Win32-network" (Win32-network) {};

  QuickCheck = self.callHackage "QuickCheck" "2.14.2" {};
  quickcheck-instances = self.callHackage "quickcheck-instances" "0.3.25.2" {};
  hedgehog = self.callHackage "hedgehog" "1.0.5" {};
  hedgehog-quickcheck = self.callHackage "hedgehog-quickcheck" "0.1.1" {};
  time-compat = haskellLib.dontCheck super.time-compat;
  # template-haskell = haskellLib.dontCheck ( (self.callHackage "template-haskell" "2.11.0.0" {}));
  # template-haskell = haskellLib.dontCheck (haskellLib.doJailbreak (self.callHackage "template-haskell" "2.11.0.0" {}));

  semialign = self.callHackage "semialign" "1.2.0.1" {};
}
