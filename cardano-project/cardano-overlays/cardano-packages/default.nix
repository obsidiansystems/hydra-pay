{ haskellLib, lib, pkgs
, ...
}:

let deps = pkgs.thunkSet ./dep;
in self: super: {

  # cardano-prelude
  # cardano-prelude = self.callCabal2nix "cardano-prelude" (deps.cardano-prelude + "/cardano-prelude") {};
  # cardano-prelude-test = self.callCabal2nix "cardano-prelude-test" (deps.cardano-prelude + "/cardano-prelude-test") {};

  # cardano-base
  cardano-binary = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary" (deps.cardano-base + "/binary") {}) "development";
  cardano-binary-test = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary-test" (deps.cardano-base + "/binary/test") {}) "development";
  cardano-slotting = self.callCabal2nix "cardano-slotting" (deps.cardano-base + "/slotting") {};
  strict-containers = self.callCabal2nix "strict-containers" (deps.cardano-base + "/strict-containers") {};
  base-deriving-via = self.callCabal2nix "base-deriving-via" (deps.cardano-base + "/base-deriving-via") {};
  orphans-deriving-via = self.callCabal2nix "orphans-deriving-via" (deps.cardano-base + "/orphans-deriving-via") {};
  measures = self.callCabal2nix "measures" (deps.cardano-base + "/measures") {};

  # cardano-ledger
  # tests fail on some env var not being set
  cardano-ledger-babbage = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron" (deps.cardano-ledger + "/eras/babbage/impl") {})) "development");
  cardano-ledger-babbage-test = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron" (deps.cardano-ledger + "/eras/babbage/test-suite") {})) "development");
  cardano-ledger-byron = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron" (deps.cardano-ledger + "/eras/byron/ledger/impl") {})) "development");
  cardano-ledger-byron-test = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron-test" (deps.cardano-ledger + "/eras/byron/ledger/impl/test") {})) "development");
  cardano-ledger-alonzo = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-alonzo" (deps.cardano-ledger + "/eras/alonzo/impl") {})) "development");
  # cardano-ledger-core = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-core" (deps.cardano-ledger + "/libs/cardano-ledger-core") {});
  cardano-ledger-shelley = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley" (deps.cardano-ledger + "/eras/shelley/impl") {});
  cardano-ledger-shelley-test = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley-test" (deps.cardano-ledger + "/eras/shelley/test-suite") {});
  cardano-ledger-shelley-ma = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley-ma" (deps.cardano-ledger + "/eras/shelley-ma/impl") {});
  cardano-protocol-tpraos = haskellLib.dontCheck (self.callCabal2nix "cardano-protocol-tpraos" (deps.cardano-ledger + "/libs/cardano-protocol-tpraos") {});
  # cardano-ledger libs
  cardano-ledger-pretty = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-pretty" (deps.cardano-ledger + "/libs/cardano-ledger-pretty") {});
  cardano-data = self.callCabal2nix "cardano-data" (deps.cardano-ledger + "/libs/cardano-data") {};
  vector-map = self.callCabal2nix "vector-map" (deps.cardano-ledger + "/libs/vector-map") {};
  set-algebra = self.callCabal2nix "set-algebra" (deps.cardano-ledger + "/libs/set-algebra") {};
  non-integral = haskellLib.dontCheck (self.callCabal2nix "non-integral" (deps.cardano-ledger + "/libs/non-integral") {});
  small-steps = haskellLib.dontCheck (self.callCabal2nix "small-steps" (deps.cardano-ledger + "/libs/small-steps") {});
  small-steps-test = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "small-steps-test" (deps.cardano-ledger + "/libs/small-steps-test") {}));
  byron-spec-chain = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-chain" (deps.cardano-ledger + "/eras/byron/chain/executable-spec") {}));
  byron-spec-ledger = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-ledger" (deps.cardano-ledger + "/eras/byron/ledger/executable-spec") {}));
  cardano-crypto-wrapper = haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-wrapper" (deps.cardano-ledger + "/eras/byron/crypto") {});
  cardano-crypto-test = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-test" (deps.cardano-ledger + "/eras/byron/crypto/test") {}));

  # iohk-monitoring
  contra-tracer = haskellLib.dontCheck (self.callCabal2nix "contra-tracer" (deps.iohk-monitoring-framework + "/contra-tracer") {});
  iohk-monitoring = haskellLib.dontCheck (self.callCabal2nix "iohk-monitoring" (deps.iohk-monitoring-framework + "/iohk-monitoring") {});
  tracer-transformers = haskellLib.dontCheck (self.callCabal2nix "tracer-transformers" (deps.iohk-monitoring-framework + "/tracer-transformers") {});
  lobemo-backend-trace-forwarder = self.callCabal2nix "lobemo-backend-trace-forwarder" (deps.iohk-monitoring-framework + "/plugins/backend-trace-forwarder") {};
  lobemo-backend-monitoring = self.callCabal2nix "lobemo-backend-monitoring" (deps.iohk-monitoring-framework + "/plugins/backend-monitoring") {};
  lobemo-backend-aggregation = self.callCabal2nix "lobemo-backend-aggregation" (deps.iohk-monitoring-framework + "/plugins/backend-aggregation") {};
  lobemo-backend-ekg = self.callCabal2nix "lobemo-backend-ekg" (deps.iohk-monitoring-framework + "/plugins/backend-ekg") {};
  lobemo-scribe-systemd = self.callCabal2nix "lobemo-scribe-systemd" (deps.iohk-monitoring-framework + "/plugins/scribe-systemd") {};

  # ouroboros-network
  # ouroboros-network = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ouroboros-network" (deps.ouroboros-network + "/ouroboros-network") {}));
  ouroboros-network-framework = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ouroboros-network-framework" (deps.ouroboros-network + "/ouroboros-network-framework") {}));
  ouroboros-network-testing = self.callCabal2nix "ouroboros-network-testing" (deps.ouroboros-network + "/ouroboros-network-testing") {};
  ouroboros-consensus = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus" (deps.ouroboros-network + "/ouroboros-consensus") {});
  ouroboros-consensus-byron = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-byron" (deps.ouroboros-network + "/ouroboros-consensus-byron") {});
  ouroboros-consensus-shelley = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-shelley" (deps.ouroboros-network + "/ouroboros-consensus-shelley") {});
  ouroboros-consensus-cardano = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-cardano" (deps.ouroboros-network + "/ouroboros-consensus-cardano") {});
  monoidal-synchronisation = self.callCabal2nix "monoidal-synchronisation" (deps.ouroboros-network + "/monoidal-synchronisation") {};
  network-mux = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "network-mux" (deps.ouroboros-network + "/network-mux") {}));
  ntp-client = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ntp-client" (deps.ouroboros-network + "/ntp-client") {}));

  # cardano-node
  # cardano-api = haskellLib.dontCheck (self.callCabal2nix "cardano-api" (deps.cardano-node + "/cardano-api") {});
  cardano-node = ((self.callCabal2nix "cardano-node" (deps.cardano-node + "/cardano-node") {}));
  cardano-cli = haskellLib.overrideCabal (self.callCabal2nix "cardano-cli" (deps.cardano-node + "/cardano-cli") {}) (drv: {
    doCheck = false;
    preCheck = ''
      export CARDANO_CLI=$PWD/dist/build/cardano-cli
      export CARDANO_NODE_SRC=$PWD
    '';
    buildTools = (drv.buildTools or []) ++ [ pkgs.jq pkgs.shellcheck pkgs.coreutils ];
    # NOTE: see link for details
    # https://3.basecamp.com/4757487/buckets/24531883/messages/5274529248
    configureFlags = [ "--dependency=cardano-api:gen=cardano-api-1.32.1-Fx8Wd6R8QrDmKMaXBLt3v-gen" ]; # gross, but it works
  });
  cardano-config = ((self.callCabal2nix "cardano-config" (deps.cardano-node + "/cardano-config") {}));
  hedgehog-extras = self.callCabal2nix "hedgehog-extras" deps.hedgehog-extras {};

  # plutus (uh oh!)
  # plutus-core = self.callCabal2nix "plutus-core" (deps.plutus + "/plutus-core") {};
  plutus-ledger = haskellLib.overrideCabal (self.callCabal2nix "plutus-ledger" (deps.plutus + "/plutus-ledger") {}) (drv: {
    doHaddock = false; # to avoid plutus-tx-plugin errors
    # NOTE: see link for details
    # https://3.basecamp.com/4757487/buckets/24531883/messages/5274529248
    configureFlags = [ "--dependency=cardano-api:gen=cardano-api-1.32.1-Fx8Wd6R8QrDmKMaXBLt3v-gen" ]; # gross, but it works
  });
  freer-extras = self.callCabal2nix "freer-extras" (deps.plutus + "/freer-extras") {};
  playground-common = self.callCabal2nix "playground-common" (deps.plutus + "/playground-common") {};
  plutus-chain-index = self.callCabal2nix "plutus-chain-index" (deps.plutus + "/plutus-chain-index") {};
  plutus-contract = haskellLib.dontHaddock (self.callCabal2nix "plutus-contract" (deps.plutus + "/plutus-contract") {}); # FIXME fix tests
  plutus-pab = haskellLib.dontHaddock (self.callCabal2nix "plutus-pab" (deps.plutus + "/plutus-pab") {});
  plutus-tx-plugin = self.callCabal2nix "plutus-tx-plugin" (deps.plutus + "/plutus-tx-plugin") {};
  # plutus-ledger-api = self.callCabal2nix "plutus-ledger-api" (deps.plutus + "/plutus-ledger-api") {};
  # only nix-build of test seems broken, but running cabal test on pkg works :/
  plutus-use-cases = haskellLib.dontHaddock (haskellLib.dontCheck (self.callCabal2nix "plutus-use-cases" (deps.plutus + "/plutus-use-cases") {}));
  prettyprinter-configurable = self.callCabal2nix "prettyprinter-configurable" (deps.plutus + "/prettyprinter-configurable") {};
  quickcheck-dynamic = self.callCabal2nix "quickcheck-dynamic" (deps.plutus + "/quickcheck-dynamic") {};
  word-array = self.callCabal2nix "word-array" (deps.plutus + "/word-array") {};
  # plutus misc
  # flat = self.callCabal2nix "flat" deps.flat {};
  size-based = haskellLib.doJailbreak super.size-based;
  row-types = self.callCabal2nix "row-types" deps.row-types {}; # "1.0.1.1"
  pcg-random = self.callHackage "pcg-random" "0.1.3.7" {};

  # cardano-addresses
  cardano-addresses = haskellLib.doJailbreak (self.callCabal2nixWithOptions "cardano-addresses" (deps.cardano-addresses + "/core") "--no-hpack" {});
  cardano-addresses-cli = haskellLib.dontCheck (self.callCabal2nixWithOptions "cardano-addresses-cli" (deps.cardano-addresses + "/command-line") "--no-hpack" { cardano-address = null; });

  # io-sim
  io-classes = self.callCabal2nix "io-classes" (deps.io-sim + "/io-classes") {};
  io-sim = self.callCabal2nix "io-sim" (deps.io-sim + "/io-sim") {};
  strict-stm = self.callCabal2nix "strict-stm" (deps.io-sim + "/strict-stm") {};

  # typed-protocols
  typed-protocols = self.callCabal2nix "typed-protocols" (deps.typed-protocols + "/typed-protocols") {};
  typed-protocols-cborg = self.callCabal2nix "typed-protocols-cborg" (deps.typed-protocols + "/typed-protocols-cborg") {};
  typed-protocols-examples = self.callCabal2nix "typed-protocols-examples" (deps.typed-protocols + "/typed-protocols-examples") {};

  # ekg-json
  ekg-json = self.callCabal2nix "ekg-json" deps.ekg-json {};

  # other iohk
  Win32-network = self.callCabal2nix "Win32-network" deps.Win32-network {};
  cardano-sl-x509 = self.callCabal2nix "cardano-sl-x509" deps.cardano-sl-x509 {};
  goblins = haskellLib.dontCheck (self.callCabal2nix "goblins" deps.goblins {});
  # cardano-crypto = self.callCabal2nix "cardano-crypto" deps.cardano-crypto {};
  bech32 = haskellLib.dontCheck (self.callCabal2nix "bech32" (deps.bech32 + "/bech32") {}); # 1.1.1 ; tests rely on bech32 executable
  bech32-th = self.callCabal2nix "bech32-th" (deps.bech32 + "/bech32-th") {}; #1.1.1
  optparse-applicative-fork = haskellLib.dontCheck (self.callCabal2nix "optparse-applicative-fork" deps.optparse-applicative {});
  servant-purescript = self.callCabal2nix "servant-purescript" deps.servant-purescript {};
  purescript-bridge = self.callCabal2nix "purescript-bridge" deps.purescript-bridge {};

  scrypt = haskellLib.overrideCabal super.scrypt (drv: { platforms = (drv.platforms or []) ++ [ "js-ghcjs" "aarch64-linux" ]; });

  # other misc
  # hw-aeson removed re-exports Data.Aeson.KeyMap which cardano-addresses expects (unintentional?)
  hw-aeson = haskellLib.overrideCabal (self.callCabal2nix "hw-aeson" deps.hw-aeson {}) { # 0.1.6.0
    preConfigure = ''
      substituteInPlace src/HaskellWorks/Data/Aeson/Compat/Map.hs \
        --replace "import qualified Data.Aeson.KeyMap as JM" "import Data.Aeson.KeyMap as JM" \
        --replace ", fromHashMapText" "" \
        --replace ", toHashMapText" ""
    '';
  };
  ekg = haskellLib.doJailbreak (self.callHackage "ekg" "0.4.0.15" {});
  yaml = self.callHackage "yaml" "0.11.7.0" {};
  trifecta = haskellLib.doJailbreak super.trifecta;
  Unique = haskellLib.dontCheck (self.callHackage "Unique" "0.4.7.9" {});
  foldl = self.callHackage "foldl" "1.4.12" {};
  rebase = haskellLib.doJailbreak super.rebase;
  profunctors = self.callHackage "profunctors" "5.6.2" {};
  contravariant = self.callHackage "contravariant" "1.5.5" {};
  semigroupoids = self.callHackage "semigroupoids" "5.3.7" {};
  StateVar = self.callHackage "StateVar" "1.2.2" {};
  criterion = self.callCabal2nix "criterion" deps.criterion {};
  js-chart = self.callHackage "js-chart" "2.9.4.1" {};
  these-lens = haskellLib.doJailbreak super.these-lens;
  lens = self.callHackage "lens" "5.1" {};
  lens-aeson = self.callHackage "lens-aeson" "1.1.3" {};
  free = self.callHackage "free" "5.1.7" {};
  microstache = self.callHackage "microstache" "1.0.2" {};
  aeson = self.callHackage "aeson" "2.0.2.0" {};
  aeson-qq = haskellLib.dontCheck super.aeson-qq;
  aeson-pretty = self.callHackage "aeson-pretty" "0.8.9" {};
  ghcjs-base-stub = self.callCabal2nix "ghcjs-base-stub" deps.ghcjs-base-stub {};
  hpack = self.callHackage "hpack" "0.34.5" {};
  dependent-sum-aeson-orphans = haskellLib.doJailbreak super.dependent-sum-aeson-orphans;
  deriving-aeson = self.callHackage "deriving-aeson" "0.2.8" {};
  semialign = self.callHackage "semialign" "1.2.0.1" {};
  openapi3 = haskellLib.doJailbreak (self.callHackage "openapi3" "3.1.0" {});
  servant-openapi3 = haskellLib.doJailbreak (self.callHackage "servant-openapi3" "2.0.1.2" {});
  servant = self.callHackage "servant" "0.18.3" {};
  servant-client = self.callHackage "servant-client" "0.18.3" {};
  servant-client-core = self.callHackage "servant-client-core" "0.18.3" {};
  servant-foreign = self.callHackage "servant-foreign" "0.15.4" {};
  servant-options = self.callHackage "servant-options" "0.1.0.0" {};
  servant-server = self.callHackage "servant-server" "0.18.3" {};
  servant-subscriber = self.callHackage "servant-subscriber" "0.7.0.0" {};
  servant-websockets = self.callHackage "servant-websockets" "2.0.0" {};
  http2 = haskellLib.dontCheck super.http2;
  http-media = haskellLib.doJailbreak super.http-media;
  tasty-bench = self.callHackage "tasty-bench" "0.2.5" {};
  async-timer = haskellLib.doJailbreak (haskellLib.dontCheck (haskellLib.markUnbroken super.async-timer));
  OddWord = haskellLib.dontCheck (haskellLib.markUnbroken super.OddWord);
  quickcheck-state-machine = haskellLib.dontCheck (haskellLib.markUnbroken super.quickcheck-state-machine);
  # tests are not compatible with base16-bytestring 1.x
  cryptohash-md5 = haskellLib.dontCheck super.cryptohash-md5;
  # tests are not compatible with base16-bytestring 1.x
  cryptohash-sha1 = haskellLib.dontCheck super.cryptohash-sha1;
  # tests are not compatible with base16-bytestring 1.x
  monoidal-containers = self.callHackage "monoidal-containers" "0.6.2.0" {};
  witherable = self.callHackage "witherable" "0.4.2" {};
  indexed-traversable = self.callHackage "indexed-traversable" "0.1.1" {};
  # QuickCheck constraints
  indexed-traversable-instances = haskellLib.dontCheck (self.callHackage "indexed-traversable-instances" "0.1" {});
  hs-rqlite = haskellLib.doJailbreak super.hs-rqlite;
  tls = self.callHackage "tls" "1.5.5" {};
  libsystemd-journal = haskellLib.overrideCabal (self.callHackage "libsystemd-journal" "1.4.5" {}) (drv: {
    librarySystemDepends = drv.librarySystemDepends or [] ++ [ pkgs.systemd ];
  });
  beam-sqlite = haskellLib.doJailbreak (self.callHackage "beam-sqlite" "0.5.0.0" {});
  scientific = haskellLib.dontCheck (self.callHackage "scientific" "0.3.7.0" {}); # min version compat with plutus-contract tests
  integer-logarithms = haskellLib.doJailbreak (self.callHackage "integer-logarithms" "1.0.3.1" {});
  smallcheck = self.callHackage "smallcheck" "1.2.1" {};
  memory = self.callCabal2nix "memory" deps.hs-memory {}; # 0.16
  katip = haskellLib.dontCheck (self.callHackage "katip" "0.8.7.0" {}); # tests also disabled in iohk-monitoring
  hspec-golden-aeson = haskellLib.dontCheck (self.callHackage "hspec-golden-aeson" "0.9.0.0" {}); # tests fail :/
  tasty-golden = self.callHackage "tasty-golden" "2.3.4" {};
  tasty = self.callHackage "tasty" "1.4.1" {};
  tasty-wai = self.callHackage "tasty-wai" "0.1.1.1" {};
  # tasty 1.4.1
  blaze-markup = haskellLib.doJailbreak super.blaze-markup;
  natural-transformation = haskellLib.doJailbreak super.natural-transformation;
  tdigest = haskellLib.doJailbreak super.tdigest;
  binary-orphans = haskellLib.doJailbreak super.binary-orphans;
  text-short = self.callHackage "text-short" "0.1.5" {};
  bytestring-type = haskellLib.doJailbreak super.bytestring-type;
  base64-bytestring-type = haskellLib.doJailbreak super.base64-bytestring-type;
  tree-diff = haskellLib.dontCheck (self.callHackage "tree-diff" "0.2.1.1" {});
  lattices = haskellLib.doJailbreak super.lattices;
  insert-ordered-containers = haskellLib.doJailbreak super.insert-ordered-containers;
  swagger2 = haskellLib.dontCheck (self.callHackage "swagger2" "2.6" {});
  lzma = haskellLib.dontCheck super.lzma;
  aeson-casing = haskellLib.dontCheck super.aeson-casing;
  servant-swagger-ui-core = self.callHackage "servant-swagger-ui-core" "0.3.5" {};
  servant-swagger-ui = self.callHackage "servant-swagger-ui" "0.3.5.3.47.1" {};
  recursion-schemes = self.callHackage "recursion-schemes" "5.2.2.2" {};

  persistent = self.callCabal2nix "persistent" (deps.persistent + "/persistent") {}; # 2.13.1.2
  persistent-test = self.callHackage "persistent-test" "2.13.0.0" {};
  persistent-sqlite = haskellLib.addPkgconfigDepend (haskellLib.enableCabalFlag (haskellLib.enableCabalFlag (self.callHackage "persistent-sqlite" "2.13.0.2" {}) "systemlib") "use-pkgconfig") pkgs.sqlite;
  persistent-postgresql = haskellLib.dontCheck (self.callHackage "persistent-postgresql" "2.13.0.3" {}); # tests use network
  persistent-template = self.callHackage "persistent-template" "2.12.0.0" {};
  persistent-qq = haskellLib.dontCheck super.persistent-qq;
  lift-type = self.callHackage "lift-type" "0.1.0.0" {};
  sqlite = null; #haskellLib.markUnbroken super.sqlite;

  generics-sop = self.callHackage "generics-sop" "0.5.1.2" {};
  nothunks = haskellLib.dontCheck (self.callHackage "nothunks" "0.1.3" {});
  moo = haskellLib.dontCheck (haskellLib.markUnbroken super.moo); # tests are failing :/
  gray-code = haskellLib.overrideCabal (haskellLib.markUnbroken super.gray-code) {
    preCompileBuildDriver = "rm Setup.hs";
  };
  # primitive = haskellLib.dontCheck (self.callHackage "primitive" "0.7.1.0" {});
  streaming-bytestring = self.callHackage "streaming-bytestring" "0.2.1" {}; # cardano-crypto-class min bound
  canonical-json = haskellLib.dontCheck (haskellLib.doJailbreak (haskellLib.markUnbroken super.canonical-json));
  cborg = haskellLib.dontCheck super.cborg; # tests don't build for base16-bytestring >=1
  text-conversions = self.callHackage "text-conversions" "0.3.1" {}; # compatible with base16-bytestring 1.x
  base16-bytestring = self.callHackage "base16-bytestring" "1.0.2.0" {}; # for cardano-prelude
  base64-bytestring = self.callCabal2nix "base64-bytestring" deps.base64-bytestring {}; # 1.2.1.0
  unordered-containers = self.callHackage "unordered-containers" "0.2.16.0" {}; # for cardano-addresses
  # protolude = self.callHackage "protolude" "0.3.0" {}; # for cardano-prelude
  # formatting = self.callHackage "formatting" "7.1.0" {};
  # fmt = self.callCabal2nix "fmt" deps.fmt {};
  fgl = haskellLib.doJailbreak super.fgl;
  fgl-arbitrary = haskellLib.doJailbreak super.fgl-arbitrary;
  string-interpolate = self.callHackage "string-interpolate" "0.3.1.1" {};
  wide-word = haskellLib.dontCheck (self.callHackage "wide-word" "0.1.1.2" {});
  graphviz = haskellLib.dontCheck super.graphviz;
  # misc expensive. 21.05 should have more recent versions.
  hspec = self.callHackage "hspec" "2.8.2" {};
  hspec-core = self.callHackage "hspec-core" "2.8.2" {};
  hspec-discover = self.callHackage "hspec-discover" "2.8.2" {};
  hspec-expectations = self.callHackage "hspec-expectations" "0.8.2" {};
  hspec-meta = self.callHackage "hspec-meta" "2.7.8" {};
  QuickCheck = self.callHackage "QuickCheck" "2.14.2" {};
  quickcheck-instances = self.callHackage "quickcheck-instances" "0.3.27" {};
  hedgehog = self.callCabal2nix "hedgehog" (deps.haskell-hedgehog + "/hedgehog") {}; # self.callHackage "hedgehog" "1.1" {};
  hedgehog-fn = haskellLib.doJailbreak super.hedgehog-fn; # allow newer hedgehog
  hedgehog-quickcheck = haskellLib.doJailbreak (self.callHackage "hedgehog-quickcheck" "0.1.1" {});
  # allow newer QuickCheck/hspec
  time-compat = self.callHackage "time-compat" "1.9.6" {};
  strict = self.callHackage "strict" "0.4.0.1" {};
  vector = self.callHackage "vector" "0.12.3.1" {};
  attoparsec = haskellLib.doJailbreak super.attoparsec;
  random = haskellLib.dontCheck (self.callHackage "random" "1.2.0" {});
  generic-random = haskellLib.dontCheck (self.callHackage "generic-random" "1.4.0.0" {});
  # TODO: fix rp version about unkwon pkg
  splitmix = haskellLib.dontCheck (self.callHackage "splitmix" "0.1.0.3" {}); # dontCheck to avoid cycle with random
  http-api-data = haskellLib.doJailbreak super.http-api-data;
  algebraic-graphs = haskellLib.doJailbreak (haskellLib.dontCheck super.algebraic-graphs);
  cassava = haskellLib.doJailbreak super.cassava;
  psqueues = haskellLib.doJailbreak super.psqueues;
  tasty-hedgehog = haskellLib.doJailbreak super.tasty-hedgehog;
  tasty-hspec = self.callHackage "tasty-hspec" "1.2" {};
  tasty-discover = haskellLib.dontCheck super.tasty-discover;
  test-framework = haskellLib.doJailbreak super.test-framework;
  test-framework-quickcheck2 = haskellLib.doJailbreak super.test-framework-quickcheck2;
  base-orphans = self.callHackage "base-orphans" "0.8.6" {};
  dom-lt = haskellLib.markUnbroken super.dom-lt;
  # # Bump to fix build with ghcjs
  # network = self.callHackage "network" "3.1.2.1" {};
  # network-bsd = self.callHackage "network-bsd" "2.8.1.0" {};
  jsaddle = haskellLib.doJailbreak super.jsaddle; # allow newer base64-bytestring
  webdriver = haskellLib.overrideCabal super.webdriver (drv: {
    jailbreak = true;
    editedCabalFile = null;
    patches = [
      # aeson 2 support
      (pkgs.fetchpatch {
        url = "https://github.com/kallisti-dev/hs-webdriver/pull/183.patch";
        sha256 = "0wb6ynr13kbxqk9wpzw76q8fffs0kv1ddi7vlmwpnxxjsax98z89";
      })
    ];
  });
  th-compat = self.callHackage "th-compat" "0.1.3" {};
  OneTuple = self.callHackage "OneTuple" "0.3.1" {};
  validation = haskellLib.doJailbreak super.validation;
  validation-selective = haskellLib.dontCheck super.validation-selective;
  base16 = self.callHackage "base16" "0.3.1.0" {};
}
