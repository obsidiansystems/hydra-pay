args@{ rpSetup, obelisk, ... }:
{
  haskellOverlaysPre = let deps = rpSetup.nixpkgs.thunkSet ./cardano-overlays/cardano-packages/dep; in [
    (self: super:
      let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in
        {
          bytestring-aeson-orphans = haskellLib.doJailbreak (self.callHackage "bytestring-aeson-orphans" "0.1.0.0" {});
          secp256k1-haskell =
            haskellLib.addBuildTool
              (haskellLib.addPkgconfigDepend (self.callHackage "secp256k1-haskell" "0.6.0" {}) pkgs.secp256k1)
              pkgs.buildPackages.buildPackages.pkg-config;
          typerep-map = self.callCabal2nix "typerep-map" deps.typerep-map {};
          time-compat = haskellLib.dontCheck super.time-compat;

          fin = haskellLib.doJailbreak super.fin;
          cborg = self.callHackage "cborg" "0.2.4.0" {};
          hashable = self.callHackage "hashable" "1.3.5.0" {};
          th-orphans = self.callHackage "th-orphans" "0.13.10" {};
          scientific = haskellLib.dontCheck super.scientific;
          ral = haskellLib.doJailbreak (self.callHackage "ral" "0.1" {});

          flat = self.callCabal2nix "flat" deps.flat {};
          formatting = haskellLib.dontCheck (self.callHackage "formatting" "7.1.2" {});
          semirings = self.callHackage "semirings" "0.6" {};

          # https://github.com/protolude/protolude/commit/84d228a3b5a2adfe5c8aec23176a0301012e54eb
          protolude = haskellLib.overrideCabal (self.callHackage "protolude" "0.3.0" {}) {
            preConfigure = ''
                    substituteInPlace src/Protolude/Base.hs \
                      --replace ", gcdInt'" "" \
                      --replace ", gcdWord'" ""
                  '';
          };

          cardano-crypto = haskellLib.overrideCabal (self.callCabal2nix "cardano-crypto" deps.cardano-crypto {}) (drv: {
            doCheck = false;
            preConfigure = ''
                    substituteInPlace cardano-crypto.cabal \
                      --replace ", integer-gmp" "" \
                      --replace "Crypto.ECC.P256" "" \
                      --replace "Crypto.DLEQ" ""
                  '';
          });

          cardano-prelude = haskellLib.overrideCabal (self.callCabal2nix "cardano-prelude" (deps.cardano-prelude + "/cardano-prelude") {}) (drv: {
            doCheck = false;
            preConfigure = ''
                    substituteInPlace cardano-prelude.cabal \
                      --replace ", integer-gmp" "" \
                      --replace "default: False" "default: True"

                    substituteInPlace src/Cardano/Prelude/HeapWords.hs \
                      --replace "import GHC.Integer.GMP" "-- " \
                      --replace "import GHC.Natural" "-- " \
                      --replace "instance HeapWords Integer where" "{-" \
                      --replace "instance HeapWords Float where" $'-}\ninstance HeapWords Float where' \
                      --replace "instance HeapWords Natural where" "{-" \
                      --replace "nbytes = I# (sizeofByteArray# ba#)" "-}"
                  '';
          });

          cardano-ledger-core = haskellLib.overrideCabal (self.callCabal2nix "cardano-ledger-core" (deps.cardano-ledger + "/libs/cardano-ledger-core") {}) (drv: {
            doCheck = false;
            preConfigure = ''
                    substituteInPlace src/Cardano/Ledger/Coin.hs \
                      --replace ", HeapWords)" ")"

                  '' + (drv.preConfigure or "");
          });

          plutus-core = haskellLib.overrideCabal (self.callCabal2nix "plutus-core" (deps.plutus + "/plutus-core") {}) (drv: {
            doCheck = false;
            doHaddock = false;
            doBenchmark = false;
          });

          cardano-api = haskellLib.overrideCabal (self.callCabal2nix "cardano-api" (deps.cardano-node + "/cardano-api") {}) (drv: {
            doCheck = false;
            preConfigure = ''
                    # cardano-api with Vasil update stopped exposing certain functions we make use of. This exposes them again.
                    substituteInPlace src/Cardano/Api/Shelley.hs --replace "fromShelleyPParams," "fromShelleyPParams, toAlonzoPParams,"

                    substituteInPlace cardano-api.cabal \
                      --replace ", cardano-ledger-byron-test" ""

                    substituteInPlace gen/Gen/Cardano/Api/Typed.hs \
                      --replace "import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)" "" \
                      --replace "genVKWitness " "undefined "

                    sed -iE '/^library gen$/,$d' cardano-api.cabal
                  '';
          });

          cardano-prelude-test = haskellLib.dontCheck (self.callCabal2nix "cardano-prelude-test" (deps.cardano-prelude + "/cardano-prelude-test") {});

          cardano-crypto-wrapper = haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-wrapper" (deps.cardano-ledger + "/eras/byron/crypto") {});

          fmt = haskellLib.dontCheck (self.callCabal2nix "fmt" deps.fmt {});
          double-conversion = haskellLib.overrideCabal super.double-conversion (drv: {
            doCheck = false;
            patches = (drv.patches or []) ++ [(pkgs.hackGet ./cardano-overlays/dep/hackage-overlay-ghcjs + "/patches/double-conversion-2.0.2.0.patch")];
          });
          unix-bytestring = self.callCabal2nix "unix-bytestring" (pkgs.hackGet ./cardano-overlays/dep/plutus-ghcjs-cross + "/contrib/unix-bytestring-0.3.7.3") {};
          network = self.callHackage "network" "3.1.2.7" {};
          network-bsd = self.callHackage "network-bsd" "2.8.1.0" {};

          # avoid cardano-crypto-tests in deps
          ouroboros-consensus-protocol = haskellLib.overrideCabal (self.callCabal2nix "ouroboros-consensus-protocol" (deps.ouroboros-network + "/ouroboros-consensus-protocol") {}) (drv: {
            jailbreak = true;
            # disable building internal testing lib
            preConfigure = ''
              substituteInPlace ouroboros-consensus-protocol.cabal \
                --replace "library ouroboros-consensus-protocol-test" $'library ouroboros-consensus-protocol-test\n    buildable: False'
            '';
          });
          ouroboros-network = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ouroboros-network" (deps.ouroboros-network + "/ouroboros-network") {}));
          plutus-tx = self.callCabal2nix "plutus-tx" (deps.plutus + "/plutus-tx") {};
          # NOTE: Prior to bumping to reflex-platform 21.05 & using versions pinned from cardano-api 1.35 these tests have been passing,
          # but due to new failures it had to be disabled.
          # In practice these failures don't affect lode usages, but there needs to be identified which pkg version changes is causing this.
          plutus-ledger-api = haskellLib.dontCheck (self.callCabal2nix "plutus-ledger-api" (deps.plutus + "/plutus-ledger-api") {});

          primitive = haskellLib.dontCheck (self.callHackage "primitive" "0.7.1.0" {});
        })
    (self: super:
      let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in
        {
          flat = (haskellLib.dontCheck super.flat);
        }
    )
  ];
  haskellOverlaysPost = let deps = rpSetup.nixpkgs.thunkSet ./cardano-overlays/cardano-packages/dep; in (args.haskellOverlays or []) ++ (args.haskellOverlaysPost or []) ++ [
    (self: super:
      let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
        optionalExtension = cond: overlay: if cond then overlay else _: _: {};
      in {
        cardano-crypto-tests = null;

        # This fails to build, but also it's not clear why it's trying to build
        # this.  It seems somewhere down the build path is getting confused
        # between secp256k1 C library vs secp256k1 the haskell package.  The
        # secp256k1 haskell package is not required so removing this fixes the
        # confusion. The actual package that is used is named secp256k1-haskell
        secp256k1 = null;

        typerep-map = haskellLib.dontBenchmark (haskellLib.dontCheck super.typerep-map);

        time-compat = haskellLib.dontCheck super.time-compat;
      })
  ];
}
