{ haskellLib, pkgs, lib }:

let deps = pkgs.thunkSet ./cardano-packages/dep;

in self: super: {
  ghcjs-base = self.callCabal2nix "ghcjs-base" deps.ghcjs-base {};

  # Force secp256k1 lib from host
  secp256k1-haskell = haskellLib.overrideCabal super.secp256k1-haskell (drv: {
    pkg-configDepends = [ pkgs.buildPackages.buildPackages.secp256k1 ];
  });

  # add missing text-short dep
  quickcheck-instances = haskellLib.addBuildDepend super.quickcheck-instances self.text-short;

  # this ensures same libsodium as ghc is used
  # cardano-crypto-class = haskellLib.overrideCabal (super.cardano-crypto-class.override { libsodium = libsodium-vrf; }) (drv: {
  #  buildTools = [pkgs.pkg-config];
  # });
  # this ensures same libsodium as ghc is used
  # cardano-crypto-praos = haskellLib.overrideCabal (super.cardano-crypto-praos.override { libsodium = libsodium-vrf; }) (drv: {
  #  buildTools = [pkgs.pkg-config];
  # });

  io-classes = haskellLib.overrideCabal super.io-classes (drv: {
    preConfigure = ''
      substituteInPlace src/Control/Monad/Class/MonadTimer.hs \
        --replace "#if defined(mingw32_HOST_OS)" "#if defined(mingw32_HOST_OS) || defined(__GHCJS__)"
    '';
  });

  # TODO: upstream to reflex-platform ghc810 branch
  network = haskellLib.overrideCabal super.network (drv: {
    doCheck = false;
    doHoogle = false;
    # avoid custom Setup.hs which has trouble configuring the pkg
    preCompileBuildDriver = "rm Setup.hs";
    preConfigure = "./configure";
    # TODO: correctly include generated HsNetworkConfig.h in pkg lib dir
    # preFixup = ''
    #  cp include/HsNetworkConfig.* $prefix/lib/ghcjs-8.10.7/js-ghcjs-ghcjs-8.10.7-ghc8_10_7/network-3.1.2.1-Bq1IGsPSGY2B7ccccStsE/include/
    # '';
  });

  # ghcjs build fixes
  terminal-size = self.callCabal2nix "terminal-size" (pkgs.hackGet ./dep/terminal-size) {};
  unix-bytestring = self.callCabal2nix "unix-bytestring" (pkgs.hackGet ./dep/plutus-ghcjs-cross + "/contrib/unix-bytestring-0.3.7.3") {};

  # for fixing 32-bit build
  # plutus-core = haskellLib.overrideCabal super.plutus-core (drv: {
  #   doCheck = false;
  #   doHaddock = false;
  #   doBenchmark = false;
  #   preConfigure = ''
  #     substituteInPlace plutus-core/src/PlutusCore/Evaluation/Machine/ExMemory.hs \
  #       --replace "WORD_SIZE_IN_BITS < 64" "0"
  #   '';
  # });

  # help find the plutus-core internal lib; Also due to there being changes for ghcjs on plutus-core the package-id is different than the ghc version
  plutus-tx = haskellLib.overrideCabal super.plutus-tx (drv: {
    # NOTE: see link for details
    # https://3.basecamp.com/4757487/buckets/24531883/messages/5274529248
    configureFlags = [ "--dependency=plutus-core:plutus-core-testlib=plutus-core-1.0.0.0-APFc5Xf8gyKEoZsNwPv0Bi-plutus-core-testlib" ];
  });

  # cryptonite with entropy support for ghcjs.
  cryptonite = self.callCabal2nix "cryptonite" (pkgs.hackGet ./dep/cryptonite-ghcjs) {};

  # this is just to force entropy to be generated properly for ghcjs.
  entropy = self.callHackage "entropy" "0.4.1.6" {};

  # Allow building with ghcjs, which is strictly disabled in the cabal file
  foundation = haskellLib.overrideCabal super.foundation (drv: {
    preConfigure = ''
      substituteInPlace foundation.cabal --replace " || impl(ghcjs -any)" ""
    '';
  });

  digest = haskellLib.overrideCabal super.digest (drv: {
    librarySystemDepends = [ pkgs.zlib ];
  });

  # cardano-addresses
  cardano-addresses = haskellLib.dontCheck super.cardano-addresses;
  cardano-addresses-jsbits = let
    jsbits = pkgs.runCommand "cardano-addresses-jsbits" {} ''
      script=$(mktemp -d)
      cp -r ${deps.cardano-addresses + "/jsbits/emscripten"}/* $script
      ln -s ${pkgs.srcOnly {name = "cryptonite-src"; src = self.cryptonite.src;}}/cbits $script/cryptonite
      ln -s ${pkgs.srcOnly {name = "cardano-crypto-src"; src = deps.cardano-crypto;}}/cbits $script/cardano-crypto
      patchShebangs $script/build.sh
      (cd $script && PATH=${
          # The extra buildPackages here is for closurecompiler.
          # Without it we get `unknown emulation for platform: js-unknown-ghcjs` errors.
          lib.makeBinPath (with pkgs.buildPackages.buildPackages;
            [emscripten closurecompiler coreutils])
        }:$PATH ./build.sh)
      mkdir -p $out
      cp $script/cardano-crypto.js $out
    '';
    addJsbits = ''
      mkdir -p jsbits
      cp ${jsbits}/* jsbits
    '';
  in haskellLib.overrideCabal (self.callCabal2nixWithOptions "cardano-addresses-jsbits" (deps.cardano-addresses + "/jsbits") "--no-hpack" {}) (drv: {
    preConfigure = ''
      ${addJsbits}
      substituteInPlace cardano-addresses-jsbits.cabal --replace "ghc-options: jsbits/cardano-crypto.js" ""
    '';
  });
}
