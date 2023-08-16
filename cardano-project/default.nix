{ system ? builtins.currentSystem
# , android-build ? false
, rpSetup ? import ((import ./.obelisk/impl {}).path + "/dep/reflex-platform") {}

, obelisk-src ? rpSetup.hackGet ./.obelisk/impl
  # patched to expose ghcAarch64 package set to projects.
, obelisk-patched ? rpSetup.nixpkgs.applyPatches {
    name = "obelisk-patched";
    src = obelisk-src;
  }
, obelisk ? import obelisk-patched {
    inherit system;
    iosSdkVersion = "16.1";

    # Use ghc 8.10.7 as the compiler.
    useGHC810 = true;

    reflex-platform-func = args@{...}:
      let
        overlayInputs = args // { inherit rpSetup obelisk; };
        baseOverlays = import ./base.nix overlayInputs;
        extraOverlays = import ./extras.nix overlayInputs;
      in
      import ((import ./.obelisk/impl {}).path + "/dep/reflex-platform")
        (args // {
          __useNewerCompiler = true;
          useTextJSString = false;
          nixpkgsOverlays = let deps = rpSetup.nixpkgs.thunkSet ./cardano-overlays/dep; in (args.nixpkgsOverlays or []) ++ [
            (self: super: let
              haskellLib = self.callPackage ({pkgs}: pkgs.haskell.lib) {};
              in {
              time-compat = haskellLib.dontCheck super.time-compat;
              libsodium-vrf = self.callPackage (deps.iohk-nix + "/overlays/crypto/libsodium.nix") {};
              secp256k1 = super.secp256k1.overrideAttrs (drv: {
                name = "secp256k1-unstable-2022-02-06";
                src = self.fetchFromGitHub {
                  owner = "bitcoin-core";
                  repo = "secp256k1";
                  rev = "5dcc6f8dbdb1850570919fc9942d22f728dbc0af";
                  sha256 = "x9qG2S6tBSRseWaFIN9N2fRpY1vkv8idT3d3rfJnmaU=";
                };
                configureFlags = (drv.configureFlags or []) ++ [
                  "--enable-module-ecdh"
                  "--enable-experimental"
                  "--enable-module-schnorrsig"
                ];
              });
            })
          ];
          haskellOverlaysPre = baseOverlays.haskellOverlaysPre ++ extraOverlays.haskellOverlaysPre;
          haskellOverlaysPost = baseOverlays.haskellOverlaysPost ++ extraOverlays.haskellOverlaysPost;
        });
  }
# Specify a different configuration directory. Used for production extension builds
, configRoot ? ./config
# Version information: important for app store releases. Can be overridden to, e.g., provide special version numbers for test builds.
, version ? builtins.readFile (configRoot + "/common/version")
}:
with obelisk;
let

pkgs = obelisk.nixpkgs;
hackGet = pkgs.hackGet;
deps = obelisk.nixpkgs.thunkSet ./dep;
deps' = obelisk.nixpkgs.thunkSet ./cardano-overlays/cardano-packages/dep;
cardano-keychain-src = deps.cardano-keychain;
foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

# An obelisk project definition with cardano dependencies ready to build on
# browser, android & iOS.
cardanoProjectDef = ({ nixpkgs, pkgs, hackGet, ... }@args: let
  haskellLib = pkgs.haskell.lib;
  cardanoOverlays = import ./cardano-overlays { inherit haskellLib pkgs lib; };

  # NOTE(skylar): The overrides here have to happen in "user space" (the space where obelisk overrides exist)
  # and cannot go into the overlays we give to haskellOverlaysPre and haskellOverlaysPost as some packages/libraries
  # need to resolve first to then be overriden.
  #
  # An example of this is we need to hack around the libsodium dependency when compiling libraries for ghcjs as
  # libsodium _can't_ compile with ghcjs, but libraries that use it can just use the ghc library.
  #
  # If we try and resolve this in haskellOverlaysPre or haskellOverlaysPost we will be resolving it while assembling reflex platform which
  # will cause ghcjs to _always_ attempt to build libsodium.
  #
  # If you find your overrides not working, try putting them in userspace!
  userSpaceOverrides = (self: super: {
    time-compat = haskellLib.dontCheck super.time-compat;

    cardano-crypto-class = haskellLib.overrideCabal ((self.callCabal2nix "cardano-crypto-class" (deps'.cardano-base + "/cardano-crypto-class") {}).override { libsodium = pkgs.libsodium-vrf; }) (drv: {
      pkg-configDepends = (drv.pkg-configDepends or []) ++ [pkgs.secp256k1];
      buildTools = [ pkgs.pkg-config ];
    });

    cardano-crypto-praos = haskellLib.overrideCabal ((self.callCabal2nix "cardano-crypto-praos" (deps'.cardano-base + "/cardano-crypto-praos") {}).override { libsodium = pkgs.libsodium-vrf; }) (drv: {
      buildTools = [ pkgs.pkg-config ];
    });

    network = haskellLib.overrideCabal super.network (drv: {
      preCompileBuildDriver = "rm Setup.hs";
      preConfigure = "./configure";
      preFixup = let pkgs = self.callPackage ({ pkgs }: pkgs) {}; in
        if (super.ghc.isGhcjs or false) then
        ''
        cp include/HsNetworkConfig.* $prefix/lib/ghcjs-8.10.7/js-ghcjs-ghcjs-8.10.7-ghc8_10_7/$pkgId/include/
        ''
        else if (with pkgs.stdenv; (hostPlatform == buildPlatform && hostPlatform.isDarwin))
        then
        ''
        cp include/HsNetworkConfig.* $prefix/lib/ghc-8.10.7/x86_64-osx-ghc-8.10.7/$pkgId/include/
        ''
        else
        ''
        cp include/HsNetworkConfig.* $prefix/lib/ghc-8.10.7/x86_64-linux-ghc-8.10.7/$pkgId/include/
        '';
    });
  });
  rhyoliteOverrides = (pkgs.callPackage deps.rhyolite (args // { inherit obelisk; })).haskellOverrides;
  rhyoliteOverridesExcludedPkgs = ["bytestring-aeson-orphans"];
in {
  overrides = foldExtensions [
    (self: super: removeAttrs (rhyoliteOverrides self super) rhyoliteOverridesExcludedPkgs)
    cardanoOverlays.combined
    cardanoOverlays.cardanoWalletCoinSelection
    (self: super: {
      cardano-keychain = self.callCabal2nix "cardano-keychain" cardano-keychain-src {};
    })
    (userSpaceOverrides)
    (self: super: let pkgs' = self.callPackage ({ pkgs }: pkgs) {}; in {
      frontend-static-lib =
        if pkgs'.hostPlatform.isiOS or false
        then haskellLib.dontStrip (haskellLib.enableCabalFlag super.frontend "static-lib")
        else super.frontend;
      beam-core = self.callHackage "beam-core" "0.9.2.1" {};
      beam-migrate = self.callHackage "beam-migrate" "0.5.1.2" {}; # aeson 2 support
      beam-postgres = haskellLib.dontCheck (self.callHackage "beam-postgres" "0.5.2.1" {});
      dns-patterns = haskellLib.doJailbreak (self.callHackageDirect {
        pkg = "dns-patterns";
        ver = "0.1.1";
        sha256 = "0yffdskxpw4a4m6r1q4dnwhpcnaalcxwngssr6b0fwgd5xixm1y7";
      } {});
      hexstring = self.callCabal2nix "hexstring" deps.haskell-hexstring {};
      reflex-gadt-api = haskellLib.overrideCabal (self.callCabal2nix "reflex-gadt-api" (deps.reflex-gadt-api) {}) (drv: {
        jailbreak = true;
        # disable readme build which is broken.
        preConfigure = ''
          substituteInPlace reflex-gadt-api.cabal --replace "executable readme" $'executable readme\n    buildable:False'
        '';
      });

      map-syntax = haskellLib.doJailbreak super.map-syntax;
      xmlhtml = haskellLib.doJailbreak super.xmlhtml;
      hspec-webdriver = self.callCabal2nix "hspec-webdriver" deps.hspec-webdriver-clone {};
      websockets = haskellLib.doJailbreak (self.callHackage "websockets" "0.12.7.2" {});
      # avoid hlint from being built
      patch = haskellLib.dontCheck super.patch;
      reflex-dom-core = haskellLib.doJailbreak (haskellLib.dontCheck super.reflex-dom-core);
      # avoid hlint from being built
      reflex = haskellLib.dontCheck super.reflex;
      # override to fix android build as haskellLib.doJailbreak does not relax bound for deps under conditionals
      reflex-dom = haskellLib.overrideCabal (haskellLib.doJailbreak super.reflex-dom) (drv: {
        # expose MainWidget so we can write a custom initializer
        preConfigure = ''
          substituteInPlace reflex-dom.cabal \
            --replace "aeson >=1.4 && <1.6" "aeson"  \
            --replace "jsaddle-webkit2gtk >=0.9.6 && <0.10" "jsaddle-webkit2gtk" \
            --replace "other-modules:    Reflex.Dom.Android.MainWidget" "exposed-modules:    Reflex.Dom.Android.MainWidget"
        '';
      });
      # reflex = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "reflex" deps.reflex {}));
      commutative-semigroups = self.callCabal2nix "commutative-semigroups" deps.commutative-semigroups {};
      aeson-gadt-th = haskellLib.doJailbreak (self.callCabal2nix "aeson-gadt-th" deps.aeson-gadt-th {});
      deriving-compat = self.callHackage "deriving-compat" "0.6" {};
      vessel = haskellLib.doJailbreak (self.callCabal2nix "vessel" deps.vessel {});
      dependent-monoidal-map = haskellLib.doJailbreak super.dependent-monoidal-map;
      entropy = self.callCabal2nix "entropy" deps.entropy {};
      ghcjs-dom = self.callHackage "ghcjs-dom" "0.9.5.0" {};
      jsaddle-dom = haskellLib.doJailbreak (self.callHackage "jsaddle-dom" "0.9.5.0" {});
      ghcjs-dom-jsffi = self.callHackage "ghcjs-dom-jsffi" "0.9.5.0" {};
      ghcjs-dom-jsaddle = self.callHackage "ghcjs-dom-jsaddle" "0.9.5.0" {};
      qrcode-core = self.callCabal2nix "qrcode-core" (deps.qrcode + "/qrcode-core") {};
      qrcode-juicypixels = self.callCabal2nix "qrcode-juicypixels" (deps.qrcode + "/qrcode-juicypixels") {};
      JuicyPixels-ghcjs = self.callCabal2nix "JuicyPixels-ghcjs" deps."juicy.pixels.ghcjs" {};

      # hpack >:0
      tls = haskellLib.dontCheck super.tls;
      # Make internal directory lookup availabe in GHC in order to prefill
      # configs for background script in chrome extension which doesn't have
      # access to read their package contents
      obelisk-executable-config-lookup = if (super.ghc.isGhcjs or false) then super.obelisk-executable-config-lookup else haskellLib.overrideCabal super.obelisk-executable-config-lookup (drv: {
        # TODO: only apply for ghc
        preConfigure = ''
          substituteInPlace obelisk-executable-config-lookup.cabal \
            --replace "        other-modules: Obelisk.Configs.Internal.Directory" "        exposed-modules: Obelisk.Configs.Internal.Directory"
        '';
      });
      # Fix build for lens-5 th generated polykinded data type changes
      obelisk-frontend = haskellLib.overrideCabal super.obelisk-frontend (drv: {
        preConfigure = ''
          substituteInPlace src/Obelisk/Frontend.hs \
            --replace "makePrisms " "-- makePrisms" \
            --replace "import Data.Functor.Sum" ""
        '';
      });
      obelisk-asset-manifest = haskellLib.overrideCabal super.obelisk-asset-manifest (drv: {
        preConfigure =
          let pkgs = self.callPackage ({ pkgs }: pkgs) {};
          in with pkgs.stdenv;
          if (hostPlatform == buildPlatform && hostPlatform.isDarwin) # Only apply this for mobile iOS build
          then
            ''
              substituteInPlace src/Obelisk/Asset/TH.hs --replace 'staticPrefix = "/static"' 'staticPrefix = "static"'
            ''
          else drv.preConfigure or "";
      });
      # aeson 2 compatible version
      heist = haskellLib.dontCheck (self.callHackage "heist" "1.1.1.0" {});
      # aeson 2 support
      hoogle = self.callHackage "hoogle" "5.0.18.3" {};
      # tests fail to build due to missing aeson 2 support
      http-streams = haskellLib.dontCheck super.http-streams;
      # logging libs that aren't on hackage yet
      logging-effect-syslog = self.callCabal2nix "logging-effect-syslog" deps.logging-effect-syslog {};
      logging-effect-colors = self.callCabal2nix "logging-effect-colors" deps.logging-effect-colors {};

      postgresql-simple = haskellLib.doJailbreak (self.callHackage "postgresql-simple" "0.6.4" {});
      postgresql-lo-stream = haskellLib.doJailbreak (haskellLib.markUnbroken super.postgresql-lo-stream);
    })
  ];

  # android.runtimeSharedLibs = nixpkgs: [
  #   "${nixpkgs.libsodium-vrf}/lib/libsodium.so"
  #   "${nixpkgs.secp256k1}/lib/libsecp256k1.so"
  # ];
  #
  # android.permissions = ''
  #   <uses-permission android:name="android.permission.READ_CLIPBOARD" />
  #   <uses-permission android:name="android.permission.WRITE_CLIPBOARD" />
  # '';
  #
  # android.javaSources = activitySrc: mainWidgetSrc:
  #   [ ./android/javabits
  #     activitySrc
  #     (cardano-keychain-src + "/javabits")
  #   ] ;
  #
  # ios.overrideInfoPlist = super:
  #   let
  #     plistOverrides = {
  #       CFBundleIcons = {
  #         CFBundlePrimaryIcon = {
  #           CFBundleIconName = "AppIcon";
  #           CFBundleIconFiles = [
  #             "AppIcon60x60"
  #           ];
  #         };
  #       };
  #     };
  #     disableiPad = plist:
  #       builtins.removeAttrs plist ["UISupportedInterfaceOrientations~ipad" "CFBundleIcons~ipad"] // {
  #         UIDeviceFamily = [ 1 ];
  #       };
  #     forcePortrait = plist:
  #       plist // {
  #         UISupportedInterfaceOrientations = [
  #           "UIInterfaceOrientationPortrait"
  #         ];
  #       };
  #   in forcePortrait (disableiPad (super // plistOverrides));
});

in {
  # Expose project definition for other projects to consume.
  inherit cardanoProjectDef obelisk;
}
