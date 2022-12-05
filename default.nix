{ system ? builtins.currentSystem
# , rpSetup ? import ((import ./.obelisk/impl {}).path + "/dep/reflex-platform") {}
# , android-build ? false
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    terms.security.acme.acceptTerms = true;

    # Override reflex-platform-func in order to get access to haskellOverlaysPre
    # for adding mobile support.
  # reflex-platform-func = args@{...}:
  #   import ((import ./.obelisk/impl {}).path + "/dep/reflex-platform") (args // { __useNewerCompiler = true; });
  }
}:
with obelisk;
let
  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});
  deps = obelisk.nixpkgs.thunkSet ./dep;
  hydra-poc = import deps.hydra-poc {};
  cardano-node = import deps.cardano-node {};  

  pkgs = obelisk.nixpkgs;
  livedoc-devnet-script = pkgs.runCommand "livedoc-devnet-script" { } ''
    cp -r ${./livedoc-devnet} $out
  '';
in
project ./. ({ pkgs, ... }: let
  haskellLib = pkgs.haskell.lib;

in
{
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = foldExtensions [
    # cardano-libs-overlay
    (self: super: {
      reflex-gadt-api = self.callCabal2nix "reflex-gadt-api" deps.reflex-gadt-api {};
      # "reflex-gadt-api" "0.2.2.0" {};
      # base16 = haskellLib.markUnbroken (super.base16);
      # secp256k1 = haskellLib.markUnbroken (super.secp256k1);
      # text-conversions = self.callHackage "text-conversions" "0.3.1" {}; # compatible with base16-bytestring 1.x
      # base16-bytestring = self.callHackage "base16-bytestring" "1.0.1.0" {}; # for cardano-prelude
      /*
      # fin = haskellLib.doJailbreak super.fin;

      # base64-bytestring = self.callCabal2nix "base64-bytestring" deps.base64-bytestring {}; # 1.2.1.0
      unordered-containers = self.callHackage "unordered-containers" "0.2.14.0" {}; # for cardano-addresses
      # aeson = (haskellLib.dontCheck (self.callHackage "aeson" "1.5.6.0" {})); # 1.5.6.0
      scientific = (haskellLib.dontCheck (self.callHackage "scientific" "0.3.7.0" {}));
      # aeson = (haskellLib.dontCheck (haskellLib.doJailbreak (self.callHackage "aeson" "2.0.0.0" {})));
      network-mux = (haskellLib.doJailbreak super.network-mux);
      cborg = (haskellLib.dontCheck super.cborg);
      aeson-pretty = haskellLib.dontCheck ((self.callHackage "aeson-pretty" "0.8.9" {}));
      katip = haskellLib.dontCheck ((self.callHackage "katip" "0.8.7.0" {}));
      http2 = haskellLib.dontCheck (super.http2);
      // ekg-json = null;
      */
      # ekg-json = haskellLib.dontCheck (haskellLib.doJailbreak super.ekg-json);
      # ((self.callHackage "ekg-json" "0.8.9" {}));
      # deriving-aeson = (haskellLib.dontCheck (haskellLib.doJailbreak (self.callHackage "deriving-aeson" "0.2.7" {})));
      # haskellLib.doJailbreak (self.callCabal2nix "aeson" deps.aeson {}); # 1.5.6.0
      string-interpolate = haskellLib.doJailbreak (haskellLib.dontCheck super.string-interpolate);


      backend = haskellLib.overrideCabal super.backend (drv: {
        librarySystemDepends = (drv.librarySystemDepends or []) ++ [
          cardano-node.cardano-node
          cardano-node.cardano-cli
          hydra-poc.hsPkgs.hydra-node.components.exes.hydra-node
          hydra-poc.hsPkgs.hydra-node.components.exes.hydra-tools
          pkgs.jq
          pkgs.coreutils
          livedoc-devnet-script
        ];
      });
    })
  ];
})
