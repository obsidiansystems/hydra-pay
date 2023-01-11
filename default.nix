{ system ? builtins.currentSystem
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
  p = project ./. ({ pkgs, ... }:
    let
      haskellLib = pkgs.haskell.lib;
    in
    {
      android.applicationId = "systems.obsidian.obelisk.examples.minimal";
      android.displayName = "Obelisk Minimal Example";
      ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
      ios.bundleName = "Obelisk Minimal Example";

      overrides = foldExtensions [
        (self: super: {
          reflex-gadt-api = self.callCabal2nix "reflex-gadt-api" deps.reflex-gadt-api {};
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
    });

  hydra-pay-exe = pkgs.runCommandNoCC "hydra-pay" {} ''
    mkdir -p $out/bin
    cp -r ${p.exe}/* $out/bin
    mv $out/bin/backend $out/bin/hydra-pay
  '';

in
p // {  exe = hydra-pay-exe; }
