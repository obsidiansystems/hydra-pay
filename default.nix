{ system ? builtins.currentSystem
, android-build ? false
, cardanoProject ? import ./dep/cardano-project {
    inherit android-build system;
  }
}:
with cardanoProject;
with obelisk;
let
  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});
  deps = nixpkgs.thunkSet ./dep;
  flake-compat = import deps.flake-compat;
  hydra = (flake-compat {
    inherit system;
    src = deps.hydra;
  }).defaultNix.packages.${system};
  cardano-node = import deps.cardano-node {};
  pkgs = nixpkgs;
  livedoc-devnet-script = pkgs.runCommand "livedoc-devnet-script" { } ''
    cp -r ${./livedoc-devnet} $out
  '';
  p = project ./. ({ pkgs, ... }@args:
    let
      pd = cardanoProjectDef args;
      haskellLib = pkgs.haskell.lib;
    in
    pkgs.lib.recursiveUpdate pd {
      android.applicationId = "systems.obsidian.obelisk.examples.minimal";
      android.displayName = "Obelisk Minimal Example";
      ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
      ios.bundleName = "Obelisk Minimal Example";

      packages =
      {
        hydra-pay = ./hydra-pay;
        cardano-transaction = pkgs.hackGet ./dep/cardano-transaction-builder;
        bytestring-aeson-orphans = pkgs.hackGet ./dep/bytestring-aeson-orphans;
      };

      overrides = self: super: pd.overrides self super // {
        aeson-gadt-th = haskellLib.doJailbreak (haskellLib.disableCabalFlag (self.callCabal2nix "aeson-gadt-th" deps.aeson-gadt-th {}) "build-readme");
        reflex-gadt-api = haskellLib.doJailbreak (self.callCabal2nix "reflex-gadt-api" deps.reflex-gadt-api {});
        string-interpolate = haskellLib.doJailbreak (haskellLib.dontCheck super.string-interpolate);

        bytestring-aeson-orphans = haskellLib.doJailbreak super.bytestring-aeson-orphans;

        cardano-transaction = haskellLib.overrideCabal super.cardano-transaction (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [
            cardano-node.cardano-cli
          ];
        });

        hydra-pay = haskellLib.overrideCabal super.hydra-pay (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [
            cardano-node.cardano-node
            cardano-node.cardano-cli
            hydra.hydra-node.package.components.exes.hydra-node
            hydra.hydra-node.package.components.exes.hydra-tools
          ];
        });

        backend = haskellLib.overrideCabal super.backend (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [
            cardano-node.cardano-node
            cardano-node.cardano-cli
            hydra.hydra-node.package.components.exes.hydra-node
            hydra.hydra-node.package.components.exes.hydra-tools
            pkgs.jq
            pkgs.coreutils
            livedoc-devnet-script
          ];
        });
      };
    });

  hydra-pay-exe = pkgs.runCommandNoCC "hydra-pay" {} ''
    mkdir -p $out
    cp -r ${p.exe}/* $out/
    mv $out/backend $out/hydra-pay
  '';

in
p // {  exe = hydra-pay-exe; inherit cardano-node hydra deps;}
