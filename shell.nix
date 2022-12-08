{}:
let pkgs = (import ./. {}).obelisk.nixpkgs;
in
  pkgs.mkShell {
    name = "hydra-pay";
    buildInputs = [
    ];
    inputsFrom = [
      (import ./. {}).shells.ghc
    ];
  }
