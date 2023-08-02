{}:
let
  project = import ./. {};
  pkgs = project.obelisk.nixpkgs;
in
  pkgs.mkShell {
    name = "hydra-pay";
    buildInputs = [
      project.cardano-node.cardano-cli
      project.cardano-node.cardano-node
      project.hydra.hydra-node.package.components.exes.hydra-node
      project.hydra.hydra-node.package.components.exes.hydra-tools
    ];
    inputsFrom = [
      (import ./. {}).shells.ghc
    ];
  }
