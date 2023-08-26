(builtins.mapAttrs (system: _: (import ./. { inherit system; }).hydra-pay)
  { "x86_64-linux" = null;
  })
//
(let
  self = import ./. {};
  obelisk = import ./.obelisk/impl {
    system = builtins.currentSystem;
  };
  nixpkgs = obelisk.nixpkgs;
  pkgs = import <nixpkgs> {};

  configs = pkgs.stdenv.mkDerivation
  {
    name = "configs";
    src = ./config;

    installPhase = ''
      mkdir -p $out
      cp -r * $out
    '';
  };
in
{
  dockerImage = args@{ version ? "latest", name ? "obsidiansys/hydra-pay" }:
    pkgs.dockerTools.buildImage ({
      name = name;
      tag = version;

      keepContentsDirlinks = true;

      copyToRoot = pkgs.buildEnv {
        name = "root";
        paths = [ self.hydra-pay nixpkgs.bashInteractive nixpkgs.iana-etc
                   nixpkgs.cacert];
        pathsToLink = [ "/bin" ];
      };

      runAsRoot = ''
        #!${nixpkgs.runtimeShell}
        ${nixpkgs.dockerTools.shadowSetup}
        mkdir -p hydra-pay/config
        ln -sft /hydra-pay/config '${configs}'/*
        groupadd -r hydra-pay
        useradd -r -g hydra-pay hydra-pay
        chown -R hydra-pay:hydra-pay /hydra-pay
      '';

      config = {
        Env = [
          ("PATH=" + builtins.concatStringsSep(":")(
            [
              "/hydrapay"
              "/bin"
            ]
            ++
            map (pkg: "${pkg}/bin") nixpkgs.stdenv.initialPath # put common tools in path so docker exec is useful
          ))
          "LANG=C.UTF-8"
          "NETWORK=preprod"
        ];

        Cmd = [ "sh" "-c" "/bin/hydra-pay instance $NETWORK" ];
        WorkingDir = "/hydra-pay";
        Expose = 8010;
        User = "hydra-pay:hydra-pay";
      };
    });
})
