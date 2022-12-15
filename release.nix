(builtins.mapAttrs (system: _: (import ./. { inherit system; }).exe)
  { "x86_64-linux" = null;
    "x86_64-darwin" = null;
  })
//
(let
  self = import ./. {};
  obelisk = import ./.obelisk/impl {
    system = builtins.currentSystem;
  };
  nixpkgs = obelisk.nixpkgs;
in
{
  dockerImage = args@{ version ? "latest", name ? "obsidiansys/hydra-pay" }:
    nixpkgs.dockerTools.buildImage ({
      name = name;
      tag = version;
      contents = [ nixpkgs.iana-etc
                   nixpkgs.cacert
                   nixpkgs.bashInteractive
                 ];
      keepContentsDirlinks = true;
      # User setup from https://unix.stackexchange.com/questions/723183/how-to-add-a-non-root-user-when-building-a-docker-image-with-nix
      runAsRoot = ''
          #!${nixpkgs.runtimeShell}
          ${nixpkgs.dockerTools.shadowSetup}
          groupadd -r hydrapay
          useradd -r -g hydrapay hydrapay
          mkdir -p /hydrapay
          ln -sft /hydrapay '${self.linuxExe}'/*
          chown -R hydrapay:hydrapay /hydrapay
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
        ];
        Expose = 8000;
        Entrypoint = ["/hydrapay/backend"];
        WorkingDir = "/hydrapay";
        User = "hydrapay:hydrapay";
      };
    });
})
