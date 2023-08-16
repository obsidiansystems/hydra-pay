args@{ rpSetup, obelisk, ... }:
{
  haskellOverlaysPre = let deps = rpSetup.nixpkgs.thunkSet ./cardano-overlays/cardano-packages/dep; in [
    (self: super:
      let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in
        {
          time-compat = haskellLib.dontCheck super.time-compat;
          text-short = super.text-short;
          quickcheck-instances = haskellLib.doJailbreak super.quickcheck-instances;

          # https://github.com/input-output-hk/plutus/pull/4413
          # Stubs out unused functionality that breaks 32-bit build
          plutus-core = haskellLib.overrideCabal super.plutus-core (drv: {
            doCheck = false;
            doHaddock = false;
            doBenchmark = false;
            preConfigure = ''
              substituteInPlace plutus-core/src/GHC/Natural/Extras.hs \
                --replace "naturalToWord64Maybe n = intCastEq <$> naturalToWordMaybe n" "naturalToWord64Maybe _ = Nothing"

              substituteInPlace plutus-core/src/PlutusCore/Default/Universe.hs \
                --replace "intCastEq @Int @Int64" "const @Int64 @Int 0" \
                --replace "intCastEq @Int64 @Int" "const @Int @Int64 0"
            '';
          });
          plutus-tx = haskellLib.overrideCabal super.plutus-tx (drv: {
            configureFlags = (drv.configureFlags or []) ++ [
              # NOTE: see link for details
              # https://3.basecamp.com/4757487/buckets/24531883/messages/5274529248
              (if pkgs.stdenv.isDarwin
                then "--dependency=plutus-core:plutus-core-testlib=plutus-core-1.0.0.0-CG0eKwFyc06A6OsGzUyWcc-plutus-core-testlib"
                else "--dependency=plutus-core:plutus-core-testlib=plutus-core-1.0.0.0-ErmoQby7JQH3z6wFaYIRDg-plutus-core-testlib")
            ];
          });
        })
  ];
  haskellOverlaysPost = let deps = rpSetup.nixpkgs.thunkSet ./cardano-overlays/cardano-packages/dep; in (args.haskellOverlays or []) ++ (args.haskellOverlaysPost or []) ++ [
    (self: super:
      let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in
      {
        time-compat = haskellLib.dontCheck super.time-compat;
      })
  ];
}
