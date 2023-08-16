{ haskellLib, pkgs, lib }:

let
  deps = pkgs.thunkSet ./dep;
in rec {

  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

  combined = self: super: foldExtensions [
    cardanoPackages
    (optionalExtension (!(super.ghc.isGhcjs or false)) ghc)
    (optionalExtension (super.ghc.isGhcjs or false) ghcjs)

    (optionalExtension (with pkgs.stdenv; !(super.ghc.isGhcjs or false) && hostPlatform != buildPlatform) loadSplices-8_10)

    # (optionalExtension (pkgs.stdenv.hostPlatform.useAndroidPrebuilt or false) android)
    (optionalExtension true android)
    (optionalExtension (pkgs.stdenv.hostPlatform.isiOS or false) ios)

  ] self super;

  cardanoPackages = import ./cardano-packages {
    inherit haskellLib pkgs lib;
  };

  cardanoWalletCoinSelection = import ./cardano-wallet-coin-selection {
    inherit haskellLib pkgs;
  };

  ghc = self: super: {
    # cardano-crypto-class = haskellLib.addPkgconfigDepend super.cardano-crypto-class libsodium-vrf;
    # cardano-crypto-praos = haskellLib.addPkgconfigDepend super.cardano-crypto-praos libsodium-vrf;
  };

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib pkgs lib;
  };

  loadSplices-8_10 = self: super: {
    flat = haskellLib.dontCheck super.flat;
  };

  android = self: super: {
    # this ensures same libsodium as ghc is used
    # cardano-crypto-class =
    #   haskellLib.overrideCabal (super.cardano-crypto-class.override { libsodium = libsodium-vrf; }) (drv: {
    #   buildTools = [pkgs.pkg-config];
    # });
    # # this ensures same libsodium as ghc is used
    # cardano-crypto-praos = haskellLib.overrideCabal (super.cardano-crypto-praos.override { libsodium = libsodium-vrf; }) (drv: {
    #   buildTools = [pkgs.pkg-config];
    # });

    terminal-size = self.callCabal2nix "terminal-size" (pkgs.hackGet ./dep/terminal-size) {};

  };

  ios = self: super: {

  };

}
