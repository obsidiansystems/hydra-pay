# Overlay for having a patched cardano-wallet that only exposes modules
# that are relevant to Coin Selection and building with ghcjs.
{ haskellLib, pkgs
}:

let deps = pkgs.thunkSet ./dep;
in self: super: {

  cardano-wallet-coin-selection = (haskellLib.disableCabalFlag (self.callCabal2nix "cardano-wallet-coin-selection" (deps.cardano-wallet-coin-selection + "/lib/core") {}) "scrypt").override { scrypt = null; };
  cardano-wallet = self.callCabal2nix "cardano-wallet" (deps.cardano-wallet-coin-selection + "/lib/shelley") {};
  cardano-wallet-test-utils = self.callCabal2nix "cardano-wallet-test-utils" (deps.cardano-wallet-coin-selection + "/lib/test-utils") {};
  cardano-numeric = self.callCabal2nix "cardano-numeric" (deps.cardano-wallet-coin-selection + "/lib/numeric") {};
  text-class = self.callCabal2nix "text-class" (deps.cardano-wallet-coin-selection + "/lib/text-class") {};
  strict-non-empty-containers = self.callCabal2nix "strict-non-empty-containers" (deps.cardano-wallet-coin-selection + "/lib/strict-non-empty-containers") {};

}
