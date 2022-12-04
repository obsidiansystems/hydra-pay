builtins.mapAttrs (system: _: (import ./. { inherit system; }).exe)
  { "x86_64-linux" = null;
    "x86_64-darwin" = null;
  }
