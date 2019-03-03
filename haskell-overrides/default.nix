{ pkgs, ... }:

{ additionalOverrides ? (self: super: {}) }:

let

  sources = import ../sources.nix;

  inherit (pkgs.haskell.lib) overrideCabal;

in

  with import ../common.nix pkgs;

{

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {

      # A hack to make certain `nix-shell` IFD sources and deps survive GC. See `sources.nix` for details.
      haskell-prevent-ifd-gc = with pkgs; []
        ++ buildPackages.cabal2nix.all ++ super.cabal2nix.all # unused, but will be in the new Nixpkgs
        ++ buildPackages.haskellPackages.cabal2nix.all;

      # A substitute prelude is trickier and has to be visible very
      # early. It can’t just be in `cabal.project`.
      prelude = self.callCabal2nix "prelude" ../prelude {};

      # TODO: remove after updating Nixpkgs.
      stm-containers = overrideCabal (self.callCabal2nix "stm-containers" sources.stm-containers {})
        (drv: { doCheck = false; });

    } // (additionalOverrides self super);
  };

}
