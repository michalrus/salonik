{ dontOptimize ? false
, newStyleBuild ? false # `true` makes sense for nix-shell development only…
}:

let

  sources = import ./sources.nix;

  nixpkgsSrc = sources.nixpkgsUnstable;
  nixpkgs = import nixpkgsSrc {};
  inherit (nixpkgs) pkgs;

in with import ./common.nix pkgs; let

  extraLibraries = [ ];

  optimizeCabalPackage = pkg:
    pkgs.haskell.lib.overrideCabal pkg (drv: {
      # For speed:
      doHaddock = false;
      # Tests are impure in a sense that they need a running Postgres.
      #   • `doCheck = false` would remove test deps from nix-shell → useless,
      #   • `checkPhase = ""` doesn’t override (see pkgs/development/haskell-modules/generic-builder.nix).
      checkPhase = ":";

      # Don’t lose time for compiling 2× when redeploying staging.
      enableLibraryProfiling = false;
      enableExecutableProfiling = false;

      configureFlags = (drv.configureFlags or []) ++ (if dontOptimize then dontOptimizeFlags.base else []);

      extraLibraries = (drv.extraLibraries or []) ++ extraLibraries;
    });

  inplace = rec {
    # A hack for cabal2nix not supporting new-style builds.
    # We need deps of all packages to transitively bubble up to Nix env.
    # See <https://github.com/NixOS/cabal2nix/issues/286>.

    # TODO: parse them directly from `cabal.project`
    packages = [
      ./salonik
    ];

    genPkgs = fun:
      pkgs.lib.listToAttrs (map (path:
        let name = baseNameOf path; in {name = name; value = fun name path; }
      ) packages);

    dummies = genPkgs (_: _: pkgs.runCommand "dummy" {} "mkdir $out");

    derivations = haskellPackages: genPkgs (name: path:
      let src = sourceByNegativeRegex (gitignoreToRegexes ./.gitignore) path;
      in optimizeCabalPackage (haskellPackages.callCabal2nix name src {}));

    dependencies = haskellPackages:
      pkgs.lib.concatMap (p: p.propagatedBuildInputs ++ p.buildInputs)
        (pkgs.lib.attrValues (derivations haskellPackages));
  };

  inherit
    (import ./haskell-overrides pkgs { additionalOverrides = self: super:
      if newStyleBuild then inplace.dummies else inplace.derivations self;
    }) haskellPackages;

  env = pkgs.lib.overrideDerivation

    (pkgs.haskell.lib.overrideCabal (inplace.derivations haskellPackages).salonik (drv: {
      libraryHaskellDepends = (drv.libraryHaskellDepends or [])
        ++ (if newStyleBuild then inplace.dependencies haskellPackages else []);
    })).env

  (oldAttrs: {
    buildInputs = extraLibraries ++
      (with haskellPackages; [ cabal-install hlint hindent stylish-haskell ]);

    # Caution: leave oldAttrs.shellHook in place, or HIE will break (just HIE!).
    shellHook = oldAttrs.shellHook + ''
      export NIX_PATH='nixpkgs=${nixpkgsSrc}'
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LC_ALL=

      cabal new-configure ${pkgs.lib.concatStringsSep " " (
        (if dontOptimize then dontOptimizeFlags.base ++ dontOptimizeFlags.shell else [])
        #++ ["--enable-tests"]
        )}
    '';
  });

  hie = (import sources.hie-nix { pkgs = import sources.hie-nix-nixpkgs {}; }).hie84;

in (inplace.derivations haskellPackages).salonik //
    { inherit env hie;
      inherit (haskellPackages) haskell-prevent-ifd-gc; }
