{ lib, ... }:

{

  # Tricks to make local incremental builds much faster, cf.
  # <https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/>.
  dontOptimizeFlags = {
    base = [
      "--disable-optimization"
      "--disable-library-profiling"
    ];
    shell = [
      "--disable-library-vanilla"
      "--enable-executable-dynamic"
      "--ghc-options='-j +RTS -A128m -n2m -RTS'"
    ];
  };

  # TODO: How to fully re-use recursive .gitignore’s? https://git.io/vSo80
  gitignoreToRegexes = gitignorePath:
    builtins.map (line: if lib.hasPrefix "/" line then line else ".*/" + line + "($|/.*)")
      (builtins.map (builtins.replaceStrings ["." "**" "*"] ["\\." ".*" "[^/]*"] )
         (builtins.map (lib.removeSuffix "/")
            (lib.filter (line: line != "" && !(lib.hasPrefix "#" line))
               (lib.splitString "\n" (builtins.readFile gitignorePath)))));

  sourceByNegativeRegex = regexes: src:
    builtins.filterSource (path: type:
       let relPath = lib.removePrefix (toString src) (toString path);
       in lib.all (re: builtins.match re relPath == null) regexes) src;

}
