#
# For all IFD (import-for-derivation) sources, add them here. They
# will be manually cached as per
# <https://github.com/NixOS/nix/issues/954#issuecomment-365261148>. Otherwise,
# Nix GC would delete them and they’d have to be re-downloaded
# afterwards. — @michalrus
#

let

  fetchNixpkgs = metasrc: {
    outPath = import ./fetchNixpkgs.nix metasrc;
    inherit metasrc;
  };

  nixpkgsUnstable = fetchNixpkgs {
    rev = "8395f9aa85e621c076334a67d814de8221ce7983";
    sha256 = "04b2gyji9yz9429cy7ah3yidh4clplfgd4smkd0hx06g5n5v790g";
  };

  inherit (import nixpkgsUnstable {}) fetchgit fetchFromGitHub;

in rec {

  inherit nixpkgsUnstable;

  hie-nix = fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "e3113da93b479bec3046e67c0123860732335dd9";
    sha256 = "05rkzjvzywsg66iafm84xgjlkf27yfbagrdcb8sc9fd59hrzyiqk";
  };

  hie-nix-nixpkgs = import "${hie-nix}/fetch-nixpkgs.nix";

  stm-containers = fetchFromGitHub {
    owner = "nikita-volkov"; repo = "stm-containers";
    rev = "bda03ee52ee2f0f7b01ba4271089ff1d72a7eb67";
    sha256 = "1jlmxgf8davjs94f06im8kf5p3gcshh90yi317y0b7gsk2d93a7x";
  };

}
