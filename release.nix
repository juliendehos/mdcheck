let
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz";
  pkgs = import (fetchTarball url) {};
in pkgs.callPackage ./default.nix {}

