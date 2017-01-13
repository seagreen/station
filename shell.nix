with import <nixpkgs> {};

# (needed for testing only)
#
# b2sum isn't packaged for Nixpkgs yet.
# Binaries are provided at https://blake2.net/, so we don't even need to build it.
#
# Code based on here:
# https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/misc/adobe-reader
#
let b2sum = pkgs.stdenv.mkDerivation {
  src = pkgs.fetchzip {
    url = "https://blake2.net/b2sum-bin_20130305.zip";
    sha256 = "0p08m2a0c57pbkrky4gn7y0zlrkmmcg015lm28kl55bgrdrxp6ij";
    stripRoot=false;
  };
  name = "b2sum";
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup

    mkdir -p $out/bin
    cp $src/b2sum-amd64-linux $out/bin/b2sum
  '';
};
in

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [

    zlib

    # For the library

    pcre

    # For testing only

    b2sum
    jq

  ];
}
