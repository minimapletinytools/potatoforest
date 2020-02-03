# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    potatoforest = ./potatoforest;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["potatoforest" "frontend"];
    ghcjs = ["potatoforest" "frontend"];
  };
})
