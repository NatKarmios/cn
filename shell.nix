{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = with pkgs; [
    opam
    pkg-config
    gnumake
    gmp
    z3
    cvc5
    (pkgs.writeShellScriptBin "cn_" "opam exec -- dune exec -p cn --profile=dev -- cn $@")
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.gmp}/lib
    eval $(opam env)
  '';
}
