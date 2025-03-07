{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  packages = with pkgs; [ llvmPackages_18.clang-unwrapped llvmPackages_18.lld ];
}
