{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  packages = with pkgs; [
    llvmPackages_22.clang-unwrapped
    llvmPackages_22.lld
    llvmPackages_22.bintools-unwrapped
  ];
}
