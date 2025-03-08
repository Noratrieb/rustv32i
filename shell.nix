{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ cmake ninja ];
  packages = with pkgs; [
    llvmPackages_18.clang-unwrapped
    llvmPackages_18.lld
    llvmPackages_18.bintools-unwrapped
  ];
}
