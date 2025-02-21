{
  description = "QDX Rust Crates";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-flake-parts.url = "github:talo/qdx-rust-flake-parts";
    rust-flake-parts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ flake-parts, rust-flake-parts, ... }:
    let
      version = "0.0.1";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ rust-flake-parts.flakeModule ];
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      perSystem =
        { self', pkgs, ... }:
        {
          commonCraneArgs = {
            src = ./.;
            extraFileTypesRegex = ".*pdb|.*sdf|.*json";
            buildInputs = [
              pkgs.cargo-sort
              pkgs.cargo-sweep
              pkgs.cargo-machete
              pkgs.cargo-deny
            ];
          };
          craneProjects.rex = {
            inherit version;
          };
          packages.default = self'.packages.rex;
        };
    };
}
