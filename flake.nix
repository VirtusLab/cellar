{
  description = "Look up the public API of any JVM dependency from the terminal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    scala-cli-nix = {
      url = "github:scala-nix/scala-cli-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, scala-cli-nix }:
    let
      version = "0.1.0-M9";
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = eachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ scala-cli-nix.overlays.default ];
          };
        in
        {
          default = pkgs.callPackage ./derivation.nix { inherit version; };
        }
      );

      overlays.default = final: prev: {
        cellar = self.packages.${final.system}.default;
      };
    };
}
