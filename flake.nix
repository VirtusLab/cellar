{
  description = "Look up the public API of any JVM dependency from the terminal";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      version = "0.1.0-M13";

      # Per-platform release artifact metadata
      platforms = {
        x86_64-linux = {
          archive = "cellar-${version}-linux-x86_64.tar.gz";
          hash = "sha256-1dXu0bLGBFnSSZoNYcCJiyqGc0GlB4NFfLGvRm9td/Q=";
        };
        aarch64-linux = {
          archive = "cellar-${version}-linux-aarch64.tar.gz";
          hash = "sha256-Os8gIdAq1owlBXRaYr4nQnwHjxWM2LM0Rcew+Zfm/1A=";
        };
        x86_64-darwin = {
          archive = "cellar-${version}-macos-x86_64.tar.gz";
          hash = "sha256-0wZcjbK8HVZ8BNKmGAm+MqA0Rc9q+zCZctwJFTqZc1Y=";
        };
        aarch64-darwin = {
          archive = "cellar-${version}-macos-arm64.tar.gz";
          hash = "sha256-rBuQJzihoZ/uf7OE/MFfd8DyAmD7BH2hqsPzFiNVJI0=";
        };
      };

      eachSystem = nixpkgs.lib.genAttrs (builtins.attrNames platforms);
    in
    {
      packages = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          meta = platforms.${system};
          src = pkgs.fetchurl {
            url = "https://github.com/VirtusLab/cellar/releases/download/v${version}/${meta.archive}";
            hash = meta.hash;
          };
        in
        {
          default = pkgs.stdenv.mkDerivation {
            pname = "cellar";
            inherit version src;

            sourceRoot = ".";

            nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              pkgs.autoPatchelfHook
              pkgs.glibc
              pkgs.zlib
            ];

            unpackPhase = ''
              tar xzf $src
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp cellar $out/bin/cellar
              chmod +x $out/bin/cellar
            '';

            meta = with pkgs.lib; {
              description = "Look up the public API of any JVM dependency from the terminal";
              homepage = "https://github.com/VirtusLab/cellar";
              license = licenses.mpl20;
              platforms = [ system ];
              mainProgram = "cellar";
            };
          };

          # Install a locally-built binary into the nix profile.
          # Usage: ./mill cli.nativeImage && nix profile install --impure .#dev
          dev = let
            binary = builtins.path {
              path = builtins.toPath "${builtins.getEnv "PWD"}/out/cli/nativeImage.dest/native-executable";
              name = "cellar-native-executable";
            };
          in pkgs.stdenv.mkDerivation {
            pname = "cellar";
            version = "dev";
            dontUnpack = true;

            installPhase = ''
              mkdir -p $out/bin
              cp ${binary} $out/bin/cellar
              chmod +x $out/bin/cellar
            '';

            meta = with pkgs.lib; {
              description = "Look up the public API of any JVM dependency from the terminal (dev build)";
              homepage = "https://github.com/VirtusLab/cellar";
              license = licenses.mpl20;
              platforms = [ system ];
              mainProgram = "cellar";
            };
          };
        }
      );

      devShells = eachSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          # sbt and scala-cli back the project-aware E2E tests, which skip when
          # the corresponding build tool is missing from PATH.
          default = pkgs.mkShell {
            packages = [
              pkgs.temurin-bin-17
              pkgs.sbt
              pkgs.scala-cli
            ];
          };
        }
      );

      overlays.default = final: prev: {
        cellar = self.packages.${final.system}.default;
      };
    };
}
