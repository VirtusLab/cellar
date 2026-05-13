{
  description = "Look up the public API of any JVM dependency from the terminal";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      version = "0.1.0-M8";

      # Per-platform release artifact metadata
      platforms = {
        x86_64-linux = {
          archive = "cellar-${version}-linux-x86_64.tar.gz";
          hash = "sha256-JWb9WTzbWSz1V7Tyu2sgwuZNaVFhIWZz/GkpVjpZwtw=";
        };
        aarch64-linux = {
          archive = "cellar-${version}-linux-aarch64.tar.gz";
          hash = "sha256-7+pEYlTMbQlZFoDSnrfSherq1gxjNTBLkbgO7vyQke8=";
        };
        x86_64-darwin = {
          archive = "cellar-${version}-macos-x86_64.tar.gz";
          hash = "sha256-YyC2+e3NtDmTZyliKH3rmllYUd//EewOKCMROgWr2TI=";
        };
        aarch64-darwin = {
          archive = "cellar-${version}-macos-arm64.tar.gz";
          hash = "sha256-Vlzv9nXQLQEKRpuIY8LZAWIg+nKadLhBDwiOVw6QN9w=";
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

      overlays.default = final: prev: {
        cellar = self.packages.${final.system}.default;
      };
    };
}
