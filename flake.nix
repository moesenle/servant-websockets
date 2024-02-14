{
  description = "Servant WebSocket";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";

    lint-utils = {
      url = "git+https://gitlab.nixica.dev/nix/lint-utils.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    inputs@
    { self
    , flake-utils
    , lint-utils
    , horizon-platform
    , nixpkgs
    , ...
    }: with builtins; let
      inherit (inputs.nixpkgs) lib;
      onlyHaskell = fs2source
        (fs.union
          (onlyExts-fs [ "cabal" "hs" "project" ] ./.)
          ./LICENSE # horizon requires this file to build
        );
      fs = lib.fileset;
      fs2source = fs': path: fs.toSource { root = path; fileset = fs'; };
      onlyExts-fs = exts: fs.fileFilter (f: foldl' lib.or false (map f.hasExt exts));
      onlyExts = exts: path: fs2source (onlyExts-fs exts path) path;

    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;

        legacyPackages =
          horizon-platform.legacyPackages.${system}.extend
            self.haskell-overlay.${system}.default;
      in
      rec {
        haskell-overlay.default = final: prev: {
          servant-websocket = final.callCabal2nix "servant-websocket" (onlyHaskell ./.) { };
        };
        packages.default = hlib.setBuildTarget legacyPackages.servant-websocket "lib:servant-websocket";
        devShells.default = legacyPackages.servant-websocket.env.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ (with pkgs; [
            cabal-install
            haskellPackages.cabal-fmt
            legacyPackages.ghcid
            legacyPackages.hlint
            legacyPackages.haskell-language-server
            nixpkgs-fmt
            stylish-haskell
          ]);

          shellHook =
            let
              instructions = ''
                ${write-descriptions general-functions}
              '';

              general-functions = {
                format = {
                  description = "format code";
                  body = ''
                    nix fmt
                    stylish-haskell -ir .
                    cabal-fmt -i *.cabal
                  '';
                };

                shelp = {
                  description = "show this message";
                  body = "echo ${lib.escapeShellArg instructions}";
                };
              };


              write-set = set: f: concatStringsSep "\n" (lib.mapAttrsToList f set);

              write-descriptions = set: ''
                ${write-set set (n: v: "  ${n}\t${v.description}")}'';
            in
            ''
              ${write-set (general-functions)
                  (n: v: ''
                     ${n}() {
                       ${v.body}
                     }
                   '')
              }

              shelp
            '';
        });

        checks = let lu = lint-utils.linters.${system}; in {
          cabal-formatting = lu.cabal-fmt { src = onlyExts [ "cabal" ] ./.; };
          haskell-warnings = lu.werror { pkg = legacyPackages.servant-websocket; };
          haskell-formatting = lu.stylish-haskell {
            src = fs2source
              (fs.union (onlyExts-fs [ "hs" ] ./.) ./.stylish-haskell.yaml)
              ./.;
          };
          haskell-linting = lu.hlint {
            src = fs2source
              (fs.union (onlyExts-fs [ "hs" ] ./.) ./.hlint.yaml)
              ./.;
          };

          nix-formatting = lu.nixpkgs-fmt { src = onlyExts [ "nix" ] ./.; };

        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
