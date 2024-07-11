{
  description = "Hakell library for the sqlcommenter spec.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs @ {
    # devenv-root,
    nixpkgs,
    devenv,
    flake-utils,
    ...
  }: let
    inherit (nixpkgs) lib;
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    pre-commit-hooks = {
      # General hooks
      end-of-file-fixer = {
        enable = true;
        excludes = [
          ".*\\.l?hs$"
        ];
      };
      # Nix hooks
      alejandra.enable = true;
      deadnix.enable = true;
      # Haskell hooks
      fourmolu = {
        enable = true;
      };
      hpack.enable = true;
    };
  in
    {
      lib = {
        haskellOverlay = final: _: {
          sqlcommenter = final.callCabal2nix "sqlcommenter" ./. {};
        };
      };
    }
    // flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {inherit system;};

      mkShellForGHC = ghcVersion: let
        myHaskellPackages = pkgs.haskell.packages.${ghcVersion}.extend (final: _: {
          sqlcommenter = final.callCabal2nix "sqlcommenter" ./. {};
        });
      in
        devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({...}: {
              packages = with pkgs; [
                ghciwatch
                hpack
              ];

              dotenv.enable = true;

              languages.haskell = {
                enable = true;
                package = myHaskellPackages.ghc.withHoogle (
                  hpkgs:
                    hpkgs.sqlcommenter.getBuildInputs.haskellBuildInputs
                );
              };

              pre-commit.hooks = pre-commit-hooks;
            })
          ];
        };
    in {
      packages = {
        # devenv-up = self.devShells.${system}.default.config.procfileScript;
      };

      devShells = rec {
        default = mkShellForGHC "ghc96";
      };

      checks = {
        pre-commit-check = devenv.inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = pre-commit-hooks;
        };
      };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.garnix.io"
      "https://devenv.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
    allow-import-from-derivation = "true";
  };
}
