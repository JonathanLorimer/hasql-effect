{
  description = "hasql-effect";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ] (system:
        function rec {
          inherit system;
          compilerVersion = "ghc982";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev:
              with pkgs.haskell.lib; {
                hasql-effect = hfinal.callCabal2nix "hasql-effect" ./. {};
                hasql = dontCheck hprev.hasql_1_8;
                hasql-pool = dontCheck hprev.hasql-pool_1_2_0_2;
                hasql-transaction = dontCheck hprev.hasql-transaction_1_1_1_2;
                postgresql-binary = dontCheck hprev.postgresql-binary_0_14;
                postgresql-libpq = dontCheck hprev.postgresql-libpq_0_10_1_0;
                # tasty = hprev.tasty_1_5_1;
                # time-compat = hprev.time-compat_1_9_7;
              };
          };
        });
  in {
    # nix fmt
    formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

    # nix develop
    devShell = forAllSystems ({
      hsPkgs,
      pkgs,
      ...
    }:
      hsPkgs.shellFor {
        # withHoogle = true;
        packages = p: [
          p.hasql-effect
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            haskellPackages.ghcid
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      });

    # nix build
    packages = forAllSystems ({hsPkgs, ...}: {
      hasql-effect = hsPkgs.hasql-effect;
      default = hsPkgs.hasql-effect;
    });

    # You can't build the hasql-effect package as a check because of IFD in cabal2nix
    checks = {};

    # nix run
    apps = forAllSystems ({system, ...}: {
      hasql-effect = {
        type = "app";
        program = "${self.packages.${system}.hasql-effect}/bin/hasql-effect";
      };
      default = self.apps.${system}.hasql-effect;
    });
  };
}
