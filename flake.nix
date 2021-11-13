{
  description = "Jonreeve.com: Personal website for Jonathan Reeve.";
  inputs = {
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    rib = { url = "github:srid/rib"; flake = false; };
    # Pin nixpkgs
    # nixpkgs = { url = "github:NixOS/nixpkgs/5272327b81ed355bbed5659b8d303cf2979b6953"; flake = false; }; # 20.03
    # nixpkgs = { url = "github:NixOS/nixpkgs/cd63096d6d887d689543a0b97743d28995bc9bc3"; flake = false; }; # 20.09
    # nixpkgs = { url = "github:NixOS/nixpkgs/7e9b0dff974c89e070da1ad85713ff3c20b0ca97"; flake = false; }; # 21.05
  };
  outputs = { self, nixpkgs, ... }@inputs: {
    packages.x86_64-linux.jonreevecom =
      with import nixpkgs { system = "x86_64-linux"; config = { allowBroken = true; doCheck = false; }; };
      pkgs.haskellPackages.developPackage {
        root = ./.;
        name = "jonreevecom";
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
            [
              cabal-install
              cabal-fmt
              pkgs.nixpkgs-fmt
              ghcid
              ormolu
              haskell-language-server
              zlib
            ]);
        overrides = self: super: {
          mmark-ext = pkgs.haskell.lib.dontCheck super.mmark-ext;
          mmark = pkgs.haskell.lib.dontCheck super.mmark;
          pandoc-types = pkgs.haskell.lib.dontCheck super.pandoc-types;
          aeson = pkgs.haskell.lib.dontCheck super.aeson;
          lucid = pkgs.haskell.lib.dontCheck super.lucid;
        };
        source-overrides = {
          # mmark = pkgs.fetchFromGitHub { owner = "mmark-md";
          #                                repo = "mmark";
          #                                rev = "de705e2bdede5eb74c4905cca355202ac6203169";
          #                                sha256 = "lFXeSymzhNRIGIhKR1z0AtYWgVLVvdD+3RqUHfI4sRM=";
          #                                 };
          # mmark-ext = pkgs.fetchFromGitHub { owner = "mmark-md";
          #                                repo = "mmark-ext";
          #                                rev = "919c4175fa815746da33c5555d9243cd7a3c8850";
          #                                sha256 = "QciPFdvYA7FKynk67RnHgXrB+d2y3myXeJ7zB4Q0QB8=";
          #                                 };
          # hslua = pkgs.fetchFromGitHub { owner = "hslua";
          #                                repo = "hslua";
          #                                rev = "21e31353a94a7fae258177222380b1770cb5f408";
          #                                sha256 = "AAAAAAAAAIlsXUmEgJRzQl9zF2loIo0Un6Lj/hGK4AI=";
          #                              };
          # citeproc = pkgs.fetchFromGitHub { owner = "jgm";
          #                                 repo = "citeproc";
          #                                 rev = "75745c7586d3db459d4ac668ba566f416b107f91";
          #                                 sha256 = "AAAAAAAAAIlsXUmEgJRzQl9zF2loIo0Un6Lj/hGK4AI=";
          #                               };
          # pandoc = pkgs.fetchFromGitHub { owner = "jgm";
          #                                 repo = "pandoc";
          #                                 rev = "96a01451efd487d0f6a91a2785fd28be001a92bf";
          #                                 sha256 = "SMKB/ErAeIlsXUmEgJRzQl9zF2loIo0Un6Lj/hGK4AI=";
          #                               };
          hashable = pkgs.fetchFromGitHub { owner = "haskell-unordered-containers";
                                            repo = "hashable";
                                            rev = "10c9b57dba7a168d4e7ba8d601e6047a1a3bd47f";
                                            sha256 = "JiU/82LLl61ofJklGtT74I4KXBNWdjnKH5Hc5r29NO0=";
                                          };
          with-utf8 = pkgs.fetchFromGitHub { owner = "serokell";
                                             repo = "haskell-with-utf8";
                                             rev = "b60991303f9cf3f2891479c581549c2d54f45a0e";
                                             sha256 = "GRYyW8oi4A8xGcZ4FeZMhlSDOy4LIVEC/Vch8DvSgAw=";
                                           };
          # lucid = pkgs.fetchFromGitHub { owner = "chrisdone";
          #                                repo = "lucid";
          #                                rev = "5718bbab42590ad61b4c4b7f52cd2c99a276823a";
          #                                sha256 = "n9DFjx1L4BhvEKYBxHirtzq0KExOM4JRaWWQ1dB6RmA=";
          #                              };

        };
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.jonreevecom;
    devShell = true;
  };
}
