{
  description = "Jonreeve.com: Personal website for Jonathan Reeve.";
  inputs = {
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    rib = { url = "github:srid/rib?rev=3f790f22e477d4a833945a9ed4d6e04adbc34747"; flake = false; };
  };

  outputs =

    { self, nixpkgs, ribRevision, rib }: {

    packages.x86_64-linux.jonreevecom = {
      rib, root ? ./. , name ? "jonreevecom" , ...
    }:
      let
        source-overrides = {
          with-utf8 = builtins.fetchTarball "https://github.com/serokell/haskell-with-utf8/archive/v1.0.0.0.tar.gz";
        };
      in import rib {
        inherit root name source-overrides;
      };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.jonreevecom;

  };
}
