
# (import
#   (
#     let
#       lock = builtins.fromJSON (builtins.readFile ./flake.lock);
#     in
#     fetchTarball {
#       url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
#       sha256 = lock.nodes.flake-compat.locked.narHash;
#     }
#   )
#   {
#     src = ./.;
#   }).defaultNix

let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "3f790f22e477d4a833945a9ed4d6e04adbc34747";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in {
    # Rib library source to use
    rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
    # Cabal project root
  , root ? gitignoreSource ./.
    # Cabal project name
  , name ? "jonreevecom"
  , ...
}:

let
  source-overrides = {
    # with-utf8 = builtins.fetchTarball "https://github.com/serokell/haskell-with-utf8/archive/v1.0.0.0.tar.gz";
  };
in import rib {
  inherit root name source-overrides;
}
