let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "fc40379cd0edda603f4355bd67df415d52e402ff";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in {
    # Rib library source to use
    rib ? builtins.fetchTarball "https://github.com/JonathanReeve/rib/archive/${ribRevision}.tar.gz"
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
