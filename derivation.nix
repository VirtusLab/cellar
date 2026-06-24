{ scala-cli-nix, version }:

scala-cli-nix.buildCoursierApp {
  pname = "cellar";
  inherit version;
  lockFile = ./scala.lock.json;
  mainClass = "cellar.cli.CellarApp";
}
