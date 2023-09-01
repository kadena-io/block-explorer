let
  obelisk = builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/obelisk/archive/7ad33cbe3e84b209e83c505ce25486445bbd602e.tar.gz";
    sha256 = "sha256:0dlk8y6rxc87crw7764zq2py7nqn38lw496ca1y893m9gdq8qdkz";
  };
  kpkgs = builtins.fetchTarball {
    url = "https://github.com/kadena-io/kpkgs/archive/a71c4263c0f1f53935d4ed9b642a672074445494.tar.gz";
    sha256 = "sha256:1mccc2bl9k2a15fkj7jg48g2lmw2zwwbxwiapl6aklkvs4j7qinb";
  };
  reflex-platform = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/efc6d923c633207d18bd4d8cae3e20110a377864.tar.gz";
    sha256 = "sha256:121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
  };
  nixpkgs_1 = builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/nixpkgs/archive/a5cc7b77c090ede3ac380962ea876b83d847592c.tar.gz";
    sha256 = "sha256:0mfrfrgkc8bfc3hcnbzqmab021i3w27mczak99ab4ca154ml297i";
  };
  gitignore = builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore.nix/archive/7415c4feb127845553943a3856cbc5cb967ee5e0.tar.gz";
    sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };
  reflex-dom = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-dom/archive/02c61ecff6ed6e1bda4a68b24d6c2e04c70a31e2.tar.gz";
    sha256 = "sha256:1m54blk368n5ljj1iyicas2ksl1f0x7i5gkqpy1l6yjq3f8r9awy";
  };
  reflex-frp = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex/archive/3369fceadb132a980ab1ccbed7471ba36a9ef642.tar.gz";
    sha256 = "sha256:0wy41ya0g6klw7ps8ki2cqb9mm1y4d21jm98dd5niahbdldzkhl2";
  };
  nix-thunk = builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/nix-thunk/archive/bab7329163fce579eaa9cfba67a4851ab806b76f.tar.gz";
    sha256 = "sha256:0wn96xn6prjzcsh4n8p1n40wi8la53ym5h2frlqbfzas7isxwygg";
  };
  chainweb-api = builtins.fetchTarball {
    url = "https://github.com/kadena-io/chainweb-api/archive/00650534d4b3065342207a732131c88519209177.tar.gz";
    sha256 = "sha256:0lmrk9plhp4x8g1xczs4f8hld9arglvfsvbkcj9xf82sjs3cj3sm";
  };
  pact = builtins.fetchTarball {
    url = "https://github.com/kadena-io/pact/archive/d4452cbf7cbf589be294ec0980bd44e97edb4729.tar.gz";
    sha256 = "sha256:0qk8hfpgcwg6s6ny71hnk1w4pwlrcy4bbba2riwmiiha7bapj3n2";
  };
in [
  obelisk
  kpkgs
  reflex-platform
  nixpkgs_1
  gitignore
  reflex-dom
  reflex-frp
  nix-thunk
  chainweb-api
  pact
]