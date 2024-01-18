let
  fromDep = depName:
    let dep = builtins.fromJSON (builtins.readFile ../deps/${depName}/github.json);
    in builtins.fetchTarball {
      url = "https://github.com/${dep.owner}/${dep.repo}/archive/${dep.rev}.tar.gz";
      sha256 = dep.sha256;
    };
  obelisk = builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/obelisk/archive/7ad33cbe3e84b209e83c505ce25486445bbd602e.tar.gz";
    sha256 = "sha256:0dlk8y6rxc87crw7764zq2py7nqn38lw496ca1y893m9gdq8qdkz";
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
in [
  obelisk
  (fromDep "kpkgs")
  reflex-platform
  nixpkgs_1
  gitignore
  reflex-dom
  reflex-frp
  (fromDep "nix-thunk")
  (fromDep "chainweb-api")
  (fromDep "pact")
]