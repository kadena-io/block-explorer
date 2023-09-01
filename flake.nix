{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs = { self, nixpkgs }: let
    forAllSystems = f:
      nixpkgs.lib.genAttrs [
        "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"
      ] (system: f nixpkgs.legacyPackages.${system});
    selfWithoutFlake = builtins.path {
      name = "block-explorer-without-flake";
      path = ./.;
      filter = path: type:
        baseNameOf path != "flake.nix" &&
        baseNameOf path != "flake.lock" &&
        baseNameOf path != "flake";
    };
  in {
    inherit selfWithoutFlake;
    packages.x86_64-linux = let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in rec {
        exe = pkgs.runCommand "block-explorer" {
            buildInputs = [ pkgs.nix ];
            NIX_PATH = "nixpkgs=${nixpkgs.outPath}";
            evalTimeDependencies = import flake/evalTimeDependencies.nix;
            requiredSystemFeatures = [ "recursive-nix" ];
          }
          ''
            OUT=$(nix-build ${selfWithoutFlake} -A exe)
            ln -s $OUT $out
          '';
        static = pkgs.runCommand "block-explorer-static"
          { buildInputs = [ pkgs.coreutils pkgs.curl pkgs.bash ];
          }
          ''
            mkdir -p config/common config/frontend
            cat - > config/common/route <<EOF
              http://localhost:8000
            EOF
            ROUTE64=$(base64 config/common/route)
            cat - > config/frontend/data-backends <<EOF
              {
                "mainnet01": {
                  "p2p": "https://estats.chainweb.com:443",
                  "service": "https://estats.chainweb.com:443",
                  "data": "https://estats.chainweb.com:443"
                },
                "testnet04": {
                  "p2p": "https://estats.testnet.chainweb.com:443",
                  "service": "https://estats.testnet.chainweb.com:443",
                  "data": "https://estats.testnet.chainweb.com:443"
                }
              }
            EOF
            DATA_BACKENDS64=$(base64 config/frontend/data-backends)
            ${exe}/backend &
            sleep 1
            curl -s -o index.html http://0.0.0.0:8000
            mkdir -p $out
            # We'll convert index.html into a mustache template in which ROUTE64
            # is replaced with {{route}} and DATA_BACKENDS64 is replaced with {{dataBackends}}
            # sed -e "s|$ROUTE64|{{route}}|g" \
            #     -e "s|$DATA_BACKENDS64|{{dataBackends}}|g" \
            #     index.html > $out/index.html.mustache
            cp index.html $out/index.html

            bash ${flake/copy-assets.sh} ${exe}/frontend.jsexe.assets $out/ghcjs
            bash ${flake/copy-assets.sh} ${exe}/static.assets $out/static
            cp ${exe}/version $out/version
          '';
        serveDefault = pkgs.writeShellScriptBin "block-explorer-serve-default"
          ''
            exec ${pkgs.caddy}/bin/caddy run \
              --config <(${pkgs.caddy}/bin/caddy adapt --config ${pkgs.writeText "Caddyfile" ''
                http://:8000 {
                    # Redirect from root to /
                    @root path /
                    redir @root /

                    root * ${static}
                    file_server
                    try_files {path} {path}/ /index.html
                }
            ''})
          '';
      };
  };
}