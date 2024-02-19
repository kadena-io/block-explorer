{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs = { self, nixpkgs }: let
    forAllSystems = f:
      nixpkgs.lib.genAttrs [
        "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"
      ] (system: f {
           pkgs = nixpkgs.legacyPackages.${system};
           inherit system;
        });
    selfWithoutFlake = builtins.path {
      name = "block-explorer-without-flake";
      path = ./.;
      filter = path: type:
        baseNameOf path != "flake.nix" &&
        baseNameOf path != "flake.lock" &&
        baseNameOf path != "flake" &&
        baseNameOf path != ".github"
      ;
    };
    publicDataBackends = let
      estats = "https://estats.chainweb.com:443";
      estats-testnet = "https://estats.testnet.chainweb.com:443";
      devnet = "http://localhost:8080";
      constNetConfig = url: {
        p2p = url;
        service = url;
        data = url;
      };
      in {
        mainnet01 = constNetConfig estats;
        testnet04 = constNetConfig estats-testnet;
        development = constNetConfig devnet;
        fast-development = constNetConfig devnet;
      };
    renderStatic = {
        pkgs,
        route ? "http://localhost:8000",
        dataBackends ? publicDataBackends
      }: pkgs.runCommand "block-explorer-static" { buildInputs = [ pkgs.coreutils ]; } ''
        mkdir $out
        ln -s ${self.packages.x86_64-linux.static}/* $out
        ROUTE=$(base64 -w 0 <<< ${route})
        DATA_BACKENDS=$(base64 -w 0 <<< ${builtins.toJSON dataBackends})
        ${pkgs.mustache-go}/bin/mustache $out/index.html.mustache > $out/index.html <<EOF
          {
            "route": "$ROUTE",
            "dataBackends": "$DATA_BACKENDS"
          }
        EOF
      '';
    x86-linux-only-packages = let
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
            echo http://localhost:8000 > config/common/route
            ROUTE64=$(base64 config/common/route)

            echo '{ "mock": "mock" }' > config/frontend/data-backends
            DATA_BACKENDS64=$(base64 config/frontend/data-backends)

            echo no-tracking > config/frontend/tracking-id

            ${exe}/backend &
            sleep 1

            curl -s -o index.html http://0.0.0.0:8000/about

            mkdir -p $out
            # We'll convert index.html into a mustache template in which ROUTE64
            # is replaced with {{route}} and DATA_BACKENDS64 is replaced with {{dataBackends}}
            sed -e "s|$ROUTE64|{{route}}|g" \
                -e "s|$DATA_BACKENDS64|{{dataBackends}}|g" \
                index.html > $out/index.html.mustache

            bash ${flake/copy-assets.sh} ${exe}/frontend.jsexe.assets ghcjs
            find ghcjs \( -name "*.unminified.js" -o -name "*.js.map" \) -exec rm -f {} +
            cp -Lr ghcjs $out/ghcjs

            bash ${flake/copy-assets.sh} ${exe}/static.assets static
            cp -Lr static $out/static

            cp ${exe}/version $out/version
          '';
      };
  in {
    inherit selfWithoutFlake;
    packages = forAllSystems ({pkgs, system, ...}:
      pkgs.lib.optionalAttrs (system == "x86_64-linux") x86-linux-only-packages // rec {
        default = renderStatic { inherit pkgs; };
        serve = pkgs.writeShellScriptBin "serve-block-explorer"
          ''
            exec ${pkgs.caddy}/bin/caddy run \
              --config <(${pkgs.caddy}/bin/caddy adapt --config ${pkgs.writeText "Caddyfile" ''
                http://:8000 {
                    redir / /mainnet

                    @notGhcjsOrStatic not path /ghcjs/* /static/*
                    header @notGhcjsOrStatic Cache-Control "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
                    header @notGhcjsOrStatic Pragma "no-cache"
                    header @notGhcjsOrStatic Expires "0"

                    root * ${default}
                    file_server
                    try_files {path} {path}/ /index.html
                }
            ''})
          '';
      });
    apps = forAllSystems ({pkgs, system, ...}: {
      default = {
        type = "app";
        program = "${self.packages.${system}.serve}/bin/serve-block-explorer";
      };
    });
  };
}