# NOTE: If obelisk is bumped, we may need to remove the below jsaddle-dom
# patch.

{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false
, kpkgs ? import ./deps/kpkgs { inherit system; }
}:
let
  obelisk = import ./.obelisk/impl { inherit system iosSdkVersion; inherit (kpkgs) reflex-platform-func; };
  pkgs = obelisk.reflex-platform.nixpkgs;
  haskellLib = pkgs.haskell.lib;
in with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super: with pkgs.haskell.lib;
    let inherit (pkgs) lib;
    in {
      bytes = dontCheck super.bytes;

      formattable = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "formattable";
        ver = "0.1.1";
        sha256 = "12ivb374zymkqzq3w9a9vhxbri5bpymi1di6kk45hp2f6b8lafpz";
      } {}));
      lens-aeson = dontCheck super.lens-aeson;

      http-media = dontCheck (self.callHackageDirect {
        pkg = "http-media";
        ver = "0.7.1.3";
        sha256 = "04d0f7rmr2z3nkd7l6jbl6iq2f1rc7psqyynrn9287bbv1hfrmqs";
      } {});

      # Comment out to see if the obelisk bump makes this unnecessary
      # jsaddle-dom = overrideCabal super.jsaddle-dom (drv: {
      #   preConfigure = (drv.preConfigure or "") + ''
      #     sed -i 's/unsafeEventNameAsync (toJSString "readystatechange")/unsafeEventName (toJSString "readystatechange")/' src/JSDOM/Generated/XMLHttpRequest.hs
      #   '';
      # });

      #pact = haskellLib.dontCheck super.pact;
      pact = dontCheck (self.callCabal2nix "pact" (pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "pact";
        rev = "7d6b89eb4ab207999553cd82b903f4e62be9cbcb";
        sha256 = "1scls0cidqfdjrk8hbb47xmcmrm6jx4354gmr16f27lpbmnwryr3";
      }) {});
      perfect-vector-shuffle = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "perfect-vector-shuffle";
        ver = "0.1.1";
        sha256 = "0ddr9ksqkl9ncvih54yzr3p6rs08r5wk0yf7aj3ijlk30dg7sdwf";
      } {}));
      reflex-dom-core = dontCheck super.reflex-dom-core; #webdriver fails to build

      servant-reflex = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "servant-reflex";
        ver = "0.3.5";
        sha256 = "1cj5b7hl4jhsqxfg8vdw50z8zvfxkj42f41hmyx217w6bv3s3fdb";
      } {}));

      typed-process = dontCheck (self.callHackageDirect {
        pkg = "typed-process";
        ver = "0.2.6.0";
        sha256 = "17m2n9ffh88nj32xc00d48phaxav92dxisprc42pipgigq7fzs5s";
      } {});

  };

  packages = {
    chainweb-api = hackGet ./deps/chainweb-api;
  };
})
