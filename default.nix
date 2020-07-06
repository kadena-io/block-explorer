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
      pact = haskellLib.dontCheck super.pact;
      perfect-vector-shuffle = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "perfect-vector-shuffle";
        ver = "0.1.1";
        sha256 = "0ddr9ksqkl9ncvih54yzr3p6rs08r5wk0yf7aj3ijlk30dg7sdwf";
      } {}));
      jsaddle-dom = overrideCabal super.jsaddle-dom (drv: {
        preConfigure = (drv.preConfigure or "") + ''
          sed -i 's/unsafeEventNameAsync (toJSString "readystatechange")/unsafeEventName (toJSString "readystatechange")/' src/JSDOM/Generated/XMLHttpRequest.hs
        '';
      });
      reflex-dom-core = dontCheck super.reflex-dom-core; #webdriver fails to build
      servant-reflex = dontCheck (self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "37a3e8f2566627d910df140982bd49bf4dba171e";
        sha256 = "1yqxf6f81n4y4527rl69hfqymrnmj7lskgns2qsh59ibisp3y9rg";
      }) {});
  };

  packages = {
    chainweb-api = hackGet ./deps/chainweb-api;
  };
})
