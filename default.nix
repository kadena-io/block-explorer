# NOTE: If obelisk is bumped, we may need to remove the below jsaddle-dom
# patch.

{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "13.2"
, withHoogle ? false
, kpkgs ? import ./deps/kpkgs { inherit system; }
}:
let
  obelisk = import ./.obelisk/impl {
    inherit system iosSdkVersion; inherit (kpkgs) reflex-platform-func;
    terms.security.acme.acceptTerms = true;
  };
  pkgs = obelisk.reflex-platform.nixpkgs;
  haskellLib = pkgs.haskell.lib;
  nix-thunk = import ./deps/nix-thunk {};
in with obelisk;
project ./. ({ pkgs, ... }: {
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

      network = dontCheck super.network;

      pact = dontCheck super.pact;
      pact-time = haskellLib.dontCheck (self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.0";
        sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
      } {});

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

      swagger2 = dontCheck (self.callCabal2nix "swagger2" (pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "swagger2";
        rev = "96d805d93cd9fb48c0a5acd8e1fb1f382d8aad05";
        sha256 = "1qrfjyp33krn5r6559xzc71dgl71mk9pcz5pzl3g0p7alwlljarb";
      }) {});

      typed-process = dontCheck (self.callHackageDirect {
        pkg = "typed-process";
        ver = "0.2.6.0";
        sha256 = "17m2n9ffh88nj32xc00d48phaxav92dxisprc42pipgigq7fzs5s";
      } {});

  };

  packages = {
    chainweb-api = nix-thunk.thunkSource ./deps/chainweb-api;
    pact = nix-thunk.thunkSource ./deps/pact;
  };
})
