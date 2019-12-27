# NOTE: If obelisk is bumped, we may need to remove the below jsaddle-dom
# patch.

{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super: with pkgs.haskell.lib;
    let callHackageDirect = {pkg, ver, sha256}@args:
          let pkgver = "${pkg}-${ver}";
          in self.callCabal2nix pkg (pkgs.fetchzip {
               url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
               inherit sha256;
             }) {};
    in {
      bytes = dontCheck super.bytes;
      formattable = doJailbreak (dontCheck (callHackageDirect {
        pkg = "formattable";
        ver = "0.1.1";
        sha256 = "12ivb374zymkqzq3w9a9vhxbri5bpymi1di6kk45hp2f6b8lafpz";
      }));
      lens-aeson = dontCheck super.lens-aeson;
      perfect-vector-shuffle = doJailbreak (dontCheck (callHackageDirect {
        pkg = "perfect-vector-shuffle";
        ver = "0.1.1";
        sha256 = "0ddr9ksqkl9ncvih54yzr3p6rs08r5wk0yf7aj3ijlk30dg7sdwf";
      }));
      jsaddle-dom = overrideCabal super.jsaddle-dom (drv: {
        preConfigure = (drv.preConfigure or "") + ''
          sed -i 's/unsafeEventNameAsync (toJSString "readystatechange")/unsafeEventName (toJSString "readystatechange")/' src/JSDOM/Generated/XMLHttpRequest.hs
        '';
      });
  };

  packages = {
    chainweb-api = hackGet ./deps/chainweb-api;
  };
})
