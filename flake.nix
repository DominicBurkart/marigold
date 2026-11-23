{
  description = "marigold development container image";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    aeneas.url = "github:AeneasVerif/aeneas/fb44cb400eb6bfe7dce97422ebe8fa748b7e4f9b";
    nix2container = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, nix2container, aeneas }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        nix2containerPkgs = nix2container.packages.${system};

        devContainerPackages = [
          rustToolchain
          pkgs.bash
          pkgs.coreutils
          pkgs.findutils
          pkgs.gnugrep
          pkgs.gnused
          pkgs.git
          pkgs.cacert
          pkgs.pkg-config
          pkgs.openssl
          pkgs.openssl.dev
          pkgs.stdenv.cc
          pkgs.zlib
        ];

        mkImage = { name, tag ? "latest", packages, extraEnv ? [ ], layerDeps ? [ ] }:
          nix2containerPkgs.nix2container.buildImage {
            inherit name tag;

            layers = map (deps: nix2containerPkgs.nix2container.buildLayer { inherit deps; }) layerDeps;

            copyToRoot = pkgs.buildEnv {
              name = "dev-env";
              paths = packages;
              pathsToLink = [ "/bin" "/etc" "/share" "/lib" "/include" ];
            };

            config = {
              Cmd = [ "${pkgs.coreutils}/bin/sleep" "infinity" ];
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                "PATH=/bin"
              ] ++ extraEnv;
              WorkingDir = "/workspace";
            };

            maxLayers = 100;
          };

        devContainerImage = mkImage {
          name = "marigold-dev";
          packages = devContainerPackages;
          extraEnv = [
            "CARGO_HOME=/tmp/cargo"
            "PKG_CONFIG_PATH=/lib/pkgconfig"
            "MARIGOLD_WORKSPACE_PATH=/workspace/marigold"
            "RUST_LOG=info"
          ];
        };

        kaniVersion = "0.68.0";

        kaniBundles = {
          x86_64-linux = {
            triple = "x86_64-unknown-linux-gnu";
            hash = "sha256-MuK0hNc+3gu/ZKLPCHnEJZQiSX2K7E7mfUSN6a54Q9M=";
          };
          aarch64-linux = {
            triple = "aarch64-unknown-linux-gnu";
            hash = "sha256-AELSaIvxVQJw3vSnnJaq7qE4YxgYy0q1SAEg8tUalCw=";
          };
        };

        kaniBundle = kaniBundles.${system};

        kaniToolchain = pkgs.rust-bin.fromRustupToolchain {
          channel = "nightly-2026-08-21";
          components = [ "llvm-tools" "rustc-dev" "rust-src" "rustfmt" "cargo" "rustc" ];
        };

        kani = pkgs.stdenv.mkDerivation {
          pname = "kani";
          version = kaniVersion;

          src = pkgs.fetchurl {
            url = "https://github.com/model-checking/kani/releases/download/kani-${kaniVersion}/kani-${kaniVersion}-${kaniBundle.triple}.tar.gz";
            inherit (kaniBundle) hash;
          };

          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          buildInputs = [ pkgs.stdenv.cc.cc.lib pkgs.zlib ];

          dontConfigure = true;
          dontBuild = true;

          preFixup = ''
            addAutoPatchelfSearchPath ${kaniToolchain}/lib
          '';

          installPhase = ''
            mkdir -p $out/share $out/bin
            cp -r . $out/share/kani-${kaniVersion}
            ln -s ${kaniToolchain} $out/share/kani-${kaniVersion}/toolchain
            ln -s kani-driver $out/share/kani-${kaniVersion}/bin/cargo-kani
            ln -s kani-driver $out/share/kani-${kaniVersion}/bin/kani
            for b in $out/share/kani-${kaniVersion}/bin/*; do
              ln -s $b $out/bin/$(basename $b)
            done
          '';

          meta.platforms = [ "x86_64-linux" "aarch64-linux" ];
        };

        leanLibs = [ pkgs.leanPackages.mathlib ] ++ pkgs.leanPackages.mathlib.allLeanDeps;

        basePackages = [
          pkgs.bash
          pkgs.coreutils
          pkgs.findutils
          pkgs.gnugrep
          pkgs.gnused
          pkgs.git
          pkgs.ripgrep
          pkgs.cacert
        ];

        rustPackages = basePackages ++ [
          rustToolchain
          pkgs.pkg-config
          pkgs.openssl
          pkgs.openssl.dev
          pkgs.stdenv.cc
          pkgs.zlib
        ];

        commonEnv = [
          "MARIGOLD_WORKSPACE_PATH=/workspace/marigold"
          "RUST_LOG=info"
        ];

        rustEnv = commonEnv ++ [
          "CARGO_HOME=/tmp/cargo"
          "PKG_CONFIG_PATH=/lib/pkgconfig"
        ];

        environments = {
          readonly = {
            packages = basePackages;
            extraEnv = commonEnv;
          };
          rust-test = {
            packages = rustPackages;
            extraEnv = rustEnv;
          };
          rust-build = {
            packages = rustPackages;
            extraEnv = rustEnv;
          };
          rust-cov = {
            packages = rustPackages ++ [ pkgs.cargo-tarpaulin ];
            extraEnv = rustEnv;
          };
          kani = {
            packages = rustPackages ++ [ kani ];
            extraEnv = rustEnv;
          };
          lean = {
            packages = rustPackages ++ [
              pkgs.lean4
              aeneas.packages.${system}.aeneas
            ];
            extraEnv = rustEnv ++ [
              "LEAN_PATH=${pkgs.lib.concatMapStringsSep ":" (d: "${d}/.lake/build/lib/lean") leanLibs}"
            ];
            layerDeps = [
              [ pkgs.lean4 ]
              leanLibs
            ];
          };
        };

        environmentSpecs = pkgs.lib.mapAttrs
          (envName: env: {
            name = "marigold-${envName}";
            inherit (env) packages extraEnv;
            layerDeps = env.layerDeps or [ ];
          })
          environments;

        environmentImages = pkgs.lib.mapAttrs'
          (envName: spec: pkgs.lib.nameValuePair "devContainerImage-${envName}" (mkImage spec))
          environmentSpecs;

        packageName = p: p.name or (builtins.baseNameOf (toString p));

        environmentManifest = pkgs.lib.mapAttrs
          (_: spec: map packageName (spec.packages ++ pkgs.lib.concatLists spec.layerDeps))
          environmentSpecs;
      in
      {
        lib = { inherit environmentManifest; };

        packages = {
          inherit devContainerImage;
        } // environmentImages;

        devShells.default = pkgs.mkShell {
          buildInputs = devContainerPackages;
        };
      }
    );
}
