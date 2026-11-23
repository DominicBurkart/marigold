{
  description = "marigold dev container for nanna";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix2container = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, nix2container }:
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

        mkImage = { name, tag ? "latest", packages, extraEnv ? [ ] }:
          nix2containerPkgs.nix2container.buildImage {
            inherit name tag;

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
        };

        environmentImages = pkgs.lib.mapAttrs'
          (envName: env: pkgs.lib.nameValuePair "devContainerImage-${envName}" (mkImage {
            name = "marigold-${envName}";
            inherit (env) packages extraEnv;
          }))
          environments;
      in
      {
        packages = {
          inherit devContainerImage;
        } // environmentImages;

        devShells.default = pkgs.mkShell {
          buildInputs = devContainerPackages;
        };
      }
    );
}
