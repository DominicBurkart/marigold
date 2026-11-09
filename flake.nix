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

        devContainerImage = nix2containerPkgs.nix2container.buildImage {
          name = "marigold-dev";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "dev-env";
            paths = devContainerPackages;
            pathsToLink = [ "/bin" "/etc" "/share" "/lib" "/include" ];
          };

          config = {
            Cmd = [ "${pkgs.coreutils}/bin/sleep" "infinity" ];
            Env = [
              "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              "PATH=/bin"
              "CARGO_HOME=/tmp/cargo"
              "PKG_CONFIG_PATH=/lib/pkgconfig"
              "MARIGOLD_WORKSPACE_PATH=/workspace/marigold"
              "RUST_LOG=info"
            ];
            WorkingDir = "/workspace";
          };

          maxLayers = 100;
        };
      in
      {
        packages = {
          inherit devContainerImage;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = devContainerPackages;
        };
      }
    );
}
