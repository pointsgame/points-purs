{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };

    purescript-overlay = {
      type = "github";
      owner = "thomashoneyman";
      repo = "purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.purescript-overlay.overlays.default ];
        };
      in
      {
        devShell = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            purescript-language-server
            purs-tidy
            purescript
            spago-unstable
            purs-backend-es
            purescm
            chez
            pcre2

            esbuild
            stylelint
            minhtml
          ];

          LD_LIBRARY_PATH = inputs.nixpkgs.lib.makeLibraryPath buildInputs;
        };
      }
    );
}
