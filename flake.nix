# SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "dependencies for my AoC 2022 solutions";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixos-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

      in rec {
        devShell = pkgs.mkShell {
          nativeBuildInputs = (with pkgs; [

            racket

          ]);
        };
      }
    );
}
