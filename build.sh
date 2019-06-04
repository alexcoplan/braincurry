#!/bin/sh

# Shelling out to ocamlbuild for now.
# TODO: improve the build system as and when we need to.

set -e
ocamlbuild -pkgs ounit test_braincurry.native
./test_braincurry.native
