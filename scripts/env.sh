#! /bin/sh

## How to use:
## eval $(scripts/env.sh)

## This script setups the shell environment, to be able to build and
## use the generated executables, including the bytecode version

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

eval "$(opam env)"

add_ld_path () {
  echo "CAML_LD_LIBRARY_PATH=\"$src_dir/$1:\$CAML_LD_LIBRARY_PATH\""
}

opam env
add_ld_path "_build/default/src/lib_sapling"
add_ld_path "_build/default/vendors/ocaml-bls12-381/src"
add_ld_path "_build/default/vendors/benchmark-utils"
add_ld_path "_build/default/vendors/numerics/lib"
add_ld_path "_build/default/vendors/ocaml-lmdb/src"
add_ld_path "_build/default/vendors/ocaml-uecc/src"
