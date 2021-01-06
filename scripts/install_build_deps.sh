#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

. "$script_dir"/version.sh

if [ "$1" = "--dev" ]; then
    dev=yes
else
    dev=
fi

opam repository set-url tezos --dont-select $opam_repository || \
    opam repository add tezos --dont-select $opam_repository > /dev/null 2>&1

opam update --repositories --development

OCAML_COMPILER_VARIANT=ocaml-variants.$ocaml_version+flambda

if [ ! -d "$src_dir/_opam" ] ; then
    opam switch create "$src_dir" --repositories=tezos $OCAML_COMPILER_VARIANT
fi

if [ ! -d "$src_dir/_opam" ] ; then
    echo "Failed to create the opam switch"
    exit 1
fi

eval $(opam env --shell=sh)

if [ -n "$dev" ]; then
    opam repository remove default > /dev/null 2>&1 || true
fi

if [ "$(ocaml -vnum)" != "$ocaml_version" ]; then
    opam install --unlock-base $OCAML_COMPILER_VARIANT
fi

if [ "$(ocamlc -config | grep flambda | awk '{ print $2 }')" != "true" ]; then
    opam install --unlock-base $OCAML_COMPILER_VARIANT
fi

# Must be done before install_build_deps.raw.sh because install_build_deps.raw.sh installs
# opam packages that depend on Rust.
"$script_dir"/install_build_deps.rust.sh

opam install --yes opam-depext

"$script_dir"/install_build_deps.raw.sh

if [ -n "$dev" ]; then
    opam repository add default --rank=-1 > /dev/null 2>&1 || true
    opam install merlin odoc --criteria="-changed,-removed"
fi
