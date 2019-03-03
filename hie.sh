#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

argv=( "$@" )
argv=( "${argv[@]/\'/\'\\\'\'}" )
argv=( "${argv[@]/#/\'}" )
argv=( "${argv[@]/%/\'}" )

exit 0

exec nix-shell --pure --run "exec $(nix-build -o dist-newstyle/nix/hie -A hie)/bin/hie ${argv[*]}"
