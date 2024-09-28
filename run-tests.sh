#!/bin/sh

# SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
# SPDX-License-Identifier: MIT

usage () {
        printf 'Usage: %s chez\n' "$(basename $0)" 1>&2
        exit 1
}

test $# -ne 1 && usage

case "$1" in
  chez)
        scheme --libdirs "$CHEZSCHEMELIBDIRS:." test-restarters.scm
        ;;
  guile)
        ./make-guile-hierarchy.sh
        guile -L . --fresh-auto-compile -l test-restarters.scm
        ;;
  -h|--help)
        usage
        ;;
  *)
        echo 'Unknown implementation keyword.' 1>&2
        ;;
esac
