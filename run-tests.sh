#!/bin/sh

# SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
# SPDX-License-Identifier: MIT

usage () {
        printf 'Usage: %s SCHEME-KEYWORD\n' "$(basename $0)" 1>&2
        printf 'Supported SCHEME-KEYWORDs: chez, guile\n' 1>&2
        exit 1
}

test $# -ne 1 && usage

check_guile_files () {
        if ! test -r srfi/srfi-255.scm || ! test -d srfi/srfi-255
        then
                echo "Can't find Guile-friendly library hierarchy."
                echo "Please run make-guile-hierarchy and try again."
                exit 1
        fi
}

case "$1" in
  chez)
        scheme --libdirs "$CHEZSCHEMELIBDIRS:." tests/test-chez.scm
        ;;
  guile)
        check_guile_files
        guile -L . --no-auto-compile -l tests/test-guile.scm
        ;;
  -h|--help)
        usage
        ;;
  *)
        echo 'Unknown implementation keyword.' 1>&2
        ;;
esac
