#!/bin/sh -e

# SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe
# SPDX-License-Identifier: MIT

# This is a hack which moves & translates the SRFI 255 library
# files for use with Guile. After running this script, you should
# be able to load the library in Guile by adding the current dir
# to your load path and importing (srfi srfi-255).

usage () {
    echo 'Usage:' "$(basename $0)" "[-h]" 1>&2
    exit 1
}

test $# -ne 0 && usage

( cd srfi/
  sed '/srfi :255 restarters/ s//srfi srfi-255/
       /srfi :255 helpers/ s//srfi srfi-255 helpers/' \
    ./\:255/restarters.sls > srfi-255.scm
  mkdir srfi-255
  sed '/srfi :255/s//srfi srfi-255/' ./\:255/helpers.sls > \
    srfi-255/helpers.scm
  )
