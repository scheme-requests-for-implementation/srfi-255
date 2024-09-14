;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

;; Test driver for Guile.
(import (rnrs base (6))
        (srfi srfi-64)
        (guile)
        (restarts))

(include "restarts-test-body.scm")
