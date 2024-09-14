;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

;; Test driver for Chez Scheme.
(import (rnrs base (6))
        (srfi :64)
        (chezscheme)
        (restarts))

(include "restarts-test-body.scm")
