;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

(library (restarts)
  (export make-restarter
          restarter?
          restarter-tag
          restarter-description

          restart
          ambient-restarters
          with-restarters
          find-restarter
          collect-restarters
          restart-interactively
          interactor)

  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs io simple (6))
          (except (srfi :1) map for-each)
          (srfi :9)
          (srfi :39)
          (only (chezscheme) include))

  (define (compound? obj) #f)
  (define (compound-subobjects obj) '())
  (include "restarts-impl.scm")
  (include "interactive.scm"))
