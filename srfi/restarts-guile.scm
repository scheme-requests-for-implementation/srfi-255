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
          (srfi srfi-1)
          (srfi srfi-9)
          (only (guile) include make-parameter parameterize))

  (define (compound? obj) #f)
  (define (compound-subobjects obj) '())
  (include "restarts-impl.scm")
  (include "interactive.scm"))
