;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

;; Test driver for chibi-scheme.
(import (scheme base)
        (chibi test)
        (restarts))

;;; SRFI 64 -> (chibi test) shims

(define-syntax test-eqv
  (syntax-rules ()
    ((_ . args)
     (test-equal eqv? . args))))

;; chibi calls this "test"
(define-syntax test-equal
  (syntax-rules ()
    ((_ . args)
     (test . args))))

(include "restarts-test-body.scm")
