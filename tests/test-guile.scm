;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

;; Shut up Guile's duplicate bindings warnings.
(eval-when (expand load eval)
  (default-duplicate-binding-handler '(last)))

(import (rnrs)
        (srfi srfi-39)
        (srfi srfi-64)
        (srfi srfi-255)
        (only (guile) include))

(include "test-restarters.scm")
