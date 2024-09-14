;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

(define-library
    (restarts)
    (cond-expand
        ((library (srfi 222)) (import (rename (scheme base)
                                        (error r7rs-error))
                                      (scheme list)
                                      (scheme write)
                                      (scheme read)
                                      (srfi 222)))
        (else (import (rename (scheme base)
                        (error r7rs-error))
                      (scheme list)
                      (scheme write)
                      (scheme read))
              (begin
                (define (compound? obj) #f)
                (define (compound-subobjects obj) '()))))
    (export
        make-restarter
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

    (begin
     ;; R6RS shim
     (define-syntax error
       (syntax-rules ()
         ((error who msg . args)
          (r7rs-error msg (list 'location who) . args)))))

    (include "restarts-impl.scm")
    (include "interactive.scm"))
