;;; Copyright © 2024 Wolfgang Corcoran-Mathe
;;; Copyright © 2024 Marc Nieper-Wißkirchen
;;;
;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject
;;; to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;;; KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;;; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(library (srfi :255 restarters)
  (export &restarter
          make-restarter
          restarter?
          restarter-tag
          restarter-formals
          restarter-description
          restarter-invoker
          restarter-who
          restart
          restartable
          restarter-guard
          restartable
          with-interactor
          with-current-interactor
          current-interactor
          default-interactor
          with-abort-restarter)

  (import (rnrs base (6))
          (rnrs conditions (6))
          (rnrs control (6))
          (rnrs exceptions (6))
          (rnrs eval (6))
          (rnrs io simple (6))
          (rnrs lists (6))
          (rnrs syntax-case (6))
          (srfi :39))

  (define-condition-type &restarter &condition
    make-restarter restarter?
    (tag restarter-tag)
    (description restarter-description)
    (who restarter-who)
    (formals restarter-formals)
    (invoker restarter-invoker))

  (define (restart restarter . args)
    (assert (restarter? restarter))
    (apply (restarter-invoker restarter) args))

  ;;; Sample default interactor

  ;;; This interactor maintains an interaction level which is
  ;;; incremented each time a restartable exception occurs within
  ;;; a restart.

  ;; Returns an interactor whose interaction level is *level*.
  (define (make-default-interactor level)
    (lambda (restarters)
      (call-with-current-continuation
       (lambda (abort)
         (let loop ()  ; main interaction loop
           (call-with-current-continuation
            (lambda (retry)
	      (interact restarters level abort retry)))
	   (loop))))))

  (define default-interactor (make-default-interactor 0))

  ;; Show the interactive user the available restarts, prompt for
  ;; a selection "command-line" and try to run it.
  (define (interact restarters level abort retry)
    ;; Alist associating restarter tags with restarters.
    (define restarters-by-tag
      (map (lambda (r) `(,(restarter-tag r) . ,r))
           restarters))

    ;; Try to run the restarter selected by the user on the
    ;; values of the provided arguments.
    (define (invoke-selection choice)
      (with-interactor
       (make-default-interactor (+ level 1))
       (lambda ()
         (restarter-guard default-interactor
                          (interaction-con
			   ((retry)
			    "default-interactor: retry"
			    (retry)))
           (assert (pair? choice))
	   ;; Lookup restarter by tag.
	   (cond ((assv (car choice) restarters-by-tag) =>
	          (lambda (p)
	            (let ((r (cdr p))
		          (vals (map (lambda (e)
		                       (eval e (environment
				                '(rnrs))))
				     (cdr choice))))
		      (apply restart r vals))))
	         (else (error 'default-interactor
	                      "invalid restarter choice"
			      choice)))))))

    (display "Restartable exception occurred.\n")
    (show-restarters restarters)
    (let loop ()  ; prompt loop
      (display "restart [")
      (display level)
      (display "]> ")
      (let ((choice (read)))
        (when (eof-object? choice) (abort))  ; TODO: Improve behavior
        (let-values ((vals (invoke-selection choice)))
	  (unless (null? vals)
	    (for-each (lambda (v)
	                (display v)
		        (newline))
		      vals))
	  (loop)))))

  ;; Print a list of restarters.
  (define (show-restarters restarters)
    (for-each (lambda (r)
                (write `(,(restarter-tag r) . ,(restarter-formals r)))
		(display " [")
		(display (restarter-who r))
		(display "]: ")
		(display (restarter-description r))
		(newline))
	      restarters))

  (define current-interactor
    (make-parameter default-interactor))

  (define with-interactor
    (lambda (interactor thunk)
      (with-exception-handler
          (lambda (c)
            (when (restarter? c)
              (let ([c* (filter restarter? (simple-conditions c))])
                (interactor c*)
                (raise
                 (condition
                  (make-who-condition 'with-interactor)
                  (make-message-condition "interactor returned")
                  (make-irritants-condition (list interactor c*))
                  (make-non-continuable-violation)))))
            (raise-continuable c))
        thunk)))

  (define with-current-interactor
    (lambda (thunk)
      (with-interactor (current-interactor) thunk)))

  (define-syntax restarter-guard
    (syntax-rules ()
      ((_ (x ...) body ...)
       (restarter-guard #f (x ...) body ...))
      ((_ who ((x ...) ...) body ...)
       ; inject 'con' for the condition object.
       (restarter-guard who (con (x ...) ...) body ...))
      ((_ who (con ((tag . arg*) description e1 e2 ...) ...)
          body1 body2 ...)
       ((call-with-current-continuation  ; apply the returned value
         (lambda (k)
           (with-exception-handler
             (lambda (con)
               (if (condition? con)
                   (raise-continuable
                    (condition
                     con
                     (build-restarter k tag description arg* who e1 e2 ...)
                     ...))
                   (raise-continuable con)))
             (lambda ()
               (call-with-values
                (lambda () body1 body2 ...)
                (lambda vals
                  (k (lambda () (apply values vals)))))))))))))

  ;; Helper for restarter-guard.
  (define-syntax build-restarter
    (syntax-rules ()
      ((_ k tag desc arg* who e1 e2 ...)
       (make-restarter 'tag
                       desc
                       'who
		       'arg*
		       (lambda arg*
                         (k (lambda () e1 e2 ...)))))))

  (define-syntax restartable
    (syntax-rules (define)
      ((_ define ((x) . rest) _ ...)
       (syntax-violation 'restartable
                         "invalid syntax"
                         (define ((x) . rest))))
      ((_ define (name arg ...) body1 body2 ...)
       (define name
         (let ((proc (lambda (arg ...) body1 body2 ...)))
           (lambda (arg ...)
             (restarter-guard name
                              (((use-arguments arg ...)
                                "Apply procedure to new arguments."
                                (name arg ...)))
               (proc arg ...))))))
      ((_ define (name . args) body1 body2 ...)
       (define name
         (let ((proc (lambda args body1 body2 ...)))
           (lambda args
             (restarter-guard name
                              (((use-arguments . args)
                                "Apply procedure to new arguments."
                                (apply name args)))
               (apply proc args))))))
      ((_ define name expr)
       (define name (restartable name expr)))
      ((_ who expr)
       (letrec*
        ((proc expr)
         (junk (if (not (procedure? proc))
                   (assertion-violation
                    'restartable
                    "expression must evaluate to a procedure"
                    'expr)))
         (restartable-proc
          (lambda args
            (restarter-guard who
                             (((use-arguments . args)
                               "Apply procedure to new arguments."
                               (apply restartable-proc args)))
              (apply proc args)))))
         restartable-proc))))

  (define with-abort-restarter
    (lambda (thunk)
      (call/cc
       (lambda (abort)
         (restarter-guard
             ([(abort)
               "abort"
               (abort)])
           (thunk))))))

  )
