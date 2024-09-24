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
          define-restartable
          restarter-guard
          restartable
          default-interactor
          )

  (import (rnrs base (6))
          (rnrs conditions (6))
          (rnrs control (6))
          (rnrs exceptions (6))
          (rnrs eval (6))
          (rnrs hashtables (6))
          (rnrs io simple (6))
          (rnrs lists (6))
          (rnrs syntax-case (6))
          (srfi :39)
          (srfi :255 helpers))

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

  ;; Return a list of the restarters composing condition *con*.
  (define (condition-restarters con)
    (filter restarter? (simple-conditions con)))

  ;; Returns an interactor whose interaction level is *level*.
  (define (make-default-interactor level)
    (lambda (obj)
      (if (restarter? obj)
          (let ((restarters (condition-restarters obj)))
            (call-with-current-continuation
              (lambda (abort)
                (let loop ()  ; main interaction loop
                  (call-with-current-continuation
                   (lambda (retry)
	             (interact restarters level abort retry)))
	             (loop)))))
	  (if (non-continuable-violation? obj)
	      (raise obj)
	      (raise-continuable obj)))))

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
      (with-exception-handler
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

  (define-syntax restarter-guard
    (lambda (syn)
      (syntax-case syn ()
        ((_ (x ...) body ...)
         (syntax (restarter-guard #f (x ...) body ...)))
        ((_ who (c1 c2 ...) body ...)
         (not (identifier? #'c1))
         (syntax (restarter-guard who (con c1 c2 ...) body ...)))
        ((_ who (con ((tag . arg*) description e1 e2 ...) ...)
           body ...)
         (and (identifier? #'con) (all-ids? #'(tag ...)))
         (begin
          (check-unique-ids 'restarter-guard syn (syntax (tag ...)))
          (with-syntax (((r ...) (generate-temporaries #'(tag ...))))
            (syntax
             ((call-with-current-continuation
               (lambda (k)
                 (let ((r (make-restarter 'tag
                                          description
                                          'who
                                          'arg*
                                          (lambda arg*
                                            (k (lambda ()
                                                 e1 e2 ...))))) ...)
                   (with-exception-handler
                    (lambda (con)
                      (raise-continuable
                       (if (condition? con) (condition con r ...) con)))
                    (lambda ()
                      ;; The body must be evaluated in the dynamic
                      ;; extent of the handler we just installed, so
                      ;; we can't just apply k to (lambda () body ...).
                      ;; Instead, we have a little dance to do.
                      (call-with-values
                       (lambda () body ...)
                       (lambda vals
                         (k (lambda ()
                              (apply values vals))))))))))))))))))

  (define-syntax restartable
    (syntax-rules ()
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

  (define-syntax define-restartable
    (syntax-rules ()
      ((_ ((x) . rest) _ ...)
       (syntax-violation 'restartable
                         "invalid syntax"
                         (define ((x) . rest))))
      ((_ (name arg ...) body1 body2 ...)
       (define name
         (let ((proc (lambda (arg ...) body1 body2 ...)))
           (lambda (arg ...)
             (restarter-guard name
                              (((use-arguments arg ...)
                                "Apply procedure to new arguments."
                                (name arg ...)))
               (proc arg ...))))))
      ((_ (name . args) body1 body2 ...)
       (define name
         (let ((proc (lambda args body1 body2 ...)))
           (lambda args
             (restarter-guard name
                              (((use-arguments . args)
                                "Apply procedure to new arguments."
                                (apply name args)))
               (apply proc args))))))
      ((_ name expr)
       (define name (restartable name expr)))))

  )
