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
          current-interactor
          with-current-interactor
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
          (srfi :39 parameters)
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
    (lambda (restarter)
      (call-with-current-continuation
       (lambda (abort)
         (let loop ()  ; main interaction loop
           (call-with-current-continuation
            (lambda (retry)
              (interact restarter level abort retry)))
              (loop))))))

  (define current-interactor
    (make-parameter (make-default-interactor 0)))

  (define (with-current-interactor thunk)
    (with-exception-handler
     (lambda (obj)
       (cond ((restarter? obj)
              ((current-interactor) obj)
              (raise
               (condition
                (make-who-condition 'with-current-interactor)
                (make-message-condition "interactor returned")
                (make-irritants-condition (list (current-interactor)))
                (make-non-continuable-violation))))
             (else (raise-continuable obj))))
     thunk))

  ;; Print some common & useful condition fields, if they're present.
  (define (show-non-restarter-condition-info con)
    (for-each (lambda (name pred acc)
                (when (pred con)
                  (display name)
                  (display ": ")
                  (display (acc con))
                  (newline)))
              '("Who" "Message" "Irritants")
              `(,who-condition? ,message-condition? ,irritants-condition?)
              `(,condition-who ,condition-message ,condition-irritants)))

  ;; Show the interactive user the available restarts, prompt for
  ;; a selection "command-line" and try to run it.
  (define (interact restarter level abort retry)
    ;; Table associating restarter tags with restarters. If there
    ;; are multiple restarters with the same tag, the first (by
    ;; index in the simple conditions list) takes priority.
    ;;
    ;; This is a quick-and-dirty way to handle the general problem
    ;; of duplicated restarter tags. A more sophisticated interactor
    ;; might allow the user to disambiguate their choice using the
    ;; restarter's "who" field, or perhaps an index number.
    (define restarters-by-tag
      (let* ((restarters (filter restarter?
                                 (simple-conditions restarter)))
             (table (make-eqv-hashtable (length restarters))))
        (for-each (lambda (r)
                    (hashtable-update! table
                                       (restarter-tag r)
                                       (lambda (t) (or t r))
                                       #f))
                  restarters)
        table))

    ;; Try to run the restarter selected by the user on the
    ;; values of the provided arguments.
    (define (invoke-selection choice)
      (parameterize ((current-interactor
                      (make-default-interactor (+ level 1))))
         (restarter-guard default-interactor
                          (interaction-con
                           ((retry)
                            "default-interactor: retry"
                            condition?
                            (retry)))
           (assert (pair? choice))
           ;; Lookup restarter by tag.
           (cond ((hashtable-ref restarters-by-tag (car choice) #f) =>
                  (lambda (r)
                    (let ((vals (map (lambda (e)
                                       (eval e (environment
                                                '(rnrs))))
                                     (cdr choice))))
                      (apply restart r vals))))
                 (else (error 'default-interactor
                              "invalid restarter choice"
                              choice))))))

    (with-current-interactor
     (lambda ()
       (display "Restartable exception occurred.\n")
       (show-non-restarter-condition-info restarter)
       (let-values (((_ks urs) (hashtable-entries restarters-by-tag)))
         (show-restarters (vector->list urs)))
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
             (loop)))))))

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

  ;; *alist* is a (Predicate . Restarter) alist. Returns a list
  ;; of only those restarters from *alist* whose predicates are
  ;; satisfied by *con*.
  (define (matching-restarters con alist)
    (map cdr (filter (lambda (p) ((car p) con)) alist)))

  (define-syntax restarter-guard
    (lambda (syn)
      ;; Raise a syntax violation if *bool* is false.
      (define (check-syntax bool who form)
        (unless bool
          (syntax-violation who "invalid syntax" form)))

      (syntax-case syn ()
        ((_ (x ...) body1 body2 ...)
         (syntax (restarter-guard #f (x ...) body1 body2 ...)))
        ((_ who (c1 c2 ...) body1 body2 ...)
         (not (identifier? #'c1))
         (syntax (restarter-guard who (con c1 c2 ...) body1 body2 ...)))
        ((_ who (con ((tag . arg*) description pred-expr e1 e2 ...) ...)
           body1 body2 ...)
         (begin
          (check-syntax (identifier? #'con) 'restarter-guard #'con)
          (check-syntax (all-ids? #'(tag ...))
                        'restarter-guard
                        #'(tag ...))
          (check-syntax (not (pair? #'who)) 'restarter-guard #'who)
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
                   (let ((alis (list (cons pred-expr r) ...)))
                     (with-exception-handler
                      (lambda (con)
                        (raise-continuable
                         (if (condition? con)
                             (apply condition
                                    con
                                    (matching-restarters con alis))
                             con)))
                      (lambda ()
                        ;; The body must be evaluated in the dynamic
                        ;; extent of the handler we just installed, so
                        ;; we can't just apply k to (lambda () body ...).
                        ;; Instead, we have a little dance to do.
                        (call-with-values
                         (lambda () body1 body2 ...)
                         (lambda vals
                           (k (lambda ()
                                (apply values vals)))))))))))))))))))

  (define-syntax restartable
    (syntax-rules (lambda)
     ((_ (x ...) . _)
      (syntax-violation 'restartable
                        "who must be an identifier or string"
                        '(x ...)))
     ((_ who (lambda formals body1 body2 ...))
      (letrec* ((proc (lambda formals body1 body2 ...))
                (restartable-proc
                 (lambda formals
                   (restarter-guard
                     who
                     (((use-arguments . formals)
                       "Apply procedure to new arguments."
                       assertion-violation?
                       (make-application restartable-proc formals)))
                     (make-application proc formals)))))
        restartable-proc))
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
                               assertion-violation?
                               (apply restartable-proc args)))
              (apply proc args)))))
         restartable-proc))))

  (define-syntax define-restartable
    (syntax-rules ()
      ((_ ((x ...) . _) . _)
       (syntax-violation 'define-restartable "invalid syntax" '(x ...)))
      ((_ (name . formals) body1 body2 ...)
       (define name
         (let ((proc (lambda formals body1 body2 ...)))
           (lambda formals
             (restarter-guard name
                              (((use-arguments . formals)
                                "Apply procedure to new arguments."
                                assertion-violation?
                                (make-application name formals)))
               (make-application proc formals))))))
      ((_ name expr)
       (define name (restartable name expr)))))

  ;; Helper for restartable and define-restartable.
  (define-syntax make-application
    (syntax-rules ()
      ((_ proc (x ...)) (proc x ...))
      ((_ proc (x1 ... xK . rest)) (apply proc x1 ... xK rest))
      ((_ proc var) (apply proc var))))
  )
