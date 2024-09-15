;;; Copyright © 2024 Wolfgang Corcoran-Mathe
;;; Copyright © 2024 Marc Nieper-Wißkirchen
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
          restartable
          with-restarters
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
    (formals restarter-formals)
    (description restarter-description)
    (invoker restarter-invoker))

  (define default-interactor
    (let default-interactor ([n 0])
      (lambda (restarters)
        (call/cc
         (lambda (abort)
           (let f ()
             (call/cc
              (lambda (retry)
                (display "Restartable exception occured.")
                (newline)
                (for-each
                 (lambda (restarter)
                   (write `(,(restarter-tag restarter) . ,(restarter-formals restarter)))
                   (display ": ")
                   (display (restarter-description restarter))
                   (newline))
                 restarters)
                (let loop ()
                  (display "restart [")
                  (display n)
                  (display "]> ")
                  ;; TODO: Remove later duplicate tags.
                  ;; TODO: Display the condition somewhere
                  (let ([e (read)])
                    (if (eof-object? e)
                        (abort)             ;TODO: improve behaviour
                        (call-with-values
                            (lambda ()
                              (apply
                               (with-interactor
                                   (default-interactor (+ n 1))
                                 (lambda ()
                                   (with-restarters
                                       ([(retry)
                                         "default-interactor: retry"
                                         (retry)])
                                     (eval `(lambda ,(map restarter-tag restarters)
                                              ,e)
                                           (environment '(rnrs))))))
                               (map (lambda (restarter)
                                      (lambda arg*
                                        (apply (restarter-invoker restarter) arg*)))
                                    restarters)))
                          (lambda val*
                            (for-each (lambda (val) (display val) (newline)) val*)
                            (loop))))))))
             (f)))))))

  (define current-interactor
    (make-parameter default-interactor))

  (define with-interactor
    (lambda (interactor thunk)
      (with-exception-handler
          (lambda (c)
            (when (restarter? c)
              (let ([c* (filter restarter? (simple-conditions c))])
                (interactor c*)
                (raise (condition (make-who-condition 'with-interactor)
                                  (make-message-condition "interactor returned")
                                  (make-irritants-condition (list interactor c*))
                                  (make-non-continuable-violation)))))
            (raise-continuable c))
        thunk)))

  (define with-current-interactor
    (lambda (thunk)
      (with-interactor (current-interactor) thunk)))

  (define-syntax with-restarters
    (syntax-rules ()
      ((_ (x ...) body ...)
       (with-restarters #f (x ...) body ...))
      ((_ who ((x ...) ...) body ...)
       ; inject 'con' for the condition object.
       (with-restarters who (con ((x ...) ...)) body ...))
      ((_ who (con (((tag . arg*) description e1 e2 ...) ...))
          body1 body2 ...)
       ((call-with-current-continuation  ; apply the returned value
         (lambda (k)
           (with-exception-handler
             (lambda (con)
               (if (condition? con)
                   (raise-continuable
                    (condition
                     con
                     (build-restarter k tag arg* description e1 e2 ...)
                     ...))
                   (raise-continuable con)))
             (lambda ()
               (call-with-values
                (lambda () body1 body2 ...)
                (lambda vals
                  (k (lambda () (apply values vals)))))))))))))

  ;; Helper for with-restarters.
  (define-syntax build-restarter
    (syntax-rules ()
      ((_ k t a* d e1 e2 ...)
       (make-restarter 't 'a* d (lambda a*
                                  (k (lambda () e1 e2 ...)))))))

  (define-syntax restartable
    (syntax-rules (define lambda)
      ((_ define ((x) . rest) _ ...)
       (syntax-violation 'restartable
                         "invalid syntax"
                         (define ((x) . rest))))
      ((_ define (name . formals) body1 body2 ...)
       (define name
         (let ((proc (lambda formals body1 body2 ...)))
           (lambda formals
             (with-restarters 'name
                              (((replace-arguments . formals)
                                "Apply procedure to new arguments."
                                (name . formals)))
               (proc . formals))))))
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
            (with-restarters who
                             (((replace-arguments . args)
                               "Apply procedure to new arguments."
                               (apply restartable-proc args)))
              (apply proc args)))))
         restartable-proc))))

  (define with-abort-restarter
    (lambda (thunk)
      (call/cc
       (lambda (abort)
         (with-restarters
             ([(abort)
               "abort"
               (abort)])
           (thunk))))))

  )
