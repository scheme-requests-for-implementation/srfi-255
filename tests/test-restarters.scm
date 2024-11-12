;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

;; Invoke the restarter with tag *tag* on the args, if
;; such a restarter is a component of *con*.
(define (restart/tag tag con . args)
  (let ((r (find (lambda (c)
                   (and (restarter? c)
                        (eqv? tag (restarter-tag c))))
                 (simple-conditions con))))
    (if r
        (apply restart r args)
        (error 'restart/tag
               "no restarter found with this tag"
               tag restarters))))

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

(define (my-test-runner-factory)
  (let ((runner (test-runner-null)))

    (define (test-end runner)
      (case (test-result-kind runner)
        ((pass)
         (display "Pass: ")
         (display (test-runner-test-name runner))
         (newline))
        ((fail)
         (display "FAIL: ")
         (display (test-runner-test-name runner))
         (display ". Expected ")
         (display (test-result-ref runner 'expected-value))
         (display ", got ")
         (display (test-result-ref runner 'actual))
         (display ".\n"))))

    (define (test-final runner)
      (display "===============================\n")
      (display "Total passes: ")
      (display (test-runner-pass-count runner))
      (newline)
      (display "Total failures: ")
      (display (test-runner-fail-count runner))
      (newline))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

(test-runner-factory my-test-runner-factory)

;;; Tests

(test-begin "Restarters")
(test-assert "restarter objects 1"
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con) (restart/tag 'use-value con #t))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-value "Return x." 'foo '(x) condition? k)))))))

(test-assert "restarter objects 2"
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con) (restart/tag 'use-not-value con #f))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-not-value
                        "Return (not x)."
                        'foo
                        '(x)
                        condition?
                        (lambda (x) (k (not x))))))))))

(test-equal "restarter objects 3"
 '(dump-restarter-data "Return restarter info as a list." () foo)
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con)
       (restart/tag 'dump-restarter-data con))
     (lambda ()
       (letrec*
        ((dump
          (lambda ()
            (k (list (restarter-tag r)
                     (restarter-description r)
                     (restarter-formals r)
                     (restarter-who r)))))
         (r (make-restarter 'dump-restarter-data
                            "Return restarter info as a list."
                            'foo
                            '()
                            condition?
                            dump)))

         (raise-continuable r)))))))

(test-assert "restarter-guard 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'return-true con))
  (lambda ()
    (restarter-guard
     (((return-true) "Return #t." condition? #t))
     (error 'no-one "something happen!")))))

(test-equal "restarter-guard 2"
 '(1 2 3)
 (with-exception-handler
  (lambda (con) (restart/tag 'return-values con 1 2 3))
  (lambda ()
    (restarter-guard
     (((return-values . vs) "Return vs." error? vs))
     (error 'no-one "something happen!")))))

;; Ensure that the (lexically) most recently installed restarter
;; takes priority when there are duplicate tags.
(test-equal "restarter-guard 3: duplicate tags"
 7
 (with-exception-handler
  (lambda (con) (restart/tag 'use con 3))
  (lambda ()
    (restarter-guard whole-expr (((use x)
                                  ""
                                  condition?
                                  x))
      (+ 4
         (restarter-guard guard2 (((use x)
                                   ""
                                   condition?
                                   x))
           (/ 2 0)))))))

(test-assert "define-restartable 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable (f x)
      (if (not x)
          (assertion-violation 'f "false")
          x))
    (f #f))))

(test-equal "define-restartable 2"
 '(1 2)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    (define-restartable (f . xs)
      (if (null? xs)
          (assertion-violation 'f "empty")
          xs))
    (f))))

(test-assert "define-restartable 3"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable f
      (lambda (x)
        (if (not x)
            (assertion-violation 'f "false")
            x)))
    (f #f))))

(test-equal "define-restartable 4 (polyvariadic)"
 '(1 (2))
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    (define-restartable (f x . rest)
      (if (null? rest)
          (assertion-violation 'f "empty rest parameter")
          (list x rest)))
    (f 1))))

(test-equal "restartable 1"
 '(2 3 4 5)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 3))
  (lambda ()
    (map (restartable
          "anonymous"
          (lambda (x)
	    (if x (+ x 1) (assertion-violation 'no-one "false"))))
	 '(1 2 #f 4)))))

(test-equal "restartable 2 (variadic)"
 '(1 2)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    ((restartable "test"
                  (lambda xs
                    (if (null? xs)
                        (assertion-violation 'test "empty")
                        xs)))))))

(test-equal "restartable 3 (polyvariadic)"
 '(1 (2))
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    ((restartable "test"
                  (lambda (x . rest)
                    (if (null? rest)
                        (assertion-violation 'test "empty")
                        (list x rest))))
     0))))

(test-equal "with-current-interactor"
 0
 (with-current-interactor
  (lambda ()
    (parameterize ((current-interactor
                    (lambda (rs)
                      (let ((r (find (lambda (r)
                                       (eqv? (restarter-tag r)
                                             'return-zero))
                                     rs)))
                        (and r (restart r))))))
      (restarter-guard somewhere
       (con ((return-zero)
              "return zero"
              assertion-violation?
              0))
       (assertion-violation 'somewhere "bad"))))))

(test-end)

(exit (test-runner-fail-count (test-runner-current)))
