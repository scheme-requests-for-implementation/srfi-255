;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi :64)
        (srfi :255))

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

;;; Tests

(test-begin "Restarters")
(test-assert "restarter objects 1"
 (call-with-current-continuation
  (lambda (k)
    (with-exception-handler
     (lambda (con) (restart/tag 'use-value con #t))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-value "Return x." 'foo '(x) k)))))))

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
                            dump)))

         (raise-continuable r)))))))

(test-assert "restarter-guard 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'return-true con))
  (lambda ()
    (restarter-guard
     (((return-true) "Return #t." #t))
     (error 'no-one "something happen!")))))

(test-equal "restarter-guard 2"
 '(1 2 3)
 (with-exception-handler
  (lambda (con) (restart/tag 'return-values con 1 2 3))
  (lambda ()
    (restarter-guard
     (((return-values . vs) "Return vs." vs))
     (error 'no-one "something happen!")))))

(test-assert "define-restartable 1"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable (f x)
      (if (not x)
          (error 'f "false")
          x))
    (f #f))))

(test-equal "define-restartable 2"
 '(1 2)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 1 2))
  (lambda ()
    (define-restartable (f . xs)
      (if (null? xs)
          (error 'f "empty")
          xs))
    (f))))

(test-assert "define-restartable 3"
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con #t))
  (lambda ()
    (define-restartable f
      (lambda (x)
        (if (not x)
            (error 'f "false")
            x)))
    (f #f))))

(test-equal "restartable 1"
 '(2 3 4 5)
 (with-exception-handler
  (lambda (con) (restart/tag 'use-arguments con 3))
  (lambda ()
    (map (restartable
          "anonymous"
          (lambda (x)
	    (if x (+ x 1) (error 'no-one "false"))))
	 '(1 2 #f 4)))))

(test-end)

(exit (test-runner-fail-count (test-runner-current)))
