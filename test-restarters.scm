;;; SPDX-FileCopyrightText: 2024 Wolfgang Corcoran-Mathe, Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi :64)
        (srfi :255))

;; Invoke the restarter with tag *tag* on the args, if
;; such a restarter is found.
(define (restart/tag tag restarters . args)
  (let ((r (find (lambda (r) (eqv? tag (restarter-tag r)))
                 restarters)))
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
    (with-interactor
     (lambda (rs) (restart/tag 'use-value rs #t))
     (lambda ()
       (raise-continuable
        (make-restarter 'use-value "Return x." 'foo '(x) k)))))))

(test-assert "restarter objects 2"
 (call-with-current-continuation
  (lambda (k)
    (with-interactor
     (lambda (rs) (restart/tag 'use-not-value rs #f))
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
    (with-interactor
     (lambda (rs)
       (restart/tag 'dump-restarter-data rs))
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
 (with-interactor
  (lambda (rs) (restart/tag 'return-true rs))
  (lambda ()
    (restarter-guard
     (((return-true) "Return #t." #t))
     (error 'no-one "something happen!")))))

(test-equal "restarter-guard 2"
 '(1 2 3)
 (with-interactor
  (lambda (rs) (restart/tag 'return-values rs 1 2 3))
  (lambda ()
    (restarter-guard
     (((return-values . vs) "Return vs." vs))
     (error 'no-one "something happen!")))))

(test-assert "restartable 1"
 (with-interactor
  (lambda (rs) (restart/tag 'use-arguments rs #t))
  (lambda ()
    (restartable
     define (f x)
       (if (not x)
           (error 'f "false")
	   x))
    (f #f))))

(test-equal "restartable 2"
 '(1 2)
 (with-interactor
  (lambda (rs) (restart/tag 'use-arguments rs 1 2))
  (lambda ()
    (restartable
     define (f . xs)
       (if (null? xs)
           (error 'f "empty")
	   xs))
    (f))))

(test-assert "restartable 3"
 (with-interactor
  (lambda (rs) (restart/tag 'use-arguments rs #t))
  (lambda ()
    (restartable
     define f
       (lambda (x)
         (if (not x)
	     (error 'f "false")
	     x)))
    (f #f))))

(test-equal "restartable 4"
 '(2 3 4 5)
 (with-interactor
  (lambda (rs) (restart/tag 'use-arguments rs 3))
  (lambda ()
    (map (restartable
          "anonymous"
          (lambda (x)
	    (if x (+ x 1) (error 'no-one "false"))))
	 '(1 2 #f 4)))))

(test-assert "with-abort-restarter 1"
 (with-interactor
  (lambda (rs) (restart/tag 'abort rs))
  (lambda ()
    (with-abort-restarter
     (lambda ()
       (raise-continuable (make-message-condition "hi"))
       #f))
    #t)))
(test-end)

(exit (test-runner-fail-count (test-runner-current)))
