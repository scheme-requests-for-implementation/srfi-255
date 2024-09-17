(import (rnrs)
        (srfi :64)
        (srfi :255))

;; Invoke the restarter with tag *tag* on the args, if
;; such a restarter is found.
(define (restart/tag tag restarters . args)
  (let ((rs (memp (lambda (r) (eqv? tag (restarter-tag r)))
                  restarters)))
    (if rs
        (apply restart (car rs) args)
        (error 'restart/tag
               "no restarter found with this tag"
               tag restarters))))

(test-begin "Restarters")
(test-assert "restarter-guard 1"
 (with-interactor
  (lambda (rs) (restart/tag 'return-true rs))
  (lambda ()
    (restarter-guard
     (((return-true) "Return #t." #t))
     (error 'no-one "something happen!")))))
(test-end)
