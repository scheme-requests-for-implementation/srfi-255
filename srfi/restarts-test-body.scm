;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

(test-begin "Restarts")

(test-group "Predicate"
 (test-equal #t (restarter? (make-restarter 'a '("a") values))))

(test-group "Accessors"
 (test-equal 'test-restart
             (restarter-tag (make-restarter 'test-restart
                                            '("test")
                                            values)))
 (test-equal '("test")
             (restarter-description
              (make-restarter 'test-restart '("test") values))))

(test-group "restart"
 (define restarter
   (make-restarter 'test-restart
                   '("test")
                   (lambda args (cons 'test args))))

 (test-equal '(test) (restart restarter))
 (test-equal '(test 1 2 3) (restart restarter 1 2 3)))

(test-group "with-restarters, find-restarter, collect-restarters"
 (define a1 (make-restarter 'r1 '("test") (lambda args args)))
 (define a2 (make-restarter 'r2 '("test") (lambda args args)))
 (define r1 (make-restarter 'r1 '("test") (lambda args args)))
 (define r2 (make-restarter 'r2 '("test") (lambda args args)))

 (with-restarters
  a1
  (lambda ()
    (with-restarters
     a2
     (lambda ()
       (let ((rs (collect-restarters '())))
         (test-assert (or (equal? rs (list a1 a2))
                          (equal? rs (list a2 a1)))))))))

 (with-restarters a1
                  (lambda ()
                    (test-eqv a1 (find-restarter 'r1 '()))))

 (with-restarters a1
                  (lambda ()
                    (test-eqv r1 (find-restarter 'r1 r1))))

 (with-restarters (list a1 a2)
                  (lambda ()
                    (define collected (collect-restarters (list r1)))
                    (test-eqv #t (and (memv r1 collected) #t))
                    (test-eqv #t (and (memv a2 collected) #t))
                    (test-assert (null? (cddr collected)))))

  ;; Ensure collect-restarters prioritizes restarters passed to it.
  (let* ((a1* (make-restarter 'r1 '("this is not a test") list))
         (thunk
          (lambda ()
            (test-eqv a1* (find-restarter 'r1 (collect-restarters a1*))))))
    (with-restarters a1 thunk)))

(test-group "default interactor"
 (define a1
   (make-restarter 'a1
                   '("ambient1")  ; by brian eno
                   (lambda args
                     (error 'interactor-test "shouldn't get here"))))

 (define r1
   (make-restarter 'r1
                   '("restarter1" "param")
                   (lambda (arg) arg)))

 ;; Choose invalid restarter, choose 1st restarter, then
 ;; supply parameter
 (let ((input-port (open-input-string "frobnitz\nr1\nfoo\n")))
   (test-eqv
    'foo
    (parameterize ((current-input-port input-port)
                   (current-output-port (open-output-string))) ; dummy
      (with-restarters a1
                       (lambda () (restart-interactively r1)))))))

(test-end)
