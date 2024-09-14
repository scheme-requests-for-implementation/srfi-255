;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

(define-syntax assert-type
  (syntax-rules ()
    ((assert-type loc test . args)
     (unless test
       (error loc "type check failed" 'expr . args)))))

(define-record-type <restarter>
  (make-raw-restarter tag description invoker)
  restarter?
  (tag restarter-tag)
  (description restarter-description)
  (invoker restarter-invoker))

;; Exported constructor.
(define (make-restarter tag description invoker)
  (assert-type 'make-restarter-tag (symbol? tag))
  (assert-type 'make-restarter-tag
               (and (pair? description)
                    (every string? description)))
  (assert-type 'make-restarter-tag (procedure? invoker))
  (make-raw-restarter tag description invoker))

(define (restart restarter . args)
  (apply (restarter-invoker restarter) args))

(define ambient-restarters (make-parameter '()))

(define (restarters->list loc restarters allow-compound?)
  ;; Ensure that each element of *restarters* is a
  ;; restarter with a unique tag (with respect to the
  ;; rest of the list).
  (define (check-restarter-list restarters)
    (let loop ((elts restarters) (seen-tags '()))
      (when (pair? elts)
        (let* ((r (car elts)) (tag (restarter-tag r)))
          (assert-type loc (restarter? r) r)
          (when (memv tag seen-tags)
            (error loc "duplicate tag in restarter list" tag))
          (loop (cdr elts) (cons tag seen-tags))))))

  (cond
   ((list? restarters)
    (check-restarter-list restarters)
    restarters)
   ((restarter? restarters) (list restarters))
   ((and allow-compound? (compound? restarters))
    (let* ((subobjs (compound-subobjects restarters))
           (restarters (filter restarter? subobjs)))
      restarters))
   (allow-compound?
    (error loc
           "not a restarter, list of restarters, or a compound object"
           restarters))
   (else
    (error loc "not a restarter or a list of restarters" restarters))))

(define (with-restarters restarters thunk)
  (parameterize ((ambient-restarters (collect-restarters restarters)))
    (thunk)))

(define (find-restarter tag restarters)
  (let ((pred (lambda (restarter)
                (eqv? tag (restarter-tag restarter)))))
    (or (find pred (restarters->list 'find-restarter restarters #t))
        (find pred (ambient-restarters)))))

(define (collect-restarters restarters)
  (let ((list (restarters->list 'collect-restarters restarters #t)))
    (append list
            (lset-difference (lambda (r1 r2)
                               (eqv? (restarter-tag r1)
                                     (restarter-tag r2)))
                             (ambient-restarters)
                             list))))
