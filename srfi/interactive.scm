;; SPDX-FileCopyrightText: 2024 Arvydas Silanskas, Wolfgang Corcoran-Mathe
;; SPDX-License-Identifier: MIT

(define (default-interactor restarters)
  ;; Like find-restarter, but only searches its argument list.
  (define (find-local-restarter tag rs)
    (find (lambda (r) (eqv? tag (restarter-tag r))) rs))

  (define (display-choices)
    (display "The following actions are available:")
    (newline)
    (for-each
     (lambda (r)
       (display (restarter-tag r))
       (display ": ")
       (display (car (restarter-description r)))
       (newline))
     restarters)
    (display "Which action do you choose: "))

  (define (read-choice)
    (let ((choice (read)))
      (or (find-local-restarter choice restarters)
          (begin
           (display "Invalid choice. Try again.")
           (newline)
           (read-choice)))))

  (define (read-restarter-params restarter)
    (unfold null?
            (lambda (ds)
              (begin
               (display (car ds))
               (display ": ")
               (read)))
            cdr
            (cdr (restarter-description restarter))))

  (display-choices)
  (let* ((restarter (read-choice))
         (params (read-restarter-params restarter)))
    (apply restart restarter params)))

(define (restart-interactively restarters)
  ((interactor) (collect-restarters restarters)))

(define interactor (make-parameter default-interactor))
