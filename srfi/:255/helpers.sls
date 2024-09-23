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

;; Based on MNW's SRFI 241 sample implementation.

(library (srfi :255 helpers)
  (export all-ids? check-unique-ids)
  (import (rnrs base (6))
          (rnrs hashtables (6))
          (rnrs lists (6))
          (rnrs syntax-case (6)))

  (define (all-ids? xs)
    (for-all identifier? xs))

  (define (identifier-hash id)
    (assert (identifier? id))
    (symbol-hash (syntax->datum id)))

  (define (make-identifier-hashtable)
    (make-hashtable identifier-hash bound-identifier=?))

  ;; If *ids* contains duplicate identifiers, signal an error.
  ;; *syn* is the relevant syntax object.
  (define (check-unique-ids who syn ids)
    (let ((id-table (make-identifier-hashtable))
          (mark (lambda (id)
                  (lambda (x)
                    (if x
                        (repeated-identifier-error who syn id)
                        #t)))))
      (for-each (lambda (id)
                  (hashtable-update! id-table id (mark id) #f))
                ids)))

  (define (repeated-identifier-error who syn id)
    (syntax-violation who
                      "duplicate identifier found:"
                      syn
                      id))

  )
