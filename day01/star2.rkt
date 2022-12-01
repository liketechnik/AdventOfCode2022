; SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
;
; SPDX-License-Identifier: CC0-1.0

#lang racket

(define (read-file file)
  (with-input-from-file file
    (lambda ()
      (for/fold ([food-items (list null)])
                ([l (in-lines)])
        (if (non-empty-string? l)
            (cons (cons (string->number l) (first food-items)) (rest food-items))
            (cons null food-items))))))

(define (lists->sums l)
  (map (lambda (l) (foldl + 0 l)) l))

(define (biggest-three l)
  (take (sort l >) 3))

(display (foldl + 0 
           (biggest-three 
             (lists->sums 
               (read-file "input")))))
