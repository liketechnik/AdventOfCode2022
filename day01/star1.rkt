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

(define (max-sum l)
  (apply max (map (lambda (l) (foldl + 0 l)) l)))

(display (max-sum (read-file "input")))
