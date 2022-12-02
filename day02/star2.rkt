; SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
;
; SPDX-License-Identifier: CC0-1.0

#lang racket

(define shapes (list 'rock 'scissors 'paper))

(define (losing-shape shape)
    (list-ref shapes (modulo (+ (index-of shapes shape) 1) 3)))

(define (winning-shape shape)
    (list-ref shapes (modulo (- (index-of shapes shape) 1) 3)))

(define (string->shape s)
  (match s
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]))

(define (string->outcome s)
  (match s
    ["X" 'lose]
    ["Y" 'draw]
    ["Z" 'win]))

(define (string->hint parts)
  (list (string->shape (first parts)) (string->outcome (second parts))))

(define (read-file file)
  (with-input-from-file file
    (lambda ()
      (for/fold ([hints null])
                ([l (in-lines)])
        (cons (string->hint (string-split l)) hints)))))

(define (hint->shape round)
  (match round
    [(list shape 'draw) shape]
    [(list shape 'win) (winning-shape shape)]
    [(list shape 'lose) (losing-shape shape)]))

(define (shape-score round)
  (match (hint->shape round)
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (won? round)
  (match (second round)
    ['win #t]
    [_ #f]))

(define (outcome-score round)
  (match (second round)
    ['draw 3]
    ['win 6]
    ['lose 0]))

(define (score round)
  (+ (outcome-score round) (shape-score round)))

(display (foldl + 0 (map score (read-file "input"))))
#| (display (map outcome-score (read-file "simple"))) |#
