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

(define (total-score rounds)
  (foldl + 0 (map score rounds)))

(module+ main
  (display (total-score (read-file "input"))))

(module+ test
  (require rackunit)

  (check-equal?
    (losing-shape 'rock)
    'scissors)
  (check-equal?
    (losing-shape 'scissors)
    'paper)
  (check-equal?
    (losing-shape 'paper)
    'rock)

  (check-equal?
    (winning-shape 'scissors)
    'rock)
  (check-equal?
    (winning-shape 'paper)
    'scissors)
  (check-equal?
    (winning-shape 'rock)
    'paper)

  (check-equal?
    (string->hint '("A" "X"))
    '(rock lose))

  (check-equal? 
    (reverse (read-file "simple")) 
    '((rock draw) (paper lose) (scissors win)))

  (check-equal?
    (hint->shape '(rock draw))
    'rock)
  (check-equal?
    (hint->shape '(paper lose))
    'rock)
  (check-equal?
    (hint->shape '(scissors win))
    'rock)

  (check-equal?
    (shape-score '(rock draw))
    1)
  (check-equal?
    (shape-score '(paper lose))
    1)
  (check-equal?
    (shape-score '(scissors win))
    1)

  (check-equal?
    (outcome-score '(rock draw))
    3)
  (check-equal?
    (outcome-score '(paper lose))
    0)
  (check-equal?
    (outcome-score '(scissors win))
    6)

  (check-equal?
    (score '(rock draw))
    4)
  (check-equal?
    (score '(paper lose))
    1)
  (check-equal?
    (score '(scissors win))
    7)

  (check-equal?
    (total-score '((rock draw) (paper lose) (scissors win)))
    12)

  )
