; SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
;
; SPDX-License-Identifier: CC0-1.0

#lang racket

(define (string->shape s)
  (match s
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]
    ["X" 'rock]
    ["Y" 'paper]
    ["Z" 'scissors]))

(define (string->hint line)
  (map string->shape (string-split line)))

(define (read-file file)
  (with-input-from-file file
    (lambda ()
      (for/fold ([hints null])
                ([l (in-lines)])
        (cons (string->hint l) hints)))))

(define (shape-score round)
  (match (second round)
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (won? round)
  (match round
    [(list 'scissors 'rock) #t]
    [(list 'paper 'scissors) #t]
    [(list 'rock 'paper) #t]
    [_ #f]))

(define (outcome-score round)
  (cond
    [(equal? (first round) (second round)) 3]
    [(won? round) 6]
    [else 0]))

(define (score round)
  (+ (outcome-score round) (shape-score round)))

(display (foldl + 0 (map score (read-file "input"))))
