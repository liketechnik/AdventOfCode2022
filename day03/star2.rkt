; SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
;
; SPDX-License-Identifier: CC0-1.0

#lang racket

(define (item->priority item)
  (if (char-upper-case? item)
    (+ (- (char->integer item) (char->integer #\A)) 27)
    (+ (- (char->integer item) (char->integer #\a)) 1)))

(define (common-item group)
  (for*/first ([i1 (string->list (first group))]
               [i2 (string->list (second group))]
               [i3 (string->list (third group))]
               #:when (and (equal? i1 i2) (equal? i2 i3)))
    i1))

(define (total-priority groups)
  (foldl + 0
    (map
      (lambda (group)
        (item->priority (common-item group)))
      groups)))

(define (read-file file)
  (with-input-from-file file
    (lambda()
      (for/fold ([groups null])
                ([i (in-naturals)]
                 [l (in-lines)])
        (if (= 0 (modulo i 3)) 
          (list* (list l) groups)
          (list* (list* l (first groups)) (rest groups)))))))

(module+ main
  (display (total-priority (read-file "input"))))

(module+ test
  (require rackunit)

  (check-equal? 
    (item->priority #\a)
    1)
  (check-equal? 
    (item->priority #\z)
    26)
  (check-equal? 
    (item->priority #\A)
    27)
  (check-equal? 
    (item->priority #\Z)
    52)

  (check-equal?
    (common-item '("vJrwpWtwJgWrhcsFMMfFFhFp"
                   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                   "PmmdzqPrVvPwwTWBwg"))
    #\r)

  (check-equal?
    (total-priority '(("vJrwpWtwJgWrhcsFMMfFFhFp"
                   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                   "PmmdzqPrVvPwwTWBwg")
                      ("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                       "ttgJtRGJQctTZtZT"
                       "CrZsJsPPZsGzwwsLwLmpwMDw")))
    70)

  (check-equal?
    (reverse (map reverse (read-file "simple")))
    '(("vJrwpWtwJgWrhcsFMMfFFhFp"
       "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
       "PmmdzqPrVvPwwTWBwg")
      ("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
       "ttgJtRGJQctTZtZT"
       "CrZsJsPPZsGzwwsLwLmpwMDw")))

  )
