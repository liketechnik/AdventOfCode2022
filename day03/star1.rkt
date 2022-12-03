; SPDX-FileCopyrightText: 2022 Florian Warzecha <liketechnik@disroot.org>
;
; SPDX-License-Identifier: CC0-1.0

#lang racket

(define (item->priority item)
  (if (char-upper-case? item)
    (+ (- (char->integer item) (char->integer #\A)) 27)
    (+ (- (char->integer item) (char->integer #\a)) 1)))

(define (line->rucksack line)
  (let* ([split-pos (quotient (string-length line) 2)]
         [compartment1 (substring line 0 split-pos)]
         [compartment2 (substring line split-pos)])
    (list compartment1 compartment2)))

(define (common-item rucksack)
  (for*/first ([i1 (string->list (first rucksack))]
               [i2 (string->list (second rucksack))]
               #:when (equal? i1 i2))
    i1))

(define (total-priority rucksacks)
  (foldl + 0 
    (map 
      (lambda (rucksack) 
        (item->priority (common-item rucksack))) 
      rucksacks)))

(define (read-file file)
  (with-input-from-file file
    (lambda ()
      (for/fold ([rucksacks null])
                ([l (in-lines)])
        (cons (line->rucksack l) rucksacks)))))

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
    (line->rucksack "vJrwpWtwJgWrhcsFMMfFFhFp")
    '("vJrwpWtwJgWr" "hcsFMMfFFhFp"))

  (check-equal?
    (common-item '("vJrwpWtwJgWr" "hcsFMMfFFhFp"))
    #\p)

  (check-equal?
    (total-priority (map line->rucksack '("vJrwpWtwJgWrhcsFMMfFFhFp"
                      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                      "PmmdzqPrVvPwwTWBwg"
                      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                      "ttgJtRGJQctTZtZT"
                      "CrZsJsPPZsGzwwsLwLmpwMDw")))
    157)

  )
