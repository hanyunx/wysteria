#lang racket
(require rackunit)
(require "poker_datadefs.rkt")
(provide (all-defined-out))

;;hand? : Any -> Boolean
;;Is this thing a hand?
(define (hand? x)
  (or (empty? x)
      (and (cons? x)
           (andmap card? x))))
(module+ test
  (check-false
   (hand? #t)
   "Testing hand?")
  (check-true
   (hand? '())
   "Testing hand?")
  (check-true
   (hand? '((#f 1 13) (#t 2 5) (#f 3 7)))
   "Testing hand?"))

;;card? : Any -> Boolean
;;Is this thing a card?
(define (card? x)
  (and (cons? x)
       (= (length x) 3)
       (boolean? (first x))
       (csuit? (second x))
       (cval? (third x))))
(module+ test
  (check-false
   (card? 'notacard)
   "Testing card?")
  (check-true
   (card? '(#f 1 5))
   "Testing card?")
  (check-false
   (card? '(#t 13 13))
   "Testing card?"))

;;csuit? : Any -> Boolean
;;Is this thing a card suit?
(define (csuit? x)
  (and (number? x)
       (in-range? x 1 4)))
(module+ test
  (check-false
   (csuit? #t)
   "Testing csuit?")
  (check-false
   (csuit? 13)
   "Testing csuit?")
  (check-true
   (csuit? 1)
   "testing csuit?"))

;;cval? : Any -> Boolean
;;Is this thing a card value?
(define (cval? x)
  (and (number? x)
       (in-range? x 1 13)))
(module+ test
  (check-false
   (cval? #t)
   "Testing cval?")
  (check-false
   (cval? 15)
   "Testing csuit?")
  (check-true
   (cval? 12)
   "testing csuit?"))

;;in-range? : Number Number Number -> Boolean
;;Is x in the range [lo, hi]
(define (in-range? x lo hi)
  (and (<= x hi) (>= x lo)))
(module+ test
  (check-true
   (in-range? 3 3 4)
   "Testing in-range?")
  (check-true
   (in-range? 3 1 3)
   "Testing in-range?")
  (check-true
   (in-range? 3 1 5)
   "Testing in-range?")
  (check-false
   (in-range? 3 5 7)
   "Testing in-range?"))

;;endclient? : Any -> Boolean
;;Is this an EndClient?
(define (endclient? x)
  (and (cons? x)
       (= (length x) 2)
       (string? (first x))
       (hand? (second x))))
(module+ test
  (check-false
   (endclient? '())
   "Testing endclient?")
  (check-true
   (endclient? '("Alice" ()))
   "Testing endclient?")
  (check-false
   (endclient? '("Alice" (#f 1 1)))
   "Testing endclient?"))

;;posn-match-range? : Nat Nat Nat Nat Number Number -> Boolean
;;Are x and y within the range of targetx and targety for the given dx and dy
(define (posn-match-range? x y targetx targety dx dy)
  (and (<= x (+ targetx (/ dx 2)))
       (>= x (- targetx (/ dx 2)))
       (<= y (+ targety (/ dy 2)))
       (>= y (- targety (/ dy 2)))))
(module+ test
  (check-true
   (posn-match-range? 10 10 20 20 100 200)
   "Testing posn-match-range?")
  (check-false
   (posn-match-range? 10 10 20 20 5 7)
   "Testing posn-match-range?"))

;;card=? : Card Card -> Boolean
;;Are these two cards the same?
(define (card=? a b)
  (and (= (second a) (second b))
       (= (third a) (third b))))
(module+ test
  (check-true
   (card=? (list #t 3 4)
           (list #t 3 4))
   "Testing card=?")
  (check-true
   (card=? (list #t 3 4)
           (list #f 3 4))
   "Testing card=?")
  (check-false
   (card=? (list #t 3 7)
           (list #t 3 4))
   "Testing card=?"))

;;member=? : X [List Y] [X Y -> Boolean] -> Boolean
;;Is this value a member of the given list according to the given definition of equality?
(define (member=? x lox equality)
  (ormap (λ (y) (equality x y)) lox))
(module+ test
  (check-true
   (member=? 1 '(1 2 3) =)
   "Testing member=?")
  (check-true
   (member=? 1 '("a" "ab" "abc")
             (λ (x y) (= x (string-length y))))
   "Testing member=?")
  (check-false
   (member=? 1 '(a b c)
             (λ (x y) (symbol? x)))
   "Testing member=?"))

;;all-consecutive? : [List Number] -> Boolean
;;Are all these numbers consecutive?
;;ASSUMPTION: The given list is sorted in order from lowest to highest
(define (all-consecutive? lon)
  (if (empty? lon) true
      (local [(define allindexes (build-list (length lon) identity))
              (define value1 (first lon))
              (define allsubtract (map (λ (n) (- n value1)) lon))]
        (andmap (λ (index) (equal? index
                                   (list-ref allsubtract index)))
                allindexes))))
(module+ test
  (check-true
   (all-consecutive? '(1 2 3))
   "Testing all-consecutive?")
  (check-false
   (all-consecutive? '(2 3 5))
   "Testing all-consecutive?"))

;;make-one-list : [List [List X]] -> [List X]
;;Flattens one layer of the list
(define (make-one-list l)
  (foldr append '() l))
(module+ test
  (check-equal?
   (make-one-list '((a b c) (d e f)))
   '(a b c d e f)
   "Testing make-one-list.")
  (check-equal?
   (make-one-list '()) '()
   "Testing make-one-list."))

;;find-string-before : String String -> String
;;Find the substring of long that comes before short
;;NOTE: If short is not a substring of long, produce long
(define (find-string-before long short)
  (local [(define long-len (string-length long))
          (define short-len (string-length short))]
    (cond [(< long-len short-len) long]
          [(or (string=? long short)
               (string=? (substring long 0 short-len) short)) ""]
          [else
           (string-append (substring long 0 1)
                          (find-string-before
                           (substring long 1) short))])))
(module+ test
  (check-equal?
   (find-string-before "tenant" "ten") ""
   "Testing find-string-before.")
  (check-equal?
   (find-string-before "hello world!" "!")
   "hello world"
   "Testing find-string-before.")
  (check-equal?
   (find-string-before "hi" "hi") ""
   "Testing find-string-before.")
  (check-equal?
   (find-string-before "hello" "hi")
   "hello" "Testing find-string-before."))

;;find-string-after : String String -> String
;;Find the substring of long that comes after short
;;NOTE: If short is not a substring of long, produce the empty string
(define (find-string-after long short)
  (local [(define long-len (string-length long))
          (define short-len (string-length short))]
    (cond [(or (< long-len short-len)
               (string=? long short)) ""]
          [(string=? (substring long 0 short-len) short)
           (substring long short-len)]
          [else
           (find-string-after
            (substring long 1) short)])))
(module+ test
  (check-equal?
   (find-string-after "tenant" "ten")
   "ant" "Testing find-string-after.")
  (check-equal?
   (find-string-after "hello world!" "!") ""
   "Testing find-string-after.")
  (check-equal?
   (find-string-after "hi" "hi") ""
   "Testing find-string-after.")
  (check-equal?
   (find-string-after "hello" "hi") ""
   "Testing find-string-after."))

;;match-elem : [List X] [X -> Boolean] -> X
;;Produces the first element in this list that satisfies
;; the given predicate
(define (match-elem l pred)
  (cond [(empty? l)
         (error "match-elem : No such element in this list.")]
        [(pred (first l)) (first l)]
        [else (match-elem (rest l) pred)]))
(module+ test
  (check-equal?
   (match-elem '(a b c) (λ (s) (= (string-length (symbol->string s)) 1))) 'a
   "Testing match-elem.")
  (check-equal?
   (match-elem '(apple banana carrot) (λ (s) (string=? (substring (symbol->string s) 0 1) "c")))
   'carrot "Testing match-elem."))