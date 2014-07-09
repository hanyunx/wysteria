#lang racket
(require 2htdp/universe)
(require "poker_datadefs.rkt")
(provide (all-defined-out))
#|Contents:
- Cards and Hands
- Worlds
- Servers|#

#|Example Data: Cards and Hands|#
;;Cards
(define CARD-D10 (list #f 3 10))
(define CARD-CK (list #f 4 13))
(define CARD-C9 (list #f 4 9))
(define CARD-CA (list #f 4 1))
(define CARD-HA (list #f 1 1))
(define CARD-S3 (list #f 2 3))
(define CARD-S7 (list #f 2 7))
(define CARD-C3 (list #f 4 3))
(define CARD-C8 (list #f 4 8))
(define CARD-DQ (list #f 3 12))
;;Hands
(define HAND1 (list CARD-D10 CARD-CK CARD-C9 CARD-CA CARD-HA))
(define HAND2 (list CARD-S3 CARD-S7 CARD-C3 CARD-C8 CARD-DQ))
;;Ranked Hands
(define HAND-RANK10
  (list CARD-HA (list #f 4 12) CARD-CK (list #f 4 11) (list #f 4 10)))
(define HAND-RANK9
  (list CARD-S3 CARD-S7 (list #f 2 4) (list #f 2 6) (list #f 2 5)))
(define HAND-RANK8
  (list (list #f 1 2) (list #f 2 2)
        (list #f 3 2) (list #f 4 2) (list #f 2 5)))
(define HAND-RANK7
  (list CARD-S3 (list #f 1 6) (list #f 1 3)
        (list #f 3 6) (list #f 3 3)))
(define HAND-RANK6
  (list (list #f 4 2) (list #f 4 7)
        (list #f 4 4) CARD-CK CARD-C9))
(define HAND-RANK5
  (list CARD-C9 (list #f 1 5) CARD-C8
        (list #f 3 6) CARD-S7))
(define HAND-RANK4
  (append (list CARD-DQ CARD-C8) (build-list 3 (λ (i) (list #f (add1 i) 4)))))
(define HAND-RANK3
  (list CARD-DQ (list #f 1 12) CARD-HA
        (list #f 1 5) (list #f 2 5)))

#|Example Data: Worlds|#
(define WORLD-DEAL0
  (make-wdeal "Alice" '() '(("Alice" #t) ("Bob" #t) ("Charlie" #t))))
(define WORLD-DEAL1
  (make-wdeal "Alice" (rest HAND1) '(("Alice" #t) ("Bob" #t) ("Charlie" #t))))

(define WORLD-DB0
  (make-wdb "Alice" '(("Alice" #t) ("Bob" #t) ("Charlie" #t))))

(define WORLD-WAIT0
  (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t))
              '() 1 3 1 0))
(define WORLD-WAIT1
  (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
              '("Some actions.") 2 5 3 1))
(define WORLD-WAIT2
  (make-wwait "Bob" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
              '("Some actions.") 2 5 1 1))

(define WORLD-MOVE0
  (make-wmove "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) '("Some actions.") 2 5 3 1))

(define WORLD-BET0
  (make-wbet "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) '("Some actions.") 2 5 3 1 5))

#|Example Data: Servers|#
(define SERVER-BEGIN0
  (make-sbegin #f #f '() #f #f))
(define SERVER-BEGIN1
  (make-sbegin iworld1 #f '() #f  #f))
(define SERVER-BEGIN2
  (make-sbegin iworld1 2 '() #f  #f))
(define SERVER-BEGIN3
  (make-sbegin iworld1 2 '("Alice" "Bob") #f  #f))
(define SERVER-BEGIN4
  (make-sbegin iworld1 2 '("Alice" "Bob") "Alice"  #f))

(define SERVER-DEAL0
  (make-sdeal #f '()))
(define SERVER-DEAL1
  (make-sdeal iworld1 (rest HAND1)))
(define SERVER-DEAL2
  (make-sdeal iworld2 HAND1))

(define SERVER-PLAY0
  (make-splay iworld1 HAND1 #t))
(define SERVER-PLAY1
  (make-splay iworld2 HAND2 #f))