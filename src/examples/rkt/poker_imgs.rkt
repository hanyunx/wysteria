#lang racket
(require 2htdp/image)
(require rackunit)
(require "poker_datadefs.rkt")
(require "poker_basicfunctions.rkt")
(require "poker_examples.rkt")
(provide (all-defined-out))

;;--- [BEGIN CONSTANTS] ---
(define BACKGROUND (empty-scene 600 500))
(define POKER-TEXT-IMG
  (beside (text "P" 50 "black") (text "O" 50 "red")
          (text "K" 50 "black") (text "E" 50 "red")
          (text "R" 50 "black")))
(define THICK-LINE-PEN (make-pen "black" 5 "solid" "round" "round"))
(define GAME-BOARD-BACKGROUND
  (add-line
   (add-line BACKGROUND 400 0 400 500
            THICK-LINE-PEN)
   0 100 400 100 THICK-LINE-PEN))
(define SECTION-BASE-IMG (rectangle 360 100 "solid" "white"))
(define THIN-SECTION-BASE-IMG (rectangle 360 40 "solid" "white"))
(define CARD-BORDER-IMG
  (overlay (rectangle 60 80 "solid" "white")
           (rectangle 62 82 "solid" "red")))
(define BOLD-CARD-BORDER-IMG
  (overlay (rectangle 60 80 "solid" "white")
           (rectangle 64 84 "solid" "black")))
(define FACE-CARDS (list "J" "Q" "K"))
(define ALL-CARD-POSNS
  (build-list 5 (λ (index) (make-posn (+ 60 (* 70 index)) 250))))
(define ALL-ADJUSTED-POSNS
  (build-list 5 (λ (i) (make-posn (+ (* 70 i) 60) 245))))
(define SUIT-IMG-HEARTS
  (scale 20/37
         (overlay/xy
          (overlay/xy
           (circle 10 "solid" "red")
           15 0 (circle 10 "solid" "red"))
          -1 10
          (overlay/xy
           (rectangle 37 5 "solid" "white")
           0 0 (triangle/sss 35 35 37 "solid" "red")))))
(define SUIT-IMG-SPADES
  (scale 20/35
         (overlay/xy
          (overlay/xy
           (overlay/xy
            (circle 10 "solid" "black")
            15 0 (circle 10 "solid" "black"))
           0 -19
           (overlay/xy
            (rectangle 35 7 "solid" "white")
            0 -23 (triangle 35 "solid" "black")))
          14 30 (rectangle 7 20 "solid" "black"))))
(define SUIT-IMG-DIAMONDS
  (scale 20/37
         (above (triangle 37 "solid" "red") (triangle/sss 35 35 37 "solid" "red"))))
(define SUIT-IMG-CLUBS
  (scale 1/2
         (overlay/xy
          (rectangle 7 20 "solid" "black")
          -17 -20
          (overlay/xy
           (circle 10 "solid" "black")
           10 -10
           (overlay/xy (circle 10 "solid" "black")
                       10 10
                       (circle 10 "solid" "black"))))))
(define ALL-SUIT-IMAGES
  (list SUIT-IMG-HEARTS SUIT-IMG-SPADES
        SUIT-IMG-DIAMONDS SUIT-IMG-CLUBS))
(define BLANK-BUTTON-IMG
  (overlay (rectangle 28 18 "solid" "white")
           (rectangle 30 20 "solid" "black")))
(define BLANK-TEXTBOX-IMG
  (rectangle 300 20 "outline" "black"))
(define SECTION-BREAK-IMG
  (rectangle 10 20 "solid" "white"))
(define ACTIONS-BACKGROUND-IMG
  (rectangle 200 200 "solid" "white"))
(define DEFAULT-IMAGE
  (overlay POKER-TEXT-IMG BACKGROUND))
(define DEAL-BTN-IMAGE
  (overlay (text "DEAL" 30 "black")
           (rectangle 100 50 "outline" "black")))
(define WDB-IMAGE
  (overlay DEAL-BTN-IMAGE BACKGROUND))

;;--- [END CONSTANTS] ---

;;--- [BEGIN FUNCTIONS] ---
#|Contents:
1. Main Drawing Functions
2. Card Drawing Functions
3. Button Drawing Functions
4. Action Drawing Functions
5. Misc|#
#|Main Drawing Functions|#
;;deal-draw : String Hand -> Image
;;Draw the cards that have been dealt so far
(define (deal-draw name hand)
  (if (empty? hand)
      DEFAULT-IMAGE
      (place-image
       (create-red/black-text-image name 25)
       200 50
       (draw-cards-at-posns
        hand (build-list
              (length hand)
              (λ (index) (make-posn (- (+ 235 (* 70 index))
                                       (* 35 (length hand))) 350)))
        GAME-BOARD-BACKGROUND))))
(module+ test
  (check-equal?
   (deal-draw "Alice" '())
   DEFAULT-IMAGE
   "Testing deal-draw.")
  (check-equal?
   (deal-draw "Alice" (rest HAND1))
   (place-image
    (create-red/black-text-image "Alice" 25)
    200 50
    (draw-cards-at-posns (rest HAND1)
                         (list (make-posn 95 350)
                               (make-posn 165 350)
                               (make-posn 235 350)
                               (make-posn 305 350))
                         GAME-BOARD-BACKGROUND))
   "Testing deal-draw."))

;;wait-draw : String Hand [List String] Nat Nat -> Image
;;Draw the player's waiting stage
(define (wait-draw name hand actions bid pool)
  (draw-actions
   (cons "Waiting for other players to play." actions)
   (draw-header name bid pool
                (draw-hand hand GAME-BOARD-BACKGROUND))))
(module+ test
  (check-equal?
   (wait-draw "Alice" HAND1 '() 1 3)
   (draw-actions
    '("Waiting for other players to play.")
    (draw-header
     "Alice" 1 3
     (draw-hand
      HAND1
      GAME-BOARD-BACKGROUND)))
   "Testing wait-draw."))

;;move-draw : String Hand [List String] Nat Nat -> Image
;;Draw the player's name and hand
;;Draw the game actions, current bid, and total bidding pool
(define (move-draw name hand actions bid pool)
  (draw-move-buttons
   bid (draw-actions
        (cons "It is your turn!" actions)
        (draw-header name bid pool
                     (draw-hand hand GAME-BOARD-BACKGROUND)))))
(module+ test
  (check-equal?
   (move-draw "Alice" HAND1 '() 1 3)
   (draw-move-buttons
    1 (draw-actions
       '("It is your turn!")
       (draw-header
        "Alice" 1 3
        (draw-hand HAND1 GAME-BOARD-BACKGROUND))))
   "Testing move-draw."))

;;bet-draw : String Hand [List String] Nat Nat Nat -> Image
;;Draw the player's name, hand, and bid so far
;;Draw the game actions, current bid, and total bidding pool
(define (bet-draw name hand actions bid pool sofar)
  (draw-bet
   sofar (draw-actions
          (append '("It is your turn!"
                    "Press the up and down"
                    "arrows on your keyboard"
                    "to change your bet.")
                  actions)
          (draw-header name bid pool
                       (draw-hand hand GAME-BOARD-BACKGROUND)))))
(module+ test
  (check-equal?
   (bet-draw "Alice" HAND1 '() 1 3 1)
   (draw-bet
    1 (draw-actions
       '("It is your turn!"
         "Press the up and down"
         "arrows on your keyboard"
         "to change your bet.")
       (draw-header
        "Alice" 1 3
        (draw-hand HAND1 GAME-BOARD-BACKGROUND))))
   "Testing bet-draw."))

#|Card Drawing Functions|#
;;value->string : CVal -> String
;;Converts a card value into a string
(define (value->string cv)
  (cond [(or (< cv 0) (> cv 13)) (error "Invalid card value")]
        [(< cv 11) (number->string cv)]
        [else (list-ref FACE-CARDS (- cv 11))]))
(module+ test
  (check-equal? (value->string 10) "10")
  (check-equal? (value->string 11) "J")
  (check-equal? (value->string 12) "Q")
  (check-equal? (value->string 13) "K"))

;;value->image : CVal -> Image
;;Renders a card value as an image
(define (value->image cv)
  (scale-to-width (draw-text (value->string cv) 20) 10))
(module+ test
  (check-equal? (value->image 10)
                (scale-to-width (draw-text "10" 20) 10)))

;;suit->image : CSuit -> Image
;;Renders a card suit as an image
(define (suit->image cs)
  (list-ref ALL-SUIT-IMAGES (sub1 cs)))
(module+ test
  (check-equal? (map suit->image (build-list 4 add1))
                ALL-SUIT-IMAGES))

;;draw-card : Card -> Image
;;Renders a card as an image
(define (draw-card c)
  (place-image
   (value->image (third c))
   15 40
   (place-image
    (suit->image (second c))
    40 40
    (if (first c) BOLD-CARD-BORDER-IMG
        CARD-BORDER-IMG))))
(module+ test
  (check-equal?
   (draw-card (list #f 4 13))
   (place-image (scale-to-width (draw-text "K" 20) 10)
                15 40
                (place-image SUIT-IMG-CLUBS
                             40 40 CARD-BORDER-IMG))))

;;draw-cards-at-posns : [List Card] [List Posn] Image -> Image
;;Renders the given cards at the given positions
(define (draw-cards-at-posns loc lop baseimg)
  (cond [(empty? loc) baseimg]
        [else (place-image (draw-card (first loc))
                           (posn-x (first lop))
                           (posn-y (first lop))
                           (draw-cards-at-posns
                            (rest loc)
                            (rest lop)
                            baseimg))]))
(module+ test
  (check-equal? (draw-cards-at-posns empty empty SECTION-BASE-IMG)
                SECTION-BASE-IMG)
  (check-equal?
   (draw-cards-at-posns HAND1 ALL-CARD-POSNS BACKGROUND)
   (place-image
    (draw-card CARD-D10) 60 250
    (place-image
     (draw-card CARD-CK) 130 250
     (place-image
      (draw-card CARD-C9) 200 250
      (place-image
       (draw-card CARD-CA) 270 250
       (place-image
        (draw-card CARD-HA) 340 250 BACKGROUND)))))
   "Testing draw-cards-at-posns."))

;;draw-hand : [List Card] Image -> Image
;;Renders the hand as an image
(define (draw-hand hand bg-img)
  (draw-cards-at-posns hand ALL-CARD-POSNS bg-img))
(module+ test
  (check-equal?
   (draw-hand HAND1 BACKGROUND)
   (place-image
    (draw-card CARD-D10) 60 250
    (place-image
     (draw-card CARD-CK) 130 250
     (place-image
      (draw-card CARD-C9) 200 250
      (place-image
       (draw-card CARD-CA) 270 250
       (place-image
        (draw-card CARD-HA) 340 250 BACKGROUND)))))
   "Testing draw-hand."))

#|Button Drawing Functions|#
;;draw-button : String -> Image
;;Draws a button with the given text on it
;;Buttons are 30 pixels high and however long it takes to write the text
(define (draw-button txt)
  (local [(define txtimg (draw-text txt 15))]
    (overlay txtimg
             (rectangle
              (+ (image-width txtimg) 6)
              30 "outline" "black"))))
(module+ test
  (check-equal?
   (draw-button "FOLD")
   (overlay (draw-text "FOLD" 15)
            (rectangle 46 30 "outline" "black")))
  (check-equal?
   (draw-button "BET")
   (overlay (draw-text "BET" 15)
            (rectangle 35 30 "outline" "black"))))

;;draw-move-buttons : Number Image -> Image
;;Draws the fold, call (or check), and bet buttons
(define (draw-move-buttons currentbid bg-img)
  (place-image
   (draw-button "FOLD") 90 350
   (place-image
    (draw-button (if (> currentbid 0) "CALL" "CHECK")) 180 350
    (place-image
     (draw-button "BET") 270 350
     bg-img))))
(module+ test
  (check-equal?
   (draw-move-buttons 0 BACKGROUND)
   (place-image
    (draw-button "FOLD") 90 350
    (place-image
     (draw-button "CHECK") 180 350
     (place-image
      (draw-button "BET") 270 350
      BACKGROUND))))
  (check-equal?
   (draw-move-buttons 10 BACKGROUND)
   (place-image
    (draw-button "FOLD") 90 350
    (place-image
     (draw-button "CALL") 180 350
     (place-image
      (draw-button "BET") 270 350
      BACKGROUND)))))

;;draw-bet : Nat Image -> Image
;;Draw the bet
(define (draw-bet bet bg-img)
  (place-image
   (text (format "YOUR BET: ~a" bet) 30 "black")
   200 350 bg-img))
(module+ test
  (check-equal?
   (draw-bet 30 GAME-BOARD-BACKGROUND)
   (place-image
    (text "YOUR BET: 30" 30 "black") 200 350
    GAME-BOARD-BACKGROUND)
   "Testing draw-bet."))

#|Action Drawing Functions|#
;;draw-actions : [List String] Image-> Image
;;Renders the actions as text on the given background
(define (draw-actions actions bg-img)
  (local [(define displayable
            (if (> (length actions) 20)
                (take actions 20) actions))]
    (if (empty? actions) bg-img
        (place-image-starting-y
         (foldr
          (λ (action img)
            (above (shrink-to-width (text action 15 "black") 180) img))
          empty-image
          displayable)
         500 20 bg-img))))
(module+ test
  (check-equal?
   (draw-actions '() BACKGROUND)
   BACKGROUND
   "Testing draw-actions.")
  (check-equal?
   (draw-actions
    '("You folded." "Charlie bet $3.") GAME-BOARD-BACKGROUND)
   (place-image-starting-y
    (above (text "You folded." 15 "black")
           (text "Charlie bet $3." 15 "black"))
    500 20 GAME-BOARD-BACKGROUND)
   "Testing draw-actions."))

#|Misc|#
;;scale-to-width : Image Number -> Image
;;Scales an image to the given width
(define (scale-to-width img n)
  (local [(define imgwidth (image-width img))
          (define fraction (/ n imgwidth))]
    (scale fraction img)))
(module+ test
  (check-equal? (image-width
                 (scale-to-width
                  (first ALL-SUIT-IMAGES) 5)) 5))

;;shrink-to-width : Image Number -> Image
;;If the image width is greater than n, shrink to size
(define (shrink-to-width img n)
  (if (> (image-width img) n)
      (scale-to-width img n)
      img))
(module+ test
  (check-equal?
   (shrink-to-width
    (circle 20 "solid" "blue") 20)
   (scale-to-width
    (circle 20 "solid" "blue") 20)
   "Testing shrink-to-width.")
  (check-equal?
   (shrink-to-width
    (circle 5 "solid" "orange") 20)
   (circle 5 "solid" "orange")
   "Testing shrink-to-width."))

;;scale-to-height : Image Number -> Image
;;Scales an image to the given width
(define (scale-to-height img n)
  (local [(define imgheight (image-height img))
          (define fraction (/ n imgheight))]
    (scale fraction img)))
(module+ test
  (check-equal? (image-height
                 (scale-to-height
                  (second ALL-SUIT-IMAGES) 5)) 5))

;;draw-text : String Number -> Image
;;Draws the image in black text of the indicated size
(define (draw-text txt size)
  (text txt size "black"))
(module+ test
  (check-equal?
   (draw-text "Hello world" 10)
   (text "Hello world" 10 "black"))
  (check-equal?
   (draw-text "Goodbye." 35)
   (text "Goodbye." 35 "black")))

;;draw-bet-instructions : Number -> Image
;;Draw betting instructions
(define (draw-bet-instructions currentbid)
  (overlay
   (above
    (scale-to-width
     (draw-text "Press the up and down arrows on your keyboard to change your bet." 15)
     360)
    (draw-text (number->string currentbid) 20))
   THIN-SECTION-BASE-IMG))
(module+ test
  (check-equal?
   (draw-bet-instructions 3)
   (overlay
    (above
     (scale-to-width
      (draw-text "Press the up and down arrows on your keyboard to change your bet." 15)
      360)
     (draw-text "3" 20))
    THIN-SECTION-BASE-IMG))
  (check-equal?
   (draw-bet-instructions 10)
   (overlay
    (above
     (scale-to-width
      (draw-text "Press the up and down arrows on your keyboard to change your bet." 15)
      360)
     (draw-text "10" 20))
    THIN-SECTION-BASE-IMG)))

;;create-red/black-text-image : String Nat -> Image
;;Creates an image of the given text (of the given size) with alternating red and black text
(define (create-red/black-text-image txt size)
  (foldr (λ (index img) (beside (text (substring txt index (+ index 1)) size
                                      (if (even? index) "black" "red")) img))
         empty-image
         (build-list (string-length txt) identity)))
(define CALCULATION-TEXT-IMG
  (create-red/black-text-image "CALCULATING" 30))
(define CALCULATION-IMAGE
  (overlay CALCULATION-TEXT-IMG BACKGROUND))
(module+ test
  (check-equal?
   (create-red/black-text-image "HI" 20)
   (beside (text "H" 20 "black") (text "I" 20 "red"))
   "Testing create-red/black-text-image.")
  (check-equal?
   (create-red/black-text-image "UpDoWn" 13)
   (beside (text "U" 13 "black")
           (text "p" 13 "red")
           (text "D" 13 "black")
           (text "o" 13 "red")
           (text "W" 13 "black")
           (text "n" 13 "red"))
   "Testing create-red/black-text-image."))

;;draw-header : String Nat Nat Image -> Image
;;Draw the name, bid, and pool, onto this image
(define (draw-header name bid pool background-image)
  (place-image
   (create-red/black-text-image name 25)
   50 50
   (place-image-starting-x
    (text (format "CURRENT BID: ~a" bid) 20 "black")
    200 30
    (place-image-starting-x
     (text (format "BIDDING POOL: ~a" pool) 20 "black")
     200 60 background-image))))
(module+ test
  (check-equal?
   (draw-header "Alice" 20 100 GAME-BOARD-BACKGROUND)
   (place-image
    (create-red/black-text-image "Alice" 25)
    50 50
    (place-image-starting-x
     (text "CURRENT BID: 20" 20 "black")
     200 30
     (place-image-starting-x
      (text "BIDDING POOL: 100" 20 "black")
      200 60 GAME-BOARD-BACKGROUND)))
   "Testing draw-header."))

;;place-image-starting-x : Image Nat Nat Image -> Image
;;Place the given image at the given location (left edge
; of the image should be at the x position given) on the
; given background
(define (place-image-starting-x img start-x y bg-img)
  (local [(define x (+ start-x (/ (image-width img) 2)))]
    (place-image img x y bg-img)))
(module+ test
  (check-equal?
   (place-image-starting-x
    (circle 20 "solid" "red") 0 50
    (square 100 "solid" "orange"))
   (place-image
    (circle 20 "solid" "red") 20 50
    (square 100 "solid" "orange"))
   "Testing place-image-starting-x."))

;;place-image-starting-y : Image Nat Nat Image -> Image
;;Place the given image at the given location (top edge
; of the image should be at the y position given) on the
; given background
(define (place-image-starting-y img x start-y bg-img)
  (local [(define y (+ start-y (/ (image-height img) 2)))]
    (place-image img x y bg-img)))
(module+ test
  (check-equal?
   (place-image-starting-y
    (circle 20 "solid" "red") 50 0
    (square 100 "solid" "orange"))
   (place-image
    (circle 20 "solid" "red") 50 20
    (square 100 "solid" "orange"))
   "Testing place-image-starting-y."))
;;--- [END FUNCTIONS] ---
