#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "poker_datadefs.rkt")
(require "poker_basicfunctions.rkt")
(require "poker_imgs.rkt")
(require "poker_examples.rkt")

#|--- [BEGIN CONSTANTS] ---|#
(define PORT-NUMBER
  (command-line #:args (port) (string->number port)))
(define NUM_CARDS 5)
#|--- [END CONSTANTS] ---|#

#|--- [BEGIN MAIN FUNCTIONS] ---|#
;;run
(define (run)
  (launch-many-worlds
   (run-server)
   (run-world)))
;;Cannot test: big-bang/universe

;;run-server : -> Server
(define (run-server)
  (universe (make-sbegin #f #f '() #f #f)
            (on-tick server-tick)
            (on-new server-new)
            (on-msg server-receive)
            (on-disconnect server-disconnect)
            (port PORT-NUMBER)))
;;Cannot test: universe

;;run-world : String [List String] -> World
(define (run-world)
  (big-bang #f
            (on-mouse world-mouse)
            (on-key world-key)
            (on-receive world-receive)
            (to-draw world-draw)
            (register LOCALHOST)
            (port PORT-NUMBER)))
;;Cannot test: big-bang

#|--- [END MAIN FUNCTIONS] ---|#

#|--- [BEGIN Server EVENT HANDLERS] ---|#
;;server-tick : Server -> Bundle
;;If you haven't gotten all your cards yet, listen for more cards
;;If it is not your turn, listen for a command from Wysteria
(define (server-tick server)
  (cond [(and (sbegin? server) (false? (sbegin-iw server)))
         (make-empty-bundle server)]
        [(and (sbegin? server)
              (false? (sbegin-showbtn? server)))
         (local [(define received-msg (read-line))]
           (display received-msg (current-error-port))
           (sbegin-handle-readline server received-msg))]
        [(sbegin? server)
         (make-bundle
          (make-sdeal (sbegin-iw server) '())
          (list (make-mail (sbegin-iw server)
                           (cons (sbegin-showbtn? server)
                                 (cons (sbegin-myname server)
                                       (sbegin-allplayers server)))))
          '())]
        [(sdeal? server)
         (server-deal-tick server)]
        [(and (splay? server) (splay-check? server))
         (local [(define received-msg (read-line))]
           (display (format "MSG FROM WYSTERIA: ~a \n" received-msg) (current-error-port))
           (server-handle-readline server received-msg))]
        [else (make-empty-bundle server)]))
;;Cannot test CASES 2, 5: read-line
(module+ test
  (check-equal?
   (server-tick SERVER-BEGIN0)
   (make-empty-bundle SERVER-BEGIN0)
   "Testing server-tick.")
  (check-equal?
   (server-tick SERVER-BEGIN4)
   (make-bundle (make-sdeal iworld1 '())
                (list (make-mail iworld1 '("Alice" "Alice" "Bob")))
                '()) "Testing server-tick.")
  (check-equal?
   (server-tick SERVER-DEAL2)
   (server-deal-tick SERVER-DEAL2)
   "Testing server-tick.")
  (check-equal?
   (server-tick SERVER-PLAY1)
   (make-empty-bundle SERVER-PLAY1)
   "Testing server-tick."))

;;server-new : Server IWorld -> Bundle
;;If you have no connections, connect, otherwise discard
(define (server-new server iw)
  (if (and (sbegin? server)
           (false? (sbegin-iw server)))
      ;(begin (displayln 0)
             ;(flush-output (current-output-port))
             (make-empty-bundle
              (make-sbegin iw (sbegin-numplayers server)
                           (sbegin-allplayers server)
                           (sbegin-myname server)
                           (sbegin-showbtn? server)))
      (make-bundle
       server '() (list iw))))
(module+ test
  (check-equal?
   (server-new
    (make-sbegin #f #f '() #f #f) iworld1)
   (make-empty-bundle
    (make-sbegin iworld1 #f '() #f #f))
   "Testing server-new.")
  (check-equal?
   (server-new
    (make-sdeal iworld1 '()) iworld2)
   (make-bundle
    (make-sdeal iworld1 '())
    '() (list iworld2))
   "Testing server-new."))

;;server-receive : ServerPlay IWorld WorldMsg -> Bundle
;;If you receive an action from the player, pass it on to Wysteria
(define (server-receive server iw msg)
  (local [(define to-send
            (cond [(and (symbol? msg)
                        (symbol=? msg 'fold)) 100]
                  [(symbol? msg) 0]
                  [else msg]))]
    #;(write to-send)
    (displayln to-send)
    (display (format "MESSAGE FROM WYSTERIA: ~a \n" to-send) (current-error-port))
    (flush-output (current-error-port))
    (flush-output (current-output-port))
    (make-empty-bundle
     (make-splay (splay-iw server)
                 (splay-hand server) #t))))
;;Cannot test: write

;;server-disconnect : Server IWorld -> Bundle
;;If a player disconnects, let them disconnect
(define (server-disconnect server iw)
  (make-bundle (make-sdeal #f '()) '() (list iw)))
(module+ test
  (check-equal?
   (server-disconnect
    (make-sdeal iworld1 '()) iworld1)
   (make-bundle
    (make-sdeal #f '()) '() (list iworld1))
   "Testing server-disconnect."))

#|--- [END SERVER EVENT HANDLERS] ---|#

#|--- [BEGIN WORLD EVENT HANDLERS] ---|#

;;world-mouse : World Nat Nat MouseEvent -> HandlerResult
;;If it's your turn, you can click the fold, call, or bet buttons
(define (world-mouse world x y me)
  (cond [(and (wmove? world)
              (string=? me "button-down"))
         (move-click world x y)]
        [(and (wdb? world)
              (string=? me "button-down"))
         (dealbtn-click world x y)]
        [else world]))
(module+ test
  (check-equal?
   (world-mouse WORLD-MOVE0 10 10 "button-down")
   (move-click WORLD-MOVE0 10 10)
   "Testing world-mouse.")
  (check-equal?
   (world-mouse WORLD-MOVE0 100 210 "drag")
   WORLD-MOVE0
   "Testing world-mouse.")
  (check-equal?
   (world-mouse WORLD-DB0 10 10 "button-down")
   (dealbtn-click WORLD-DB0 10 10)
   "Testing world-mouse.")
  (check-equal?
   (world-mouse WORLD-BET0 10 13 "button-down")
   WORLD-BET0
   "Testing world-mouse."))

;;world-key : World KeyEvent -> HandlerResult
;;If it's you are betting you can move the bet up/down
(define (world-key world ke)
  (if (wbet? world)
      (bet-key world ke) world))
(module+ test
  (check-equal?
   (world-key WORLD-BET0 "\r")
   (bet-key WORLD-BET0 "\r")
   "Testing world-key.")
  (check-equal?
   (world-key WORLD-MOVE0 "a")
   WORLD-MOVE0
   "Testing world-key."))

;;world-receive : World ServerMsg -> HandlerResult
;;If the server gives you a new hand, update
;;If the server asks for your move, go to the move phase
;;If the server gives you everyone's hands, check who won
(define (world-receive world msg)
  (cond [(and (false? world)
              (string=? (first msg) "\"showbtn\""))
         (wbegin->wdealbtn (rest msg))]
        [(false? world)
         (wbegin->wdeal (rest msg))]
        [(wwait? world)
         (wait-receive world msg)]
        [(and (wcheck? world)
              (string? msg))
         (make-wupdate world msg)]
        [(and (wupdate? world)
              (number? msg))
         (update-receive
          (wupdate-worldstate world)
          (wupdate-name world) msg)]
        [(and (wdeal? world)
              (hand? msg)
              (= (length (wdeal-hand world)) (sub1 NUM_CARDS)))
         ;;deal the last card and switch to wait phase
         (wdeal->wwait (wdeal-name world) msg (wdeal-players world))]
        [(hand? msg)
         ;;update the hand
         (update-world-with-hand world msg)]
        [else world]))
(module+ test
  (check-equal?
   (world-receive WORLD-WAIT0 'go)
   (wait-receive WORLD-WAIT0 'go)
   "Testing world-receive.")
  (check-equal?
   (world-receive WORLD-DEAL1 HAND1)
   (wdeal->wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)))
   "Testing world-receive.")
  (check-equal?
   (world-receive WORLD-DEAL0 (rest HAND1))
   (update-world-with-hand WORLD-DEAL0 (rest HAND1))
   "Testing world-receive.")
  (check-equal?
   (world-receive WORLD-DEAL1 'go)
   WORLD-DEAL1
   "Testing world-receive."))

;;world-draw : World -> Image
;;Render the world as an image (dependent on the phase of the game)
(define (world-draw world)
  (cond [(wdb? world)
         WDB-IMAGE]
        [(wdeal? world)
         (deal-draw (wdeal-name world)
                    (wdeal-hand world))]
        [(wwait? world)
         (wait-draw (wwait-name world)
                    (wwait-hand world)
                    (wwait-actions world)
                    (wwait-bid world)
                    (wwait-pool world))]
        [(wmove? world)
         (move-draw (wmove-name world)
                    (wmove-hand world)
                    (wmove-actions world)
                    (wmove-bid world)
                    (wmove-pool world))]
        [(wbet? world)
         (bet-draw (wbet-name world)
                   (wbet-hand world)
                   (wbet-actions world)
                   (wbet-bid world)
                   (wbet-pool world)
                   (wbet-sofar world))]
        [(wupdate? world)
         (world-draw (wupdate-worldstate world))]
        [(wcheck? world)
         CALCULATION-IMAGE]
        [(string? world)
         (overlay (text world 20 "black") BACKGROUND)]
        [else DEFAULT-IMAGE]))
(module+ test
  (check-equal?
   (world-draw WORLD-DEAL0)
   (deal-draw "Alice" '())
   "Testing world-draw.")
  (check-equal?
   (world-draw WORLD-WAIT0)
   (wait-draw "Alice" HAND1 '() 1 3)
   "Testing world-draw.")
  (check-equal?
   (world-draw WORLD-MOVE0)
   (move-draw "Alice" HAND1 '("Some actions.") 2 5)
   "Testing world-draw.")
  (check-equal?
   (world-draw WORLD-BET0)
   (bet-draw "Alice" HAND1 '("Some actions.") 2 5 5)
   "Testing world-draw.")
  (check-equal?
   (world-draw "You lost.")
   DEFAULT-IMAGE
   "Testing world-draw."))

#|--- [END WORLD EVENT HANDLERS] ---|#

#|--- [BEGIN SERVER HELPER FUNCTIONS] ---
***
Contents:
- Tick Functions
- Message Handling Functions
- Misc
****|#
;[Tick Functions]
;;server-deal-tick : ServerDeal -> Bundle
;;If no world has connected, do nothing
;;If you have not dealt all the cards, deal a card
;;If you have dealt all the cards, switch to the play phase
(define (server-deal-tick server)
  (cond [(false? (sdeal-iw server))
         (make-empty-bundle server)]
        [(< (length (sdeal-hand server)) NUM_CARDS)
         (local [(define siw (sdeal-iw server))
                 (define shand (sdeal-hand server))
                 (define card-msg (read-line))
                 (define TESTSUBJECT-WYSTERIA
                   (display card-msg (current-error-port)))
                 (define new-card
                   (list-ref FULL-DECK (extract-card-message card-msg)))
                 #;(define TESTSUBJECT
                   (displayln new-card))
                 (define new-hand
                   (cons new-card shand))]
           (make-bundle
            (make-sdeal siw new-hand)
            (list (make-mail siw new-hand)) '()))]
        [(= (length (sdeal-hand server)) NUM_CARDS)
         (make-empty-bundle (make-splay (sdeal-iw server) (sdeal-hand server) #t))]
        [else (make-empty-bundle server)]))
;;Cannot test CASE 2: read-line
(module+ test
  (check-equal?
   (server-deal-tick SERVER-DEAL0)
   (make-empty-bundle SERVER-DEAL0)
   "Testing server-deal-tick.")
  (check-equal?
   (server-deal-tick SERVER-DEAL2)
   (make-empty-bundle (make-splay iworld2 HAND1 #t))
   "Testing server-deal-tick."))

;[Message Handling Functions]
;;server-handle-readline : ServerPlay String -> Bundle
;;Handles messages from Wysteria
(define (server-handle-readline server msg)
  (local [(define siw (splay-iw server))
          (define shand (splay-hand server))]
    (cond [(string=? msg "\"getinp\"")
           (make-bundle
            (make-splay siw shand #f)
            (list (make-mail siw 'go)) '())]
          [(string=? msg "\"startcheck\"")
           (make-bundle
            server
            (list (make-mail siw 'check)) '())]
          [(string=? msg "\"endcheck\"")
           (make-empty-bundle siw)]
          [(string->number msg)
           (make-bundle
            server
            (list (make-mail siw (string->number msg))) '())]
          [(is-name-message? msg)
           (make-bundle
            server
            (list (make-mail siw (substring msg 1))) '())]
          [else
           (make-empty-bundle server)])))
(module+ test
  (check-equal?
   (server-handle-readline
    SERVER-PLAY0 "getinp")
   (make-bundle
    (make-splay iworld1 HAND1 #f)
    (list (make-mail iworld1 'go)) '())
   "Testing server-handle-readline.")
  (check-equal?
   (server-handle-readline
    SERVER-PLAY0 "startcheck")
   (make-bundle
    SERVER-PLAY0
    (list (make-mail iworld1 'check)) '())
   "Testing server-handle-readline.")
  (check-equal?
   (server-handle-readline
    SERVER-PLAY0 "!Alice")
   (make-bundle
    SERVER-PLAY0
    (list (make-mail iworld1 "Alice")) '())
   "Testing server-handle-readline.")
  (check-equal?
   (server-handle-readline
    SERVER-PLAY0 "42")
   (make-bundle
    SERVER-PLAY0
    (list (make-mail iworld1 42)) '())
   "Testing server-handle-readline.")
  (check-equal?
   (server-handle-readline
    SERVER-PLAY1 "stop")
   (make-empty-bundle SERVER-PLAY1)
   "Testing server-handle-readline."))

;;sbegin-handle-readline : ServerBegin String -> Bundle
;;Collect information about the game from Wysteria
(define (sbegin-handle-readline server msg)
  (cond [(false? (sbegin-numplayers server))
         (make-empty-bundle
          (make-sbegin (sbegin-iw server)
                       (string->number msg) '() #f #f))]
        [(< (length (sbegin-allplayers server))
            (sbegin-numplayers server))
         (make-empty-bundle
          (make-sbegin (sbegin-iw server)
                       (sbegin-numplayers server)
                       (cons (substring msg 1) (sbegin-allplayers server))
                       #f #f))]
        [(false? (sbegin-myname server))
         (make-empty-bundle
          (make-sbegin (sbegin-iw server)
                       (sbegin-numplayers server)
                       (sbegin-allplayers server)
                       (find-string-before
                              (find-string-after msg "\"!") "\"") #f))]
        [(false? (sbegin-showbtn? server))
         (make-empty-bundle
          (make-sbegin (sbegin-iw server)
                       (sbegin-numplayers server)
                       (sbegin-allplayers server)
                       (sbegin-myname server) msg))]
        [else
         (error "ServerBegin received an unexpected message.")]))
;;TODO: Test

;;extract-card-message : String -> Number
;;Extract the number of the card that has been dealt
(define (extract-card-message msg)
  (string->number
   (find-string-before
    (find-string-after msg ":") "]")))
(module+ test
  (check-equal?
   (extract-card-message
    "[!Alice:10]")
   10 "Testing extract-card-message.")
  (check-equal?
   (extract-card-message
    "[!Bob:5]")
   5 "Testing extract-card-message."))

;;is-name-message? : String -> Boolean
;;Is this message someone's name?
(define (is-name-message? msg)
  (and (> (string-length msg) 1)
       (string=? (substring msg 0 1) "!")))
(module+ test
  (check-true
   (is-name-message? "!Alice")
   "Testing is-name-message?")
  (check-false
   (is-name-message? "Bob")
   "Testing is-name-message?"))

;;[Misc]
;;make-empty-bundle : Server -> Bundle
;;Make a bundle with no mail and no disconnects
(define (make-empty-bundle server)
  (make-bundle server '() '()))
(module+ test
  (check-equal?
   (make-empty-bundle
    (make-sdeal #f '()))
   (make-bundle
    (make-sdeal #f '()) '() '())
   "Testing make-empty-bundle."))
#|--- [END SERVER HELPER FUNCTIONS] ---|#

#|--- [BEGIN WORLD HELPER FUNCTIONS] ---
***
Contents:
- Click Functions
- Key Functions
- Message Handling Functions
- Transition Functions
- Winner Functions
- Hand Ranking Functions
- Misc
***|#
;;[Click Functions]
;;dealbtn-click : WorldDealButton Nat Nat -> HandlerResult
;;If you clicked the deal button, deal
(define (dealbtn-click world x y)
  (if (clicked-button? 'deal x y)
      (begin (displayln 0)
             (flush-output (current-output-port))
             (make-wdeal (wdb-name world) '() (wdb-players world)))
      world))
;;TODO: Test

;;move-click : WorldMove Nat Nat -> HandlerResult
;;If you clicked the fold, call, or bet buttons,
;; react appropriately
(define (move-click world x y)
  (local [(define wname (wmove-name world))
          (define whand (wmove-hand world))
          (define wplayers (wmove-players world))
          (define wactions (wmove-actions world))
          (define wbid (wmove-bid world))
          (define wpool (wmove-pool world))
          (define wmypool (wmove-mypool world))
          (define wagreed (wmove-agreed world))]
    (cond [(clicked-button? 'fold x y)
           (make-package
            (make-wwait
             wname whand (update-players-fold wplayers wname)
             (cons "You folded." wactions)
             wbid wpool wmypool wagreed)
            'fold)]
          [(or (clicked-button? 'call x y)
               (and (zero? wbid) (clicked-button? 'check x y)))
           (make-package
            (make-wwait
             wname whand wplayers (cons "You called." wactions) wbid
             (+ wbid wpool) (+ wbid wmypool) (add1 wagreed))
            'pass)]
          [(clicked-button? 'bet x y)
           (wmove->wbet world)]
          [else world])))
(module+ test
  (check-equal?
   (move-click WORLD-MOVE0 70 340)
   (make-package
    (make-wwait "Alice" HAND1 '(("Alice" #f) ("Bob" #f) ("Charlie" #t))
                '("You folded." "Some actions.") 2 5 3 1) 'fold)
   "Testing move-click.")
  (check-equal?
   (move-click WORLD-MOVE0 185 355)
   (make-package
    (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
                '("You called." "Some actions.") 2 7 5 2) 2)
   "Testing move-click.")
  (check-equal?
   (move-click WORLD-MOVE0 270 350)
   (wmove->wbet WORLD-MOVE0)
   "Testing move-click."))

;;clicked-button? : Symbol Nat Nat -> Boolean
;;Did the player click the given button?
(define (clicked-button? mode x y)
  (local [(define targetx
            (cond [(symbol=? mode 'fold) 90]
                  [(or (symbol=? mode 'call)
                       (symbol=? mode 'check)) 180]
                  [(symbol=? mode 'bet) 270]
                  [(symbol=? mode 'deal) 300]
                  [else (error "clicked-button? : Invalid mode.")]))
          (define targety
            (if (symbol=? mode 'deal) 250 350))
          (define targetwidth
            (cond [(symbol=? mode 'fold) 46]
                  [(symbol=? mode 'call) 42]
                  [(symbol=? mode 'check) 58]
                  [(symbol=? mode 'bet) 35]
                  [(symbol=? mode 'deal) 100]
                  [else (error "clicked-button? : Invalid mode.")]))
          (define targetheight
            (if (symbol=? mode 'deal) 50 30))]
    (posn-match-range? x y targetx targety targetwidth targetheight)))
(module+ test
  (check-true
   (clicked-button? 'fold 100 340)
   "Testing clicked-button?")
  (check-true
   (clicked-button? 'call 190 345)
   "Testing clicked-button?")
  (check-true
   (clicked-button? 'check 180 350)
   "Testing clicked-button?")
  (check-true
   (clicked-button? 'bet 270 349)
   "Testing clicked-button?")
  (check-false
   (clicked-button? 'fold 0 0)
   "Testing clicked-button?"))

;;[Key Functions]
;;bet-key : World KeyEvent -> HandlerResult
(define (bet-key world ke)
  (local [(define wname (wbet-name world))
          (define whand (wbet-hand world))
          (define wplayers (wbet-players world))
          (define wactions (wbet-actions world))
          (define wbid (wbet-bid world))
          (define wpool (wbet-pool world))
          (define wmypool (wbet-mypool world))
          (define wagreed (wbet-agreed world))
          (define wsofar (wbet-sofar world))]
  (cond [(string=? ke "up")
         (make-wbet wname whand wplayers wactions wbid wpool wmypool wagreed (add1 wsofar))]
        [(and (string=? ke "down")
              (> wsofar wbid))
         (make-wbet wname whand wplayers wactions wbid wpool wmypool wagreed (sub1 wsofar))]
        [(string=? ke "\r")
         (make-package
          (make-wwait
           wname whand wplayers (cons (format "You bet $~a." wsofar) wactions)
           wsofar (+ wsofar wpool) (+ wsofar wmypool) (if (> wsofar wbid) 1 (add1 wagreed)))
          wsofar)]
        [else world])))
(module+ test
  (check-equal?
   (bet-key WORLD-BET0 "up")
   (make-wbet "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
              '("Some actions.") 2 5 3 1 6)
   "Testing bet-key.")
  (check-equal?
   (bet-key WORLD-BET0 "down")
   (make-wbet "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
              '("Some actions.") 2 5 3 1 4)
   "Testing bet-key.")
  (check-equal?
   (bet-key WORLD-BET0 "\r")
   (make-package
    (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
                '("You bet $5." "Some actions.") 5 10 8 1) 5)
   "Testing bet-key.")
  (check-equal?
   (bet-key WORLD-BET0 "a") WORLD-BET0
   "Testing bet-key."))

;;[Message Handling Functions]
;;wait-receive : WorldWait ServerMsg -> HandlerResult
;;If you receive a new hand, update
;;If it is your turn to play, play or pass
;;If someone else has played, update
;;If the game is over, find out who won
(define (wait-receive world msg)
  (cond [(hand? msg)
         ;;update the hand
         (update-world-with-hand world msg)]
        [(and (is-playing? (wwait-players world) (wwait-name world))
              (symbol? msg) (symbol=? msg 'go))
         ;;switch to move phase
         (wwait->wmove world)]
        [(and (symbol? msg) (symbol=? msg 'go))
         ;;you have folded, pass
         (make-package world 'pass)]
        [(symbol? msg)
         ;;msg = 'check, begin checking to see who won
         (wwait->wcheck world)]
        [(string? msg)
         ;;this player has done something
         (make-wupdate world msg)]
        [else world]))
(module+ test
  (check-equal?
   (wait-receive WORLD-WAIT0 HAND2)
   (update-world-with-hand WORLD-WAIT0 HAND2)
   "Testing wait-receive.")
  (check-equal?
   (wait-receive WORLD-WAIT0 'go)
   (wwait->wmove WORLD-WAIT0)
   "Testing wait-receive.")
  (check-equal?
   (wait-receive WORLD-WAIT2 'go)
   (make-package WORLD-WAIT2 'pass)
   "Testing wait-receive.")
  (check-equal?
   (wait-receive WORLD-WAIT2 'check)
   (wwait->wcheck WORLD-WAIT2)
   "Testing wait-receive.")
  (check-equal?
   (wait-receive WORLD-WAIT0 "Alice")
   (make-wupdate WORLD-WAIT0 "Alice")
   "Testing wait-receive.")
  (check-equal?
   (wait-receive WORLD-WAIT1 '(this is not a valid msg))
   WORLD-WAIT1 "Testing wait-receive."))

;;update-receive : World String Number -> World
;;If you are waiting, you have received a player's action
;;If you are checking who won, you have received a player's card
;;Update accordingly
(define (update-receive worldstate name action)
  (cond [(and (wwait? worldstate)
              (= action 100))
         (player-fold worldstate name)]
        [(and (wwait? worldstate)
              (zero? action))
         (player-call/pass worldstate name (wwait-bid worldstate))]
        [(wwait? worldstate)
         (player-bet worldstate name action)]
        [(wcheck? worldstate)
         (update-check worldstate name action)]
        [else worldstate]))

;;update-check : WorldCheck String Number -> World
;;Got somebody's card! Update.
(define (update-check world name card-num)
  (local [(define wname (wcheck-name world))
          (define wpool (wcheck-pool world))
          (define wmypool (wcheck-mypool world))
          (define all-data
            (wcheck-playerdata world))
          (define card (list-ref FULL-DECK card-num))
          
          (define playing?
            (ormap (λ (ec) (string=? name (first ec)))
                   all-data))
          (define newdata
            (update-endclient-data
             all-data name card))]
    (cond [(andmap
            (λ (endclient)
              (= (length (second endclient)) NUM_CARDS))
            newdata)
           (wcheck->endgame wname wpool wmypool newdata)]
          [else (make-wcheck wname wpool wmypool newdata)])))
;;TODO: Test

;;[Transition Functions]
;;wbegin->wdealbtn : [List String] -> WorldDealButton
;;Get information from server, display the button to click to deal cards
(define (wbegin->wdealbtn msg)
  (make-wdb (first msg) (map (λ (name) (list name #t)) (rest msg))))
;;TODO: Test

;;wbegin->wdeal : [List String] -> WorldDeal
;;Get information from server and begin the game
(define (wbegin->wdeal msg)
  (make-wdeal (first msg) '() (map (λ (name) (list name #t)) (rest msg))))
(module+ test
  (check-equal?
   (wbegin->wdeal '("Alice" "Alice" "Bob" "Charlie"))
   (make-wdeal "Alice" '() '(("Alice" #t) ("Bob" #t) ("Charlie" #t)))
   "Testing wbegin->wdeal.")
  (check-equal?
   (wbegin->wdeal '("Bob" "Bob"))
   (make-wdeal "Bob" '() '(("Bob" #t)))
   "Testing wbegin->wdeal."))
   

;;wdeal->wwait : String Hand -> WorldWait
;;Switch to the waiting phase
(define (wdeal->wwait name hand players)
  (make-wwait name hand players '() 1 (length players) 1 0))
(module+ test
  (check-equal?
   (wdeal->wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)))
   (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)) '() 1 3 1 0)
   "Testing wdeal->wwait.")
  (check-equal?
   (wdeal->wwait "Bob" HAND-RANK6 '(("Alice" #t) ("Bob" #t)))
   (make-wwait "Bob" HAND-RANK6 '(("Alice" #t) ("Bob" #t)) '() 1 2 1 0)
   "Testing wdeal->wwait."))

;;wwait->wmove : WorldWait -> WorldMove
;;If it's your turn, switch to the move phase
(define (wwait->wmove world)
  (make-wmove
   (wwait-name world)
   (wwait-hand world)
   (wwait-players world)
   (wwait-actions world)
   (wwait-bid world)
   (wwait-pool world)
   (wwait-mypool world)
   (wwait-agreed world)))
(module+ test
  (check-equal?
   (wwait->wmove WORLD-WAIT0)
   (make-wmove "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t))
               '() 1 3 1 0)
   "Testing wwait->wmove.")
  (check-equal?
   (wwait->wmove WORLD-WAIT2)
   (make-wmove "Bob" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
               '("Some actions.") 2 5 1 1)
   "Testing wwait->wmove."))

;;wmove->wbet : WorldMove -> WorldBet
;;This player wants to bet some money
(define (wmove->wbet world)
  (make-wbet
   (wmove-name world) (wmove-hand world)
   (wmove-players world) (wmove-actions world)
   (wmove-bid world) (wmove-pool world)
   (wmove-mypool world) (wmove-agreed world)
   (wmove-bid world)))
(module+ test
  (check-equal?
   (wmove->wbet WORLD-MOVE0)
   (make-wbet "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
               '("Some actions.") 2 5 3 1 2)
   "Testing wmove->wbet."))

;;wwait->wcheck : WorldWait -> WorldCheck
(define (wwait->wcheck world)
  (make-wcheck
   (wwait-name world)
   (wwait-pool world)
   (wwait-mypool world)
   (map (λ (p) (list (first p) '()))
        (filter second (wwait-players world)))))
(module+ test
  (check-equal?
   (wwait->wcheck WORLD-WAIT0)
   (make-wcheck "Alice" 3 1 '(("Alice" ()) ("Bob" ()) ("Charlie" ())))
   "Testing wwait->wcheck.")
  (check-equal?
   (wwait->wcheck WORLD-WAIT2)
   (make-wcheck "Bob" 5 1 '(("Alice" ()) ("Charlie" ())))
   "Testing wwait->wcheck."))

;;wcheck->endgame : String Nat Nat [List EndClient] -> String
;;Received all hands from Wysteria, check who won
(define (wcheck->endgame name pool mypool alldata)
  (local [(define winner (find-winner alldata))
          (define iwon? (string=? name (first winner)))]
    (if iwon? (format "You won $~a!" pool)
        (format "You lost $~a." mypool))))
(module+ test
  (check-equal?
   (wcheck->endgame "Alice" 10 1 (list (list "Alice" HAND-RANK3)
                                       (list "Bob" HAND-RANK7)
                                       (list "Charlie" HAND-RANK4)))
   "You lost $1." "Testing wcheck->endgame.")
  (check-equal?
   (wcheck->endgame "Alice" 5 2 (list (list "Alice" HAND-RANK9)
                                      (list "Bob" HAND-RANK7)
                                      (list "Charlie" HAND-RANK4)))
   "You won $5!" "Testing wwait->endgame."))

;;[Winner Functions]
;;find-winner : [List EndClient] -> EndClient
;;Finds who won the game
(define (find-winner allclients)
  (local [(define allhands (map second allclients))
          (define allranks (map rank-hand allhands))
          (define allinfo
            (map (λ (index) (list (list-ref allclients index)
                                  (list-ref allranks index)))
                 (build-list (length allclients) identity)))
          (define maxrank (if (empty? allclients) 1 (apply max allranks)))
          (define onlymaxrank
            (filter (λ (info) (equal? maxrank (second info))) allinfo))]
    (cond [(empty? onlymaxrank) (error "Nobody won.")]
          [(equal? (length onlymaxrank) 1) (first (first onlymaxrank))]
          [else (find-winner-same-rank (map first onlymaxrank))])))
(module+ test
  (check-equal?
   (find-winner
    (list (list "Alice" HAND-RANK7)
          (list "Bob" HAND-RANK8)
          (list "Charlie" HAND-RANK5)))
    (list "Bob" HAND-RANK8)
    "Testing find-winner.")
  (check-equal?
   (find-winner
    (list (list "Alice" HAND-RANK8)
          (list
           "Bob"
           (list (list #f 1 5) (list #f 2 5)
                 (list #f 3 5) (list #f 4 5) (list #f 2 7)))))
   (list
    "Bob"
    (list (list #f 1 5) (list #f 2 5)
          (list #f 3 5) (list #f 4 5) (list #f 2 7)))
   "Testing find-winner."))

;;find-winner-same-rank : [List EndClient] -> EndClient
;;Finds the winner given that the clients in the list have the same rank
;;ASSUMPTION: There are at least two clients in the list
(define (find-winner-same-rank allplayers)
  (local [(define allhands (map second allplayers))
          (define highesthand (find-highest-hand allhands allhands))
          (define winner (find-client-with-hand allplayers highesthand))]
    winner))
(module+ test
  (check-equal?
   (find-winner-same-rank
    (list (list "Alice" HAND-RANK8)
          (list
           "Bob"
           (list (list #f 1 5) (list #f 2 5)
                 (list #f 3 5) (list #f 4 5) (list #f 2 7)))))
   (list
    "Bob"
    (list (list #f 1 5) (list #f 2 5)
          (list #f 3 5) (list #f 4 5) (list #f 2 7)))
   "Testing find-winner-same-rank."))

;;find-highest-hand : [List Hand] [List [List Card]] -> Hand
;;Finds the hand with the highest card
(define (find-highest-hand original loloc)
  (local [(define onelist (make-one-list loloc))
          (define highestvalue (third (find-highest-card onelist)))
          (define withhighest (filter (λ (hand) (hand-contains-value? highestvalue hand)) loloc))]
    (cond [(empty? withhighest)
           (error "find-highest-hand : No winner found.")]
          [(equal? (length withhighest) 1)
           (match-to-original-hand original (first withhighest))]
          [else
           (find-highest-hand
            original
            (map
             (λ (loc)
               (filter
                (λ (c) (not (equal? (third c) highestvalue)))
                loc)) loloc))])))
(module+ test
  (check-equal?
   (find-highest-hand (list HAND1 HAND2)
                      (list HAND1 HAND2))
   HAND1 "Testing find-highest-hand.")
  (check-equal?
   (find-highest-hand (list HAND-RANK9 HAND-RANK5)
                      (list HAND-RANK9 HAND-RANK5))
   HAND-RANK5 "Testing find-highest-hand."))

;;find-client-with-hand : [List EndClient] [List Card] -> EndClient
;;Find the client with the given hand
;;ASSUMPTION: There is a client in the list with this hand
(define (find-client-with-hand allclients tomatch)
  (cond [(empty? allclients)
         (error "find-client-with-hand : No such client exists.")]
        [(hand-match? (second (first allclients))
                      tomatch) (first allclients)]
        [else (find-client-with-hand (rest allclients) tomatch)]))
(module+ test
  (check-equal?
   (find-client-with-hand
    (list
     (list "Alice" HAND1)
     (list "Bob" HAND2))
     HAND2)
    (list "Bob" HAND2)
    "Testing find-client-with-hand.")
   (check-equal?
    (find-client-with-hand
     (list
      (list "Charlie" HAND-RANK9)
      (list "Bob" HAND-RANK5))
     HAND-RANK9)
    (list "Charlie" HAND-RANK9)
    "Testing find-client-with-hand."))

;;find-highest-card : [List Card] -> Card
;;Finds the highest card in all these cards
(define (find-highest-card allcards)
  (local [(define allvalues (map third allcards))
          (define maxvalue (apply max allvalues))
          (define allmaxvalue (filter (λ (c) (equal? (third c) maxvalue)) allcards))]
    (cond [(empty? allmaxvalue)
           (error "find-highest-card : There was an error computing the highest card.")]
          [else (first allmaxvalue)])))
(module+ test
  (check-equal?
   (find-highest-card HAND1) CARD-CK
   "Testing find-highest-card.")
  (check-equal?
   (find-highest-card HAND2) CARD-DQ
   "Testing find-highest-card."))

;;hand-contains-value? : CVal [List Card] -> Boolean
;;Does the hand contain a card of the given value
(define (hand-contains-value? cval hand)
  (ormap (λ (c) (equal? (third c) cval)) hand))
(module+ test
  (check-true
   (hand-contains-value? 13 HAND1)
   "Testing hand-contains-value?")
  (check-false
   (hand-contains-value? 1 HAND2)
   "Testing hand-contains-value?"))

;;match-to-original-hand : [List [List Card]] [List Card] -> [List Card]
;;Finds the hand that contains the cards given
(define (match-to-original-hand original tomatch)
  (cond [(empty? original)
         (error "match-to-original-hand : Hand not found.")]
        [(hand-subset? (first original) tomatch)
         (first original)]
        [else
         (match-to-original-hand (rest original) tomatch)]))
(module+ test
  (check-equal?
   (match-to-original-hand
    (list HAND1 HAND2)
    (list CARD-CK CARD-D10))
   HAND1
   "Testing match-to-original-hand.")
  (check-equal?
   (match-to-original-hand
    (list HAND-RANK7 HAND-RANK9 HAND-RANK3)
    (list CARD-S3 CARD-S7))
    HAND-RANK9
    "Testing match-to-original-hand."))

;;hand-match? : Hand Hand -> Boolean
;;Do the two hands contain the same cards?
(define (hand-match? hand1 hand2)
  (cond [(not (equal? (length hand1)
                      (length hand2)))
         false]
        [(empty? hand1) true]
        [(card=? (first hand1) (first hand2))
         (hand-match? (rest hand1) (rest hand2))]
        [else false]))
(module+ test
  (check-true
   (hand-match? HAND1 HAND1)
   "Testing hand-match?")
  (check-false
   (hand-match? HAND2 HAND-RANK4)
   "Testing hand-match?"))

;;hand-subset? : Hand [List Card] -> Boolean
;;Are these cards a subset of this hand?
(define (hand-subset? hand loc)
  (andmap (λ (c) (member=? c hand card=?)) loc))
(module+ test
  (check-true
   (hand-subset? HAND1 (list CARD-C9 CARD-HA))
   "Testing hand-subset?")
  (check-false
   (hand-subset? HAND2 (list CARD-CK CARD-C8))
   "Testing hand-subset?"))

;;find-playing : [List EndClient] [List PlayerData] -> [List EndClient]
;;Find the clients who were still playing when the game ended
(define (find-playing players-with-hands player-data)
  (filter (λ (player) (is-playing? player-data (first player))) players-with-hands))
(module+ test
  (check-equal?
   (find-playing (list (list "Alice" HAND-RANK3)
                       (list "Bob" HAND-RANK7)
                       (list "Charlie" HAND-RANK5))
                 '(("Alice" #t) ("Bob" #f) ("Charlie" #t)))
   (list (list "Alice" HAND-RANK3)
         (list "Charlie" HAND-RANK5))
   "Testing find-playing.")
  (check-equal?
   (find-playing (list (list "Alice" HAND-RANK4)
                       (list "Bob" HAND-RANK5)
                       (list "Charlie" HAND-RANK9))
                 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)))
   (list (list "Alice" HAND-RANK4)
         (list "Bob" HAND-RANK5)
         (list "Charlie" HAND-RANK9))
   "Testing find-playing."))

;;[Hand Ranking Functions]
;;rank-hand : Hand -> Nat
;;Produces the ranking of this hand
(define (rank-hand torank)
  (cond [(royalflush? torank) 10]
        [(straightflush? torank) 9]
        [(contains-n-of-a-kind? torank 4) 8]
        [(fullhouse? torank) 7]
        [(all-same-suit? torank) 6]
        [(all-consecutive-cards? torank) 5]
        [(contains-n-of-a-kind? torank 3) 4]
        [(doublepair? torank) 3]
        [(contains-n-of-a-kind? torank 2) 2]
        [else 1]))
(module+ test
  (check-equal?
   (rank-hand HAND-RANK10) 10
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK9) 9
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK8) 8
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK7) 7
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK6) 6
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK5) 5
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK4) 4
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND-RANK3) 3
   "Testing rank-hand.")
  (check-equal?
   (rank-hand HAND2) 2
   "Testing rank-hand.")
  (check-equal?
   (rank-hand
    '((#f 1 7) (#f 2 5) (#f 3 3) (#f 4 2) (#f 3 6)))
   1 "Testing rank-hand."))

;;royalflush? : Hand -> Boolean
;;Is this hand a royal flush?
(define (royalflush? hand)
  (local [(define royals (filter (λ (c) (> (third c) 9)) hand))
          (define collectroyals (collect-suits royals empty))]
    (ormap (λ (sp) (equal? (second sp) 4)) collectroyals)))
(module+ test
  (check-false
   (royalflush? HAND-RANK5)
   "Testing royalflush?")
  (check-true
   (royalflush? HAND-RANK10)
   "Testing royalflush?"))

;;straightflush? : [List Card] -> Boolean
;;Is this hand a straight flush?
(define (straightflush? hand)
  (and (all-consecutive-cards? hand)
       (all-same-suit? hand)))
(module+ test
  (check-false
   (straightflush? HAND-RANK3)
   "Testing straightflush?")
  (check-true
   (straightflush? HAND-RANK9)
   "Testing straightflush?"))

;;contains-n-of-a-kind? : [List Card] Number -> Boolean
;;Determines if this hand contains n cards with the same value
(define (contains-n-of-a-kind? hand n)
  (local [(define allkinds (find-kinds hand))]
    (> (length (filter (λ (kp) (>= (second kp) n)) allkinds)) 0)))
(module+ test
  (check-true
   (contains-n-of-a-kind? HAND-RANK8 4)
   "Testing contains-n-of-a-kind?")
  (check-false
   (contains-n-of-a-kind? HAND-RANK6 2)
   "Testing contains-n-of-a-kind?"))

;;fullhouse? : [List Card] -> Boolean
;;Is this hand a full house? (1 pair and 1 set of three)
(define (fullhouse? hand)
  (local [(define howmanypairs (how-many-sets-of-n? hand 2))
          (define howmanytriples (how-many-sets-of-n? hand 3))]
    (and (> howmanypairs 0) (> howmanytriples 0))))
(module+ test
  (check-false
   (fullhouse? HAND-RANK6)
   "Testing fullhouse?")
  (check-true
   (fullhouse? HAND-RANK7)
   "Testing fullhouse?"))

;;all-same-suit? : [List Card] -> Boolean
;;Are all these cards the same suit?
(define (all-same-suit? hand)
  (if (empty? hand) true
      (local [(define suit1 (second (first hand)))]
        (andmap (λ (c) (equal? (second c) suit1)) hand))))
(module+ test
  (check-false
   (all-same-suit? HAND-RANK8)
   "Testing all-same-suit?")
  (check-true
   (all-same-suit? empty)
   "Testing all-same-suit?")
  (check-true
   (all-same-suit? HAND-RANK6)
   "Testing all-same-suit?"))

;;all-consecutive-cards? : [List Card] -> Boolean
;;Are all the cards in this hand consecutive values?
(define (all-consecutive-cards? hand)
  (local [(define allvalues (map third hand))
          (define sortvalues (sort allvalues <))]
    (all-consecutive? sortvalues)))
(module+ test
  (check-false
   (all-consecutive-cards? HAND-RANK3)
   "Testing all-consecutive-cards?")
  (check-true
   (all-consecutive-cards? HAND-RANK5)
   "Testing all-consecutive-cards?"))

;;doublepair? : [List Card] -> Boolean
;;Does this hand contain two distinct pairs?
(define (doublepair? hand)
  (local [(define allkinds (find-kinds hand))]
    (>= (length (filter (λ (kp) (>= (second kp) 2)) allkinds)) 2)))
(module+ test
  (check-false
   (doublepair? HAND-RANK10)
   "Testing doublepair?")
  (check-true
   (doublepair? HAND-RANK3)
   "Testing doublepair?"))

;;collect-suits : [List Card] [List SuitPair] -> [List SuitPair]
;;Identifies how many cards of each suit are in the list of cards
(define (collect-suits loc losp)
  (if (empty? loc) losp
      (local [(define suit1 (second (first loc)))]
        (cond [(member=? suit1
                         losp
                         (λ (s sp) (equal? s (first sp))))
               (collect-suits (rest loc)
                              (add1-to-suitpair suit1 losp))]
              [else
               (collect-suits (rest loc)
                              (cons (list suit1 1) losp))]))))
(module+ test
  (check-equal?
   (collect-suits HAND1 empty)
   (list (list 1 1)
         (list 4 3)
         (list 3 1))
   "Testing collect-suits.")
  (check-equal?
   (collect-suits HAND-RANK10 empty)
   (list (list 4 4)
         (list 1 1))
   "Testing collect-suits."))

;;find-kinds : [List Card] -> [List KindPair]
;;Finds all the values and how many times they appear in this hand
(define (find-kinds hand)
  (local [(define (find-kinds-acc tofind found)
            (if (empty? tofind) found
                (let ((cv1 (third (first tofind))))
                  (cond [(member=? cv1 found
                                   (λ (n kp) (equal? n (first kp))))
                         (find-kinds-acc (rest tofind)
                                         (add1-to-kind cv1 found))]
                        [else
                         (find-kinds-acc (rest tofind)
                                         (cons (list cv1 1) found))]))))]
    (find-kinds-acc hand empty)))
(module+ test
  (check-equal?
   (find-kinds HAND1)
   (list (list 1 2)
         (list 9 1)
         (list 13 1)
         (list 10 1))
   "Testing find-kinds.")
  (check-equal?
   (find-kinds HAND-RANK7)
   (list (list 6 2)
         (list 3 3))
   "Testing find-kinds."))

;;how-many-sets-of-n? : Hand Number -> Number
;;How many sets of n cards are there in this hand?
(define (how-many-sets-of-n? hand n)
  (local [(define allkinds (find-kinds hand))
          (define allkindsn
            (filter (λ (kp) (equal? (second kp) n)) allkinds))]
    (length allkindsn)))
(module+ test
  (check-equal?
   (how-many-sets-of-n? HAND-RANK10 2)
   0 "Testing how-many-sets-of-n?")
  (check-equal?
   (how-many-sets-of-n? HAND-RANK7 3)
   1 "Testing how-many-sets-of-n?"))

;;add1-to-suitpair : CSuit [List SuitPair] -> [List SuitPair]
;;Finds the suitpair with this suit and increments its counter
(define (add1-to-suitpair suit losp)
  (map (λ (sp) (if (equal? suit (first sp))
                   (list suit (add1 (second sp))) sp))
       losp))
(module+ test
  (check-equal?
   (add1-to-suitpair 2 (list (list 4 1) (list 2 3)))
   (list (list 4 1) (list 2 4))
   "Testing add1-to-suitpair.")
  (check-equal?
   (add1-to-suitpair 2 empty)
   empty "Testing add1-to-suitpair."))

;;add1-to-kind : CVal [List KindPair] -> [List KindPair]
;;Find the KindPair with the given value and adds 1 to its counter
(define (add1-to-kind cv lokp)
  (map (λ (kp) (if (equal? cv (first kp))
                   (list cv (add1 (second kp))) kp))
       lokp))
(module+ test
  (check-equal?
   (add1-to-kind 10 empty) empty
   "Testing add1-to-kind.")
  (check-equal?
   (add1-to-kind
    10 (list (list 6 1)
             (list 13 1)
             (list 10 2)))
   (list (list 6 1)
         (list 13 1)
         (list 10 3))
   "Testing add1-to-kind."))

;;player-bet : WorldWait String Number -> WorldWait
;;Update the current bid, the total pool, and the actions
(define (player-bet world name bet)
  (if (string=? name (wwait-name world)) world
      (make-wwait (wwait-name world)
              (wwait-hand world)
              (wwait-players world)
              (cons (format "~a bet $~a~a." name bet
                            (if (> bet (wwait-bid world)) "" " (called)"))
                    (wwait-actions world))
              bet
              (+ (wwait-pool world) bet)
              (wwait-mypool world)
              (if (> bet (wwait-bid world)) 1
                  (add1 (wwait-agreed world))))))
(module+ test
  (check-equal?
   (player-bet WORLD-WAIT0 "Alice") WORLD-WAIT0
   "Testing player-bet.")
  (check-equal?
   (player-bet WORLD-WAIT0 "Bob" 5)
   (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)) '("Bob bet $5.") 5 8 1 1)
   "Testing player-bet.")
  (check-equal?
   (player-bet WORLD-WAIT1 "Charlie" 2)
   (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) '("Charlie bet $2 (called)." "Some actions.") 2 7 3 2)
   "Testing player-bet."))

;;player-fold : WorldWait String -> WorldWait
;;Update the player list and actions to reflect this player folding
(define (player-fold world name)
  (if (string=? name (wwait-name world)) world
      (make-wwait (wwait-name world)
              (wwait-hand world)
              (map (λ (player) (if (string=? name (first player))
                                   (list name #f) player))
                   (wwait-players world))
              (cons (format "~a folded." name)
                    (wwait-actions world))
              (wwait-bid world)
              (wwait-pool world)
              (wwait-mypool world)
              (wwait-agreed world))))
(module+ test
  (check-equal?
   (player-fold WORLD-WAIT0 "Alice")
   WORLD-WAIT0 "Testing player-fold.")
  (check-equal?
   (player-fold WORLD-WAIT0 "Bob")
   (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) '("Bob folded.") 1 3 1 0)
   "Testing player-fold.")
  (check-equal?
   (player-fold WORLD-WAIT1 "Charlie")
   (make-wwait "Alice" HAND1 '(("Alice" #t) ("Bob" #f) ("Charlie" #f))
               '("Charlie folded." "Some actions.") 2 5 3 1)
   "Testing player-fold."))

;;player-call/pass : WorldWait String Number -> WorldWait
;;If the player has folded, this is a pass
;;If the player has not folded, they have called
(define (player-call/pass world name bid)
  (local [(define this-player-data
            (match-elem (wwait-players world)
                        (λ (p) (string=? (first p) name))))]
    (cond [(second this-player-data)
           (player-bet world name bid)]
          [else world])))
(module+ test
  (check-equal?
   (player-call/pass WORLD-WAIT0 "Charlie" 1)
   (player-bet WORLD-WAIT0 "Charlie" 1)
   "Testing player-call/pass.")
  (check-equal?
   (player-call/pass WORLD-WAIT1 "Bob" 2)
   WORLD-WAIT1
   "Testing player-call/pass."))

;;[Misc]
;;is-playing? : [List PlayerData] String -> Boolean
;;Is this player playing?
(define (is-playing? players name)
  (andmap
   second
   (filter (λ (player) (string=? (first player) name))
           players)))
(module+ test
  (check-true
   (is-playing? '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) "Charlie")
   "Testing is-playing?")
  (check-false
   (is-playing? '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) "Bob")
   "Testing is-playing?"))

;;update-players-fold : [List PlayerData] String -> [List PlayerData]
;;Update the given player to indicate that they have folded
(define (update-players-fold players name)
  (map (λ (player) (if (string=? name (first player))
                       (list name #f) player)) players))
(module+ test
  (check-equal?
   (update-players-fold
    '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) "Dave")
   '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
   "Testing update-players-fold.")
  (check-equal?
   (update-players-fold
    '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) "Alice")
   '(("Alice" #f) ("Bob" #f) ("Charlie" #t))
   "Testing update-players-fold.")
  (check-equal?
   (update-players-fold
    '(("Alice" #t) ("Bob" #f) ("Charlie" #t)) "Bob")
   '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
   "Testing update-players-fold."))

;;update-world-with-hand : World Hand -> World
;;Given a new hand, update
(define (update-world-with-hand world newhand)
  (cond [(wdeal? world)
         (make-wdeal
          (wdeal-name world) newhand
          (wdeal-players world))]
        [(wwait? world)
         (make-wwait
          (wwait-name world) newhand
          (wwait-players world)
          (wwait-actions world)
          (wwait-bid world)
          (wwait-pool world)
          (wwait-mypool world)
          (wwait-agreed world))]
        [(wmove? world)
         (make-wmove
          (wmove-name world) newhand
          (wmove-players world)
          (wmove-actions world)
          (wmove-bid world)
          (wmove-pool world)
          (wmove-mypool world)
          (wmove-agreed world))]
        [(wbet? world)
         (make-wbet
          (wbet-name world) newhand
          (wbet-players world)
          (wbet-actions world)
          (wbet-bid world)
          (wbet-pool world)
          (wbet-mypool world)
          (wbet-agreed world)
          (wbet-sofar world))]
        [(wupdate? world)
         (make-wupdate
          (update-world-with-hand
           (wupdate-worldstate world) newhand)
          (wupdate-name world))]
        [else (error "Invalid world state.")]))
(module+ test
  (check-equal?
   (update-world-with-hand WORLD-DEAL1 HAND1)
   (make-wdeal "Alice" HAND1 '(("Alice" #t) ("Bob" #t) ("Charlie" #t)))
   "Testing update-world-with-hand.")
  (check-equal?
   (update-world-with-hand WORLD-WAIT1 HAND-RANK7)
   (make-wwait "Alice" HAND-RANK7 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
               '("Some actions.") 2 5 3 1)
   "Testing update-world-with-hand.")
  (check-equal?
   (update-world-with-hand WORLD-MOVE0 HAND-RANK4)
   (make-wmove "Alice" HAND-RANK4 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
               '("Some actions.") 2 5 3 1)
   "Testing update-world-with-hand.")
  (check-equal?
   (update-world-with-hand WORLD-BET0 HAND-RANK8)
   (make-wbet "Alice" HAND-RANK8 '(("Alice" #t) ("Bob" #f) ("Charlie" #t))
              '("Some actions.") 2 5 3 1 5)
   "Testing update-world-with-hand."))

;;update-endclient-data : [List EndClient] String Card -> [List EndClient]
;;If the player is in the list, add this card to his/her hand
(define (update-endclient-data alldata name card)
  (map (λ (endclient)
         (if (string=? (first endclient) name)
             (list name (cons card (second endclient)))
             endclient)) alldata))
(module+ test
  (check-equal?
   (update-endclient-data
    '(("Alice" ()) ("Bob" ())) "Alice" CARD-DQ)
   `(("Alice" (,CARD-DQ)) ("Bob" ()))
   "Testing update-endclient-data.")
  (check-equal?
   (update-endclient-data
    '(("Alice" ()) ("Bob" ())) "Charlie" CARD-DQ)
   '(("Alice" ()) ("Bob" ()))
   "Testing update-endclient-data."))

#|--- [END WORLD HELPER FUNCTIONS] ---|#

(run)