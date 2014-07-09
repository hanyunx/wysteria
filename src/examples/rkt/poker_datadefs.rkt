#lang racket
(provide (all-defined-out))

#|--- [BEGIN SERVER DEFINITIONS] ---
A Server is one of:
- ServerBegin
- ServerDeal
- ServerPlay
|#
;;A ServerBegin is a (make-sbegin [Maybe IWorld] [Maybe Nat] [List String]
;                                 [Maybe String] [Maybe String])
(define-struct sbegin (iw numplayers allplayers myname showbtn?) #:transparent)
; - The IWorld is the world the server is connected to
; (false if no world has connected yet)
; - The Nat is the number of expected players
; (false if unknown)
; - The [List String] is the list of players
; - The String is the player's name
; (false if unknown)
; - The String tells whether or not to show the button

;;A ServerDeal is a (make-sdeal IWorld Hand)
(define-struct sdeal (iw hand) #:transparent)
; - The IWorld is the world the server is connected to
; - The Hand is the player's hand so far

;;A ServerPlay is a (make-splay IWorld Hand Boolean)
(define-struct splay (iw hand check?) #:transparent)
; - The IWorld is the world the server is connected to
; - The Hand is the player's hand
; - The Boolean is true if the server should check for a message from Wysteria

;;A ServerCheck is an IWorld
; - at this point the game is over so we don't need any other information

#|
A ServerMsg is one of:
- [List String] (a list of all players in the game)
- Hand
- 'go
- 'check
- String (a player's name)
- Number (represents either an action or
a card depending on the phase of the game)

A BeginMsg is one of:
- Nat
- 

An EndClient is a (list String Hand)
--- [END SERVER DEFINITIONS] ---|#

#|--- [BEGIN WORLD DEFINITIONS] ---
A World is one of:
- WorldBegin
- WorldDealButton
- WorldDeal
- WorldWait
- WorldMove
- WorldUpdate
- WorldBet
- WorldCheck
- String
|#

;;A WorldBegin is #f

;;A WorldDealButton is a (make-wdb String [List PlayerData])
(define-struct wdb (name players))
; - The String is the player's name
; - The [List PlayerData] is a list of players (name + whether they've folded)

;;A WorldDeal is a (make-wdeal String Hand [List PlayerData])
(define-struct wdeal (name hand players) #:transparent)
; - The String is the player's name
; - The Hand is the player's hand
; - The [List PlayerData] is a list of players (name + whether they've folded)

;;A WorldWait is a (make-wwait String Hand [List PlayerData]
;                             [List String] Nat Nat Nat Nat)
(define-struct wwait (name hand players actions bid pool mypool agreed)
  #:transparent)
#|- The String is the player's name
 - The Hand is the player's hand
 - The [List PlayerData] is a list of players (name + whether they've folded)
 - The [List String] is a list of actions to display
 - The first Nat is the current bid
 - The second Nat is the total betting pool
 - The third Nat is this player's contribution to the betting pool
 - The fourth Nat is the # of players who have agreed on a bet
|#

;;A WorldMove is a (make-wmove String Hand [List PlayerData]
;                             [List String] Nat Nat Nat Nat)
(define-struct wmove (name hand players actions bid pool mypool agreed)
  #:transparent)
#|- The String is the player's name
 - The Hand is the player's hand
 - The [List PlayerData] is a list of players (name + whether they've folded)
 - The [List String] is a list of actions to display
 - The first Nat is the current bid
 - The second Nat is the total betting pool
 - The third Nat is this player's contribution to the betting pool
 - The fourth Nat is the # of players who have agreed on a bet
|#

;;A WorldUpdate is a (make-wupdate World String)
(define-struct wupdate (worldstate name))
; - The World is the current state of the world
; - The String is the name of the player who's information is being updated

;;A WorldBet is a (make-wbet String Hand [List PlayerData]
;                           [List String] Nat Nat Nat Nat Number)
(define-struct wbet (name hand players actions bid pool mypool agreed sofar)
  #:transparent)
#|- The String is the player's name
 - The Hand is the player's hand
 - The [List PlayerData] is a list of players (name + whether they've folded)
 - The [List String] is a list of actions to display
 - The first Nat is the current bid
 - The second Nat is the total betting pool
 - The third Nat is this player's contribution to the betting pool
 - The fourth Nat is the # of players who have agreed on a bet
 - The Number is the player's bet so far
|#

;;A WorldCheck is a (make-wcheck String Nat Nat [List EndClient])
(define-struct wcheck (name pool mypool playerdata))
#|- The String is the player's name
 - The first Nat is the total betting pool
 - The second Nat is this player's contribution tot he betting pool
 - The [List EndClient] is a list of all data about the players

A PlayerData is a (list String Boolean)
- The String is the player's name
- The Boolean is true if the player is still playing and false if they've folded

An EndClient is a (list String Hand)
- The String is the player's name
- The Hand is the player's hand

A WorldMsg is one of:
- 'fold
- 'pass
- Nat

--- [END WORLD DEFINITIONS] ---|#

#|--- [BEGIN CARD/HAND DEFINITIONS] ---|#
;;A Card is a (list Boolean CSuit CVal)

;;A CSuit is a Nat in the range [1,4]
(define ALL-SUITS (build-list 4 add1))
;;A CVal is a Nat in the range [1,13]
(define ALL-VALS (build-list 13 add1))

;;A Hand is a [List Card]

;;A Deck is a [List Card]
(define FULL-DECK
  (foldr
   (λ (suit sofar)
     (append (map (λ (val) (list #f suit val)) ALL-VALS)
             sofar)) '() ALL-SUITS))
#|--- [END CARD/HAND DEFINITIONS] ---|#

#|--- [BEGIN MISC DEFINITIONS] ---|#
;;A Posn is a (make-posn Nat Nat)
(define-struct posn (x y) #:transparent)
; - The first Nat is the x-position
; - The second Nat is the y-position

;;A ButtonMode is one of: 'fold, 'bet, 'call, 'done
#|--- [END MISC DEFINITIONS] ---|#