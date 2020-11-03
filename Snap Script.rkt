#lang racket/gui
(require games/cards racket/gui racket/class )
(define snapmenu (new frame%
[label "Snap Menu"]
[width 400] [height 400])) ;;Starting window with 'Start' button is created here.

(define memory (make-table "Snap" (+ 7) (+ 5))) ;;make-table is from games library,so width is
                                                ;;how many card/s wide the window will be.

(define gamestart (new horizontal-panel% 
                       [parent snapmenu]))  ;;This is the 'start game' option.
(new button%	[min-width 100 ]	 
   	 	[min-height 100]   ;;The size of the button                                   
                [horiz-margin 200] ;;centred the button
                [parent gamestart] ;;It is inside the gamestart wincow.
                [callback 
                 (λ (a b)
                 (send memory show #t)
                (send snapmenu show #f))] ;;Calls off the snapmenu and shows the game window.
     [label "Start Game\n"])

(define deck ;;deck of cards
  (let ([cards (map (lambda (name value)
                      (let ([bm (make-object
                                    bitmap%
                                  (build-path
                                   (collection-path "games" "memory" "images")
                                   (format "~a.png" name)))])
                        ;;the bitmaps are collected from games->memory->images
                        
                        (make-card bm #f 0 value))) ;;gives suit+id+bitmap name values of each of the cards
                    '("ankh" "atom" "dragon" "freemason"
                             "horus" "illuminati"
                             "psych" "skull"
                             "templar" "thelema")
                    '(1 2 3 4 5 6 7 8 9 10))])
    (append cards (map (lambda (c) (send c copy)) cards))))

(map (lambda (card)
            (send card user-can-move #f)
            (send card user-can-flip #t))
          deck)

(define cardwidth (send (car deck) card-width)) ;;cardwidth is card-width
(define cardheight (send (car deck) card-height)) ;;cardheight is card's height

(define dx (/ cardwidth 7)) ;;cardwidth/7=dx
(define dy (/ cardheight 5)) ;;cardheight/7=dy
 
(define tablew (send memory table-width)) ;;Returns the width of the table (in pixels) to tablew.
(define match-x (- tablew cardwidth dx))
(define match-y dy)



(send memory add-cards deck match-x match-y) ;;add-cards then deck (meaning all cards)

(define (snapSetup)   ;;This is the setup of the cards on the table and they will be shuffled
  (set! deck (shuffle-list deck 7))
  (send memory stack-cards deck) ;;cards are stacked and moved orderly
  (send memory move-cards deck 0 0
        (lambda (pos)
          (let ([x (remainder pos 5)] ;; is the remainder of a divison
                [y (quotient pos 5)]) ;;(result of a division) NOT remainder
            (values (+ dx (* x (+ cardwidth dx)))  ;;columns
                    (+ dy (* y (+ cardheight dy)))) ;;rows
            ))
        )
  )
(define matches 0)
(define card2 #f) ;;not flipped/clicked.
(define (matcher card1)
  (cond [(eq? card1 card2)
              (send memory flip-card card1) ;;flips card
              (set! card2 #f)] ;;card2 is true now (and flipped)
            [(not (send card1 face-down?)) ;; Can't click a matched card.             
         (when (= matches 10)
           (send memory flip-cards deck)
           (set! matches 0)
           (snapSetup))]
    ;;game is reset with the snapSetup function shuffling the deck and facing cards down.
        [else
         (send memory flip-card card1) ;;flips over clicked card
         (send memory card-to-front card1)
         (cond [(not card2) (set! card2 card1)] ;;set 
               [(and (equal? (send card2 get-value) (send card1 get-value))
                     (equal? (send card2 get-suit) (send card1 get-suit)))
                (send memory pause 0.5) ;;pauses for 0.5 seconds, no clicking allowed on the table window.
                (send memory move-cards (list card2 card1) match-x match-y)
                (set! card2 #f)
                (set! matches (+ matches 1))] ;;matches are plus one here
               [else ;;When cards don't match
                (send memory pause 0.5)
                (send memory flip-cards (list card2 card1))
                (set! card2 #f)]
               )
         ])
  )
        
                   

 (send memory set-single-click-action matcher)
(snapSetup)
(send snapmenu show #t)

     
