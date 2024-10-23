;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Henry Bex hw5 updated|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))


;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (= (first lon) 0)
         '()
         (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

(define L2 (cons 4 (cons 44 (cons 2 (cons 0 (cons 12 '()))))))
(define L3 (cons "Hello" (cons "there" (cons "professor" (cons "." '())))))

;; Abstraction : {X} (X -> X) [ListOf X] -> [ListOf X]
;; Purpose: constructs list until true statement

(define (abstraction op l)
  (cond [(or (empty? l) (op (first l))) '()]
        [(cons? l) (cons (first l) (abstraction op (rest l)))]))

(check-expect (abstraction .? L3) (cons "Hello" (cons "there" (cons "professor" '()))))

;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or words-until-period/v2.

;; 0?: Number -> Boolean
;; 0?: Determines if a number is = 0
(define (0? l)
  (= 0 l))

;; until-zero/v2: [ListOf Num] -> [ListOf Num]
;; until-zero/v2: returns a list of numbers before 0
(define (until-zero/v2 l)
  (abstraction 0? l))

(check-expect (until-zero/v2 L2)
              (cons 4 (cons 44 (cons 2 '()))))

;; .?: String -> Boolean
;; .?: Determines if a string is .
(define (.? l)
  (string=? "." l))

;; words-until-period/v2: [ListOf String] -> [ListOf String]
;; words-until-period/v2: returns list of strings before .
(define (words-until-period/v2 l)
  (abstraction .? l))

(check-expect (words-until-period/v2 L3)
              (cons "Hello" (cons "there" (cons "professor" '()))))

;;! Problem 2

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.

(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of any type
;; - first is the first item in the pair
;; - second is the second item in the pair


;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())

;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair.
(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (alternate-case (list (make-pair "one" "two") (make-pair "three" "four") (make-pair "five" "six")))
              (list (make-pair "ONE" "two") (make-pair "THREE" "four") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana"))) (list (make-pair "APPLE" "banana")))

;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second.
(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))

;; However, you must not _directly_ use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.

;; abs2 : {X} (X -> X) {X} (X -> X) [ListOf X] -> [ListOf X]
;; abs2 : returns a modified list of pairs
(define (abs2 op1 op2 l)
  (cond [(empty? l) '()]
        [(cons? l) (cons (make-pair
                           (op1 (pair-first (first l)))
                           (op2 (pair-second (first l))))
                     (abs2 op1 op2 (rest l))
                     )]))

;; soro-1: Number -> String
;; soro-1: helper function - converts first number in pair to string
(define (soro-1 p)
 (number->string p))

(check-expect (soro-1 1) "1")

;; soro-2: Any -> "odd"
;; soro-2: helper function - converts second data in pair to "odd"
(define (soro-2 p)
  "odd")

(check-expect (soro-2 L2) "odd")

(define (strings-or-odds l)
  (abs2 soro-1 soro-2 l))

;; alt-1: String -> String
;; alt-1: helper function - uppercases the first string in a pair
(define (alt-1 p)
  (string-upcase p))

(check-expect (alt-1 "hi") "HI")

;; alt-2: Any -> Any
;; alt-2: returns an input
(define (alt-2 p)
  p)

(check-expect (alt-2 "x") "x")

(define (alternate-case l)
  (abs2 alt-1 alt-2 l))

;; fk-1: Boolean -> Boolean
;; fk-1: flips the first boolean in a pair
(define (fk-1 b)
  (not b))

(check-expect (fk-1 #t) #f)

(define (flip-or-keep-boolean l)
  (abs2 fk-1 alt-2 l))

;;! Problem 3

;; Objective: Build a Word Game

;; Your goal is to author a word-building game. You will start with an empty 5x1 grid
;; and a hidden list of random letters. When the player clicks on a cell, its
;; contents should be replaced by the next letter in the list. The game concludes
;; when the cells spell a five-letter word. (You should build a short list of
;; five letter words.)
;;
;; Here is a video that demonstrates the game:
;;
;; https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw5.gif
;;
;; Here are questions to help you think through your program design:
;;
;; 1. What do you need in your world state? (What changes during the game?)
;;    Come up with a data design to represent the world state.
;;
;; 2. Your program needs to draw a board, handle mouse clicks, and stop when
;;    the player constructs a word or runs out of letters. These are three 
;;    functions that you need to design.
;;
;; 3. Finally, put it all together using big-bang.

;; Constants 
(define BACKGROUND ;; 5 empty squares background
    (beside (square 50 "outline" "black")
        (beside (square 50 "outline" "black")
                (beside (square 50 "outline" "black")
                        (beside (square 50 "outline" "black")
                                (square 50 "outline" "black")
                                        )))))

(define BOX-SIZE 50) 

(define LETTER-LIST (cons "z" (cons "y" (cons "x" (cons "w" (cons "v" (cons "u"
                    (cons "t" (cons "s" (cons "r" (cons "q" (cons "p" (cons "o"
                    (cons "n" (cons "m" (cons "l" (cons "k" (cons "j" (cons "i"
                    (cons "h" (cons "g" (cons "f" (cons "e" (cons "d" (cons "c"
                    (cons "b" (cons "a" '()))))))))))))))))))))))))))) ;; hidden letter list 

(define WORD-LIST (cons "place" (cons "flour" (cons "hours"'())))) ;;five letter word list 


(define-struct WorldState [letters l1 l2 l3 l4 l5])
;; A WorldState is a
;; - (make-WorldState list String String String String String)


(define WS-1 (make-WorldState LETTER-LIST "" "" "" "" ""))
(define WS-2 (make-WorldState LETTER-LIST "f" "l" "o" "u" "r"))

                                        

;; word-game: WorldState -> Image
;; word-game: runs a word game 
(define (word-game initial)
	(big-bang initial
	   [to-draw draw-letter] 
	   [on-mouse mouse-letter] 
           [stop-when word-made?]))


;; mouse-letter : WorldState Real Real MouseEvent -> WorldState
;; when the mouse is clicked it replaces the string in the place
;; of the box that was lcikced with the next letter in the list
(check-expect (mouse-letter WS-1 0 0 "button-up") (make-WorldState (rest LETTER-LIST) "z" "" "" "" ""))
(check-expect (mouse-letter WS-2 51 0 "button-up") (make-WorldState (rest LETTER-LIST) "f" "z" "o" "u" "r"))
(check-expect (mouse-letter WS-2 101 0 "button-up") (make-WorldState (rest LETTER-LIST) "f" "l" "z" "u" "r"))
(check-expect (mouse-letter WS-2 151 0 "button-up") (make-WorldState (rest LETTER-LIST) "f" "l" "o" "z" "r"))
(check-expect (mouse-letter WS-2 201 0 "button-up") (make-WorldState (rest LETTER-LIST) "f" "l" "o" "u" "z"))
(check-expect (mouse-letter WS-1 -1 0 "button-down")(make-WorldState LETTER-LIST "" "" "" "" ""))

(define (mouse-letter ws x y me)
  (cond [(mouse=? me "button-up")
        (cond [(empty? (WorldState-letters ws)) ws]
              [(> x 200) (make-WorldState (rest(WorldState-letters ws))
                                          (WorldState-l1 ws)
                                          (WorldState-l2 ws)
                                          (WorldState-l3 ws)
                                          (WorldState-l4 ws)
                                          (first (WorldState-letters ws)))]
              [(> x 150) (make-WorldState (rest(WorldState-letters ws))
                                          (WorldState-l1 ws)
                                          (WorldState-l2 ws)
                                          (WorldState-l3 ws)
                                          (first (WorldState-letters ws))
                                          (WorldState-l5 ws))]
              [(> x 100)(make-WorldState (rest(WorldState-letters ws))
                                          (WorldState-l1 ws)
                                          (WorldState-l2 ws)
                                          (first (WorldState-letters ws))
                                          (WorldState-l4 ws)
                                          (WorldState-l5 ws))] 
              [(> x 50) (make-WorldState (rest(WorldState-letters ws))
                                          (WorldState-l1 ws)
                                          (first (WorldState-letters ws))
                                          (WorldState-l3 ws)
                                          (WorldState-l4 ws)
                                          (WorldState-l5 ws))]
              [(>= x 0)(make-WorldState (rest(WorldState-letters ws))
                                          (first (WorldState-letters ws))
                                          (WorldState-l2 ws)
                                          (WorldState-l3 ws)
                                          (WorldState-l4 ws)
                                          (WorldState-l5 ws))])]
        [else ws]))

;; draw-letter : WorldState -> Image
;; draws each of the letters in their respective box
(check-expect (draw-letter WS-1) (overlay/offset (text "" (/ BOX-SIZE 2) "black") (* -2 BOX-SIZE) 0
                                                 (overlay/offset (text "" (/ BOX-SIZE 2) "black") (* -1 BOX-SIZE) 0
                                                                 (overlay/offset (text "" (/ BOX-SIZE 2) "black") 0 0
                                                                                 (overlay/offset (text "" (/ BOX-SIZE 2) "black") (* 1 BOX-SIZE) 0
                                                                                                 (overlay/offset (text "" (/ BOX-SIZE 2) "black") (* 2 BOX-SIZE) 0 BACKGROUND))))))
(check-expect (draw-letter WS-2) (overlay/offset (text "r" (/ BOX-SIZE 2) "black") (* -2 BOX-SIZE) 0
                                                 (overlay/offset (text "u" (/ BOX-SIZE 2) "black") (* -1 BOX-SIZE) 0
                                                                 (overlay/offset (text "o" (/ BOX-SIZE 2) "black") 0 0
                                                                                 (overlay/offset (text "l" (/ BOX-SIZE 2) "black") (* 1 BOX-SIZE) 0
                                                                                                 (overlay/offset (text "f" (/ BOX-SIZE 2) "black") (* 2 BOX-SIZE) 0 BACKGROUND))))))
(define (draw-letter ws)
 (overlay/offset (text (WorldState-l5 ws) (/ BOX-SIZE 2) "black") (* -2 BOX-SIZE) 0
                  (overlay/offset (text (WorldState-l4 ws) (/ BOX-SIZE 2) "black") (* -1 BOX-SIZE) 0
                                  (overlay/offset (text (WorldState-l3 ws) (/ BOX-SIZE 2) "black") 0 0
                                                  (overlay/offset (text (WorldState-l2 ws) (/ BOX-SIZE 2) "black") (* 1 BOX-SIZE) 0
                                                                  (overlay/offset (text (WorldState-l1 ws) (/ BOX-SIZE 2) "black") (* 2 BOX-SIZE) 0 BACKGROUND))))))

;; word-made? : WorldState  -> Boolean
;; alerts the BigBang to stop if a word is made
(check-expect (word-made? WS-1) #false)
(check-expect (word-made? WS-2) #true)
(define (word-made? ws)
  (cond [(check-word ws WORD-LIST) #true]
        (else #false)))

;; word-made? : WorldState List -> Boolean
;; helper function to compare the world state to the words in the word list
(define (check-word ws wl)
  (cond [(empty? wl) #false]
        [(string=? (WorldState->string ws) (first wl)) #true]
        [else  (check-word ws (rest wl))]))
                    
;; WorldState->string : WorldState -> String
;; computes the five strings into one string
(check-expect (WorldState->string WS-1) "")
(define (WorldState->string ws)
  (string-append (WorldState-l1 ws)
                 (WorldState-l2 ws)
                 (WorldState-l3 ws)
                 (WorldState-l4 ws)
                 (WorldState-l5 ws)))
                 
          
               


  
 

                                          



