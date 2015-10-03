;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Problem 1:

;;Natural is natural number which is greater or equal than 0.
;;Natural -> Boolean
;;Given a natural number check wehther it is a prime number
(define (prime? p)
  (local [ ;;Natural Natural-> [Listof Natural]
          ;;List all the factors of n (which is greater or equal than 2)    
          (define (factor n d)
            (cond [(= d 1) empty]
            [else (if (= 0 (remainder n d))
                      (cons d (factor n (- d 1)))
                      (factor n (- d 1)))]))]
(cond [(< p 2) false]
      [else (not (ormap (lambda (x) (<= x (sqrt p))) (factor p (- p 1))))])))
          
          
(check-expect (prime? 15) false)
(check-expect (prime? 7) true)
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)

;;Natural -> [Listof Natural]
;;Given a Natural Number n,
;;produces the list of prime numbers up to n.
(define (list-primes n)
     (filter prime? (build-list  n (lambda (y) (+ y 1)))))

(check-expect (list-primes 1) empty)
(check-expect (list-primes 50) (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
(check-expect (list-primes 2) (list 2))

;;Problem 2:

;;String ->String
;;Given a non-empty String and constructs a palindrome by mirroring the String around the last letter.
(define (make-palindrome s)
  (local [;;String -> String
          ;;Given a word, produce a new word but in opposite letter direction.
          (define (helper s)
            (cond [(<= (string-length s) 1) ""]
                  [else (string-append (substring s (- (string-length s) 2) (- (string-length s) 1))
                           (helper  (substring s 0 (- (string-length s) 1))))]))] 
      (string-append s (helper s))))
(check-expect (make-palindrome "123") "12321")
(check-expect (make-palindrome "How are you") "How are youoy era woH")
(check-expect (make-palindrome "Terry") "TerryrreT")


;;String -> Boolean
;;Determine wether this string is a palindrome.
(define (is-palindrome? s)
  (cond [(odd? (string-length s))
         (string=? 
          (implode (reverse (explode (substring s 0 (quotient (string-length s) 2)))))
          (substring s (+ (quotient (string-length s) 2) 1) (string-length s)))]
        [(even? (string-length s))
         (string=? 
          (implode (reverse (explode (substring s 0  (quotient (string-length s) 2)))))
          (substring s  (quotient (string-length s) 2) (string-length s)))]))
(check-expect (is-palindrome? "abcba") true)
(check-expect (is-palindrome? "1234554321") true)
(check-expect (is-palindrome? "affairsjsks") false)


;;Problem 3:



;;Number -> Number 
;;Given a Natural Number and produces its Fibonacci number. 
(define (fibonacci n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2)))]))
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 6) 8)

;;Natural -> Natural
;;Given a Natural Number and produces its Fibonacci number.
(define (effective-fibonacci n)
  (local [;;Number Number Natural -> Natural
          ;;Given the first Fibonacci number, a accumulator and the natural number which
          ;;return the Fibonacci number of that natural number      
          (define (acc-fibonacci x acc n)
            (cond [(= n 1) acc]
                  [else (acc-fibonacci acc (+ acc x) (- n 1))]))]
    (cond [(= n 0) 0]
          [else (acc-fibonacci 0 1 n)])))
            
                  
(check-expect (effective-fibonacci 3) 2)
(check-expect (effective-fibonacci 11) 89)
(check-expect (effective-fibonacci 0) 0)
(check-expect (effective-fibonacci 1) 1)
(check-expect (effective-fibonacci 6) 8)

;;Natural -> [Listof Natural]
;;consumes a Natural Number and produces 
;;the list of Fibonacci numbers from F0 to Fn.


(define (list-fibonacci n)
  (build-list (add1 n) effective-fibonacci))
  
(check-expect (list-fibonacci 11) (list 0 1 1 2 3 5 8 13 21 34 55 89))
(check-expect (list-fibonacci 1) (list 0 1))
(check-expect (list-fibonacci 0) (list 0))

;; Problem 4

(define-struct card (value suit))
; a card is a (make-card Number Suit)
; value indicates the value of the card (aces are low) 2 -> 11 -> jack etc. 

;; a Suit is a symbol which is one of:
; 'hearts
; 'spades
; 'diamonds
; 'clubs


(define-struct play (player card))
;; a Play is a (make-play String Card).
;;A (make-play p c) which p is the name of the player, c is the card of player 
;;hold.


(define-struct wins (name count))
;; a Wins is a (make-wins String Number)
;; A (make-wins n c) which n represent the name of the winner,
;;c represents the number of the player wining the game.
     
(define LOWCARD (make-card 0 'clubs))
(define LOWPLAY (make-play "Error! You shouldn't be seeing this!" LOWCARD))

;;card example:
(define clubs2 (make-card 2 'clubs))
(define clubs9 (make-card 9 'clubs))
(define diamonds5 (make-card 5 'diamonds))
(define diamonds2 (make-card 2 'diamonds))
(define heart5 (make-card 5 'heart))
(define heart7 (make-card 7 'heart))
(define spades6 (make-card 6 'spades))
(define spades2 (make-card 2 'spades))
(define clubs11 (make-card 11 'clubs))
(define diamonds12 (make-card 12 'diamonds))
(define spades7 (make-card 7 'spades))


;;play example:
(define pe1 (make-play "claire" clubs2))
(define pe2 (make-play "terry" clubs9))
(define pe3 (make-play "jason" diamonds5))
(define pe4 (make-play "cici" diamonds2))
(define pe5 (make-play "ll" heart5))
(define pe6 (make-play "dan" heart7))
(define pe7 (make-play "aci" spades7))


;;listof play example (trick example)
(define trick1 (list pe1 pe2 pe3 pe4))
(define trick2 (list pe5 pe6 pe3 pe4))
(define trick3 (list pe1 pe6 pe3 pe4))
(define trick4 (list pe3 pe5 pe6 pe7))
(define trick5 (list pe1 pe4 pe6 pe7)) 
;; bettersuit?: Card Card -> Boolean
;; Checks if the first card has a better suit than the second.
;; we (assuming that they are different and have same values.
(define (bettersuit? card1 card2) 
       (cond [(or (symbol=? 'spades (card-suit card1)) 
           (symbol=? 'clubs (card-suit card2))) true]
             [(and (symbol=? 'hearts (card-suit card1))
                   (symbol=? 'diamonds (card-suit card2))) true]
             [else false]))
           

(check-expect (bettersuit? (make-card 9 'hearts)
                           (make-card 9 'spades)) false)
(check-expect (bettersuit? (make-card 12 'hearts)
                           (make-card 12 'clubs)) true)
(check-expect (bettersuit? (make-card 2 'spades)
                           (make-card 2 'diamonds)) true)
(check-expect (bettersuit? (make-card 4 'diamonds)
                           (make-card 4 'clubs)) true)
(check-expect (bettersuit? (make-card 6 'hearts)
                           (make-card 6 'diamonds)) true)
(check-expect (bettersuit? (make-card 6 'heart)
                           (make-card 6 'spades)) false)



;; highcard-bettersuit: [listof Play] -> String
;; given a trick which have two palyer (four cards)
;; and returns the winner based on trick card rule :
;; 1. high value card won
;; 2. In the case of a tie i.e. two or more cards of the same value are played the trick 
;; goes to the player with the better suit. (Clubs < Diamonds < Hearts < Spades) 

(define (highcard-bettersuit trick)
  (play-player (foldr (lambda (p acc) (cond 
                           [(or (> (card-value (play-card p)) (card-value (play-card acc))) 
                                (and (equal? (card-value (play-card p)) (card-value (play-card acc))) (bettersuit? (play-card p) (play-card acc))))  p]
                           [else acc]))
         LOWPLAY trick)))

(check-expect (highcard-bettersuit trick1) "terry")
(check-expect (highcard-bettersuit trick2) "dan")
(check-expect (highcard-bettersuit trick4) "aci")
(check-expect (highcard-bettersuit trick3) "dan")
(check-expect (highcard-bettersuit trick5) "aci")


;; namecounter: String [Listof String] -> Number
;; compute the number of occurances of the string in a list of string.
(define (namecounter name lon)
  (cond 
    [(empty? lon) 0]
    [(string=? name (first lon)) (+ 1 (namecounter name (rest lon)))]
    [else (namecounter name (rest lon))]))

(check-expect (namecounter "claire" empty) 0)
(check-expect (namecounter "cici" (list "dan")) 0)
(check-expect (namecounter "dan" (list "dan" "aci" "cici" "jason")) 1)
(check-expect (namecounter "claire" (list "cici" "claire" "claire" "ii" "claire")) 3)




;; winners: [Listof [Listof Play]] -> [Listof Win]
;; produces a list of win which record how many times players have won in that game
(define (winners lop)
  (cond
    [(empty? lop) empty]
    [else (cons (make-wins (highcard-bettersuit (first lop))
                           (namecounter (highcard-bettersuit (first lop))
                                        (map highcard-bettersuit lop))) (winners (rest lop)))]))

(check-expect (winners (list trick1 trick2 trick3 trick4))
              (list
               (make-wins "terry" 1)
               (make-wins "dan" 2)
               (make-wins "dan" 1)
               (make-wins "aci" 1)))
(check-expect (winners (list trick1 trick2))
              (list (make-wins "terry" 1)
                    (make-wins "dan" 1)))


;; gamewinner: [listof [listof Play]] -> String
;; produce the name of the player who won most tricks
(define (gamewinner game)
 (wins-name (foldr (lambda (x acc) 
                     (if (> (wins-count x) (wins-count acc))
                         x acc)) 
                   (make-wins "ERROR" 0) (winners game))))

(check-expect (gamewinner empty) "ERROR")
(check-expect (gamewinner (list trick1)) "terry")
(check-expect (gamewinner (list trick1 trick2 trick3 trick4)) "dan")
(check-expect (gamewinner (list trick1 trick2 trick3 trick4 trick5)) "aci")
  
;; Problem 5


(define-struct node (left right))
; a Node is one of 
;- (make-node Btn Btn) 
;- (make-node Bts Bts)

;; a Btn (Binary Tree of Numbers) is one of:
; Number
; (make-node Btn Btn)

(define Btn1 (make-node (make-node 2 4) 5))
(define Btn2 (make-node 7 (make-node (make-node 1 2) 6)))

;; a Bts (Binary tree of Symbols) is one of:
; Symbol
; (make-node Bts Bts)

(define Bts1 (make-node 'Aba (make-node 'chalk 'paper)))
(define Bts2 (make-node (make-node 'penny 'nickel) (make-node (make-node 'dime 'quarter) 'dollar)))

 
;; a BtX (Binary tree of X) is one of:
; X
; (make-node BtX BtX)

;; BtX -> Number
;; counts the largest number of nodes you can travel to get to a leaf
(define (height btx)
  (cond 
    [(or (symbol? btx) (number? btx)) 0]
    [(> (height (node-left btx)) (height (node-right btx))) (+ 1 (height (node-left btx)))]
    [else (+ 1 (height (node-right btx)))]))
 (check-expect (height 5) 0) 
 (check-expect (height (make-node 'yes (make-node 'no 'maybe))) 2) 
 (check-expect (height (make-node (make-node 2 3) 4)) 2)
 
 
 
(define leaf 'leaf)
;; a Lbt (Leafy Bianary Tree) is one of 
 ; 'leaf
 ; (make-node Lbt Lbt)

;; Number -> [Listof Lbt]
;; produces a list of all Lbt with the given height
;;create-leafy-binary<: Number-> [Listof BT]
;;takes a number and return all BT of height less than n
(define (create-leafy-binary< n)
  (cond [(zero? n) empty]
        [else (append (all-bt (- n 1)) 
                      (create-leafy-binary< (- n 1)))]))
(check-expect (create-leafy-binary< 0) empty)
(check-expect (create-leafy-binary< 1) (list 'leaf))
(check-expect (create-leafy-binary< 2) (list (make-node 'leaf 'leaf) 'leaf))
(check-expect (create-leafy-binary< 3) (list
                                        (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))
                                        (make-node (make-node 'leaf 'leaf) 'leaf)
                                        (make-node 'leaf (make-node 'leaf 'leaf))
                                        (make-node 'leaf 'leaf)
                                        'leaf))


;;add: BT [Listof BT] -> [Listof BT]
;;takes a Bt and a list of BT
;;and returns a list of BT that 
;;add the given BT to every element of the list of BT
(define (add bt lobt)
  (cond [(empty? lobt) empty]
        [else (cons (make-node bt (first lobt))
                    (add bt (rest lobt)))]))
(check-expect (add (make-node 'leaf 'leaf) empty) empty)
(check-expect (add (make-node 'leaf 'leaf) (list (make-node 'leaf 'leaf)))
              (list (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))))
(check-expect (add (make-node 'leaf 'leaf) (list (make-node (make-node 'leaf 'leaf) 
                                                            (make-node 'leaf 'leaf))))
              (list (make-node (make-node 'leaf 'leaf) (make-node (make-node 'leaf 'leaf) 
                                                                  (make-node 'leaf 'leaf)))))


;;branch: [Listof BT] [Listof BT] -> [Listof BT]
;;takes two list of BT
;;and returns a list of BT which combine the given list of BT together
(define (branch a b)
  (cond [(empty? a) empty]
        [else (append (add (first a) b)
                      (branch (rest a) b))]))
(check-expect (branch empty empty) empty)
(check-expect (branch empty (make-node 'leaf 'leaf)) empty)
(check-expect (branch (list (make-node 'leaf 'leaf)
                            (make-node (make-node 'leaf 'leaf)
                                       'leaf))
                      (list (make-node 'leaf 'leaf)))
              (list
               (make-node (make-node 'leaf 'leaf) (make-node 'leaf 'leaf))
               (make-node (make-node (make-node 'leaf 'leaf) 'leaf) (make-node 'leaf 'leaf))))

;;all-bt: Number -> [Listof BT]
;;consumes a natural number n 
;;and creates (a list of) all leafy binary trees of height n
(define (all-bt n)
  (cond [(zero? n) (list 'leaf)]
        [else (append (branch (all-bt (- n 1)) 
                              (all-bt (- n 1)))
                      (branch (all-bt (- n 1)) 
                              (create-leafy-binary< (- n 1)))
                      (branch (create-leafy-binary< (- n 1)) 
                              (all-bt (- n 1))))]))

(check-expect (all-bt 0) (list 'leaf))
(check-expect (all-bt 1) (list (make-node 'leaf 'leaf)))
(check-expect (all-bt 2) (list(make-node (make-node 'leaf 'leaf) 
                                         (make-node 'leaf 'leaf))
                              (make-node (make-node 'leaf 'leaf) 'leaf)
                              (make-node 'leaf 
                                         (make-node 'leaf 'leaf))))
  


(check-expect (all-bt 2) 
              (list
               (make-node (make-node 'leaf 'leaf)
                          (make-node 'leaf 'leaf))
               (make-node (make-node 'leaf 'leaf) 'leaf)
               (make-node 'leaf (make-node 'leaf 'leaf))))








  
         


  

