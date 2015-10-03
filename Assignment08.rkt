;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;Problem 1:

;;A [List-of X] is one of:
;; -empty
;; -(cons X [List-of X])

;;tabulate: X->[List-of X]
;;given a X and produce a [List-of X]
(define (tabulate op x)
  (cond [(= x 0) (list (op 0))]
        [else (cons (op x)
                    (tabulate op (sub1 x)))]))

;;tab-sqrt: Number-> [List-of Number]
;;given a Number which is greater or equal than 0
;;and produce a [List-of (sqrt (natural number))] 
;;which start with the given  Number
;;and then subtract 1 every time untill euqal 0.
(define (tab-sqrt x)
  (tabulate sqrt x))




(check-expect (tab-sqrt 0) (list (sqrt 0)))
(check-expect (tab-sqrt 1) (cons (sqrt 1)
                                 (list (sqrt 0))))

;;tab-tan: Number -> [List-of Number]
;;given a Number which is greater or equal than 0
;;and produce a list-of (tan natural number) 
;;which start with the given Number
;;and then subtract 1 every time untill euqal 0.
(define (tab-tan x)
  (tabulate tan x))

(check-expect (tab-tan 0) (list (tan 0)))
(equal? (tab-tan 3) (list (tan 3) (tan 2) (tan 1) (tan 0)))

(equal? (tab-tan 2) (list (tan 2) (tan 1) (tan 0)))



;;Problem 2:
;;A [Listof Number] is one of:
;;-empty
;;-(cons Number [Listof Number])

; (Number -> Boolean)
; (Boolean String -> Boolean)
; (Number Number Number -> Number)
; (Number -> [List-of Number])
; ([List-of Number] -> Boolean)


;;(Number -> Boolean)
;;Given number, returns boolean.
;;For example 
;;Given a number, determine wether it is odd.
(define (check-odd n)
  (cond [(odd? n) true]
        [else false]))
(check-expect (check-odd 15) true)
(check-expect (check-odd 12) false)



;;(Boolean String -> Boolean)
;;Given boolean and string, returns a boolean.
;;For example
;;If it gives true, then determine whether the string-length is greater
;;than 5. If it gives false determine whether the string length is less
;;than 7
(define (check1 b s)
  (cond[(boolean=? b true) (> (string-length s) 5)]
       [(boolean=? b false) (< (string-length s) 7)]))


(check-expect (check1 true "Love") false)
(check-expect (check1 false "The nature of promise") false)



;;(Number Number Number -> Number)
;;Given Number Number Number, returns Number
;;For example
;;Calculate the mutiplication of the first two Numbers then add the 
;;third one.
(define (cal n1 n2 n3)
  (+ n3 (* n1 n2)))

(check-expect (cal 1 2 3) 5)
(check-expect (cal 2 3 4) 10)


;;(Number -> [List-of Number])
;;Given a number, returns a list.
;;For example
;;Given a number which is greater than 0, convert it in the form of list.

(define (convert n)
  (cond[(= n 0) (cons 0 empty)]
       [else (cons n (convert (sub1 n)) )]))

(check-expect (convert 5)
              (list 5 4 3 2 1 0))
(check-expect (convert 3)
              (list 3 2 1 0))

;;([List-of Number] -> Boolean)
;;Given list of number, returns boolean
;;For example 
;;Determine whether the first number of the list is 
;;greater than 5
(define (check2 lon)
  (> (first lon) 5))

(check-expect (check2 (cons 6 (cons 3 empty))) true)
(check-expect (check2 (cons 4 (cons 7 empty))) false)


;;Problem 3:

;; sort-n: [Listof Number][Number Number-> Boolean]->[Listof Number]



;;  sort-s: [Listof String] [String String->Boolean]->[Listof String]


;; sort-x:  [X] [Listof X][X X-> Boolean]-> [Listof X]
;;If X is Number then it becomes sort-n.
;;If X is String then it becomes sort-s.

;; For IR the signature is:
;;[Listof IR] [IR IR-> Boolean] -> [Listof IR]


;;Problem 4:

;;map-n: [Listof Number] [Number-> Number]  -> [Listof Number]


;;map-s: [Listof String] [String->String] -> [Listof String]

;;map-y: [Y] [Listof Y] [Y->Y] -> [Listof Y]
;;If Y is Number then it becomes map-n.
;;If Y is String then it becomes map-s.

;;If Y is IR, the signature is 
;;map-IR: [Listof IR] [IR-> IR] -> [Listof IR]

;;Problem 5:

;;A Ploygon is one of:
;;- (list Posn Posn Posn)
;;- (cons Posn Polygon)

;;A NELoP is one of:
;;- (cons Posn empty)
;; -(cons Posn NELoP)

(define MT (empty-scene 500 500))


;;render-polygon: Polygon-> Image
;; add the Polygon p into an image in MT
(define (render-polygon p)
  (local [;; NELoP -> Posn
          ;; extract the last Posn from p
          (define (last p)
            (cond [(empty? (rest (rest (rest p)))) (third p)]
                  [else (last (rest p))]))
          ; NELoP -> Image
          ; connect the Posns in p
          (define (connect-dots p)
            (cond [(empty? (rest p)) MT]
                  [else (render-line
                         (connect-dots (rest p))
                       (first p) (second p))]))]
    
    (render-line (connect-dots p) (first p) (last p))))



; render-line: Image Posn Posn -> Image
; draw a red line from Posn p to Posn q into im
(define (render-line im p q)
  (add-line 
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))


(define p1 (list (make-posn 20 0)
                                  (make-posn 10 10)
                                  (make-posn 30 10)))
(define p2 (cons (make-posn 10 10) (list
                                    (make-posn 10 20)
                                    (make-posn 20 10)
                                    (make-posn 20 20))))
 (check-expect (render-polygon p1)
              (add-line 
               (add-line
                (add-line MT 20 0 10 10 "red")
                20 0 30 10 "red")
               10 10 30 10 "red"))
(check-expect (render-polygon p2)
              (add-line (add-line (add-line (add-line MT 10 10 10 20 "red")
                                            10 20 20 10 "red")
                                  20 10 20 20 "red")
                        20 20 10 10 "red"))



;;Problem 6:


;;A [Listof Number] is one of:
;;-empty
;;-(cons Number [Listof Number])


;;(a).
;;[Listof Number]-> [Listof Number]
;;Convert a list of dollar amounts to a list of euro amounts.

(define (convert-euro lon)
  (local[;;Number->Number
         ;;Conver the dollar amounts to euro amounts.
         (define (f1 x)
           (* 1.22 x))]
    (map f1 lon)))

(check-expect (convert-euro '(1 2 3))
              '(1.22 2.44 3.66))
(check-expect (convert-euro '(5))
                            '(6.1))



;;(b).

;;[Listof Number]-> [Listof Number]
;;Converts a list of Fahrenheit measurements 
;;to a list of Celsius measurements.

(define (convertFC lon)
  (local[;;Number->Number
         ;;Convert Fahrenheit measurements to
         ;;Celsius measurements
         (define (f2 x)
           (/ (- x 32) 1.8))]
    (map f2 lon)))

(check-expect (convertFC '(32))
              '(0))
(check-expect (convertFC '(33.8 50))
              '(1 10))



;;(c).
;;A [Listof Posn] is one of:
;;-empty
;;-(cons posn [Listof Posn])

;;[Listof Posn]-> [Listof [list Number Number]]
;; Translates a list of Posns into a list of list of pairs of numbers.

(define (translate lop)
  (cond [(empty? lop) empty]
        [else (cons (list (posn-x (first lop))
                          (posn-y (first lop))) (translate (rest lop)))]))

(check-expect (translate (list (make-posn 3 4)
                               (make-posn 5 6)))
              (list (list 3 4)
                    (list 5 6)))
(check-expect (translate (list (make-posn 5 6)
                               (make-posn 7 8)))
              (list (list 5 6)
                    (list 7 8)))

;;Problem 7:

(define-struct toy (name des ap p))
;;A toy is a (make-toy String String Number Number)
;;Interpretation:
;;a toy is (make-toy name des ap p) which name is the 
;;name of the toy, des is the description of the toy, ap is the acquisition price
;;of the toy and p is the price of the toy.
(define t1 (make-toy "Teddy" "bear" 50 70))
(define t2 (make-toy "Tom" "cat" 30 40))
(define t3 (make-toy "Spiderman" "man" 80 90))
;; A [Listof toy] is one of:
;;-empty
;;-(cons toy [Listof toy])

;;Number [Listof toy] -> [Listof toy]
;;Given a Number and a list of toy 
;;produces a list of all those structures 
;;whose sales price is below the Number.
(define  (eliminate-exp ua lot)
  (cond [(empty? lot) empty]
        [(and (cons? lot)
              ( < (toy-p (first lot)) ua))
         (cons (first lot) (eliminate-exp ua (rest lot)))]
        [else (eliminate-exp ua (rest lot))]))

(check-expect (eliminate-exp 50 (list t1 t2 t3))
              (list t2))
(check-expect (eliminate-exp 500 (list t1 t2 t3))
              (list t1 t2 t3))
              

;;String [Listof toy] -> [Listof toy]
;;Given the name of a toy 
;;and a list of toy, produces a list of toy that do not use that name.
(define (recall ty lot)
  (local [;String toy-> Boolean
          ;; Given the name of the toy and the name of the toy in the list
          ;;determine whether they are the same.
            (define (nty? a-lot)
               (not (string=? ty (toy-name a-lot))))]
  (filter nty?  lot)))

(check-expect (recall "Teddy" (list t1 t2 t3))
              (list t2 t3))
(check-expect (recall "Spiderman" (list t1 t2 t3))
              (list t1 t2))

;;[Listof Names] is one of:
;;-empty
;;-(cons Name [Listof Names])

;;[Listof Names] [Listof Names] -> [Listof Names]
;;Consumes two lists of names and selects all those from 
;;the second one that are also on the first. 
(define (selection lon1 lon2)
  (local [ ;;String-> Boolean
          ;;Determine whether x is in lon1 list.
          (define (inlon1? x)
             (local[;;String-> Boolean
                    ;;Determine whether two strings are equal.
                    (define (x=? y)
                       (string=? x y))]
               (ormap x=? lon1)))]
    (filter  inlon1? lon2)))
(check-expect (selection (list "Teddy" "Tom")
                         (list "Tom" "Spiderman"))
             (list "Tom"))
(check-expect (selection (list "Tom" "Teddy")
                         (list "Teddy" "Spiderman"))
              (list "Teddy"))
                         
;;Problem 8:
;;Fold-map: [X->Y] [Listof X]-> [Listof Y]
;;Use fold to represent map.
(define (fold-map op lox)
  (local [;;X [Listof Y] -> [Listof Y]
          (define (opra a loy)
                  (cons (op a) loy))]
          (foldr opra empty lox)))


(check-expect (fold-map add1 '(5 6 7))
              (map add1 '(5 6 7)))
(check-expect (fold-map sub1 '(2 3 4))
              '(1 2 3))

               
    
           






























  























  
            
            
            

            


























 
 
 
 
 
 
 
 
 
 














