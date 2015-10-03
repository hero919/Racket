;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1

;;(a).
;;An [Listof X] is one of:
;;-empty
;;-(cons X [Listof X])

; an [NEListof X] is one of:
; -(cons X [Listof X])


;;(b).
;; Number (which is nonnegative) -> [NEListof Number]
;; Produces a List of all squares from 0 to the provided number.
(define (all-int-squares n)
  (cond 
    [(= 0 n) (list 0)]
    [else (append (all-int-squares (- n 1)) (list (sqr n)))]))

(check-expect (all-int-squares 5) (list 0 1 4 9 16 25))
(check-expect (all-int-squares 0) (list 0))


;;(c).
;; a UOP is: Number -> Number
;; Number (nonnegative) UOP -> [NEListof Number]
;; Produces a List of all results of o from 0 to the provided number
(define (all-int-results n o)
  (cond 
    [(= 0 n) (list (o 0))]
    [else (append (all-int-results (- n 1) o) (list (o n)))]))

(check-expect (all-int-results 5 add1) (list 1 2 3 4 5 6))
(check-expect (all-int-results 2 sub1) (list -1 0 1))

;; Number (nonnegative) -> [NEListof Number]
;;Use all-int-results to represent the all-int-squares.
(define (all-int-squares2 n)
  (all-int-results n sqr))

(check-expect (all-int-squares2 5) (all-int-squares 5))
(check-expect (all-int-squares2 0) (all-int-squares 0))

;;(d).
;; Number (which is nonnegative) -> [NEListof Number]
;; Produces a List of all values doubled from 0 to the provided number
(define (all-int-doubles n)
(local 
  [;; Number -> Number
   ;; doubles the provided value
  (define (double x) 
    (* 2 x))]
  (all-int-results n double)))
(check-expect (all-int-doubles 5) (list 0 2 4 6 8 10))
(check-expect (all-int-doubles 0) (list 0))


;; Problem 2

;;(a).
;; [List-of String] String -> Boolean
;; checks if the provided string is in the list
(define (find-string los s)
  (ormap (lambda (x) (string=? s x)) los))

(check-expect (find-string (list "all" "the" "little" "piggies" "went" "for" "tea")
                           "piggies") true)
(check-expect (find-string (list "all" "the" "little" "piggies" "went" "for" "tea")
                           "foxes") false)

;;(b).
;; [Listof String] String [String String-> Boolean] -> Boolean
;; checks if the provided String is in the list.
(define (generic-find-string los s funct)
  (ormap (lambda (x) (funct s x)) los))


;; [List-of String] String -> Boolean
;; checks if the provided string is in the list (case sensitive)
(define (find-string-case-sensitive los s)
  (generic-find-string los s string=?))



(check-expect (find-string-case-sensitive 
               (list "all" "the" "little" "piggies" "went" "for" "tea") 
               "piggies") 
              (find-string (list "all" "the" "little" "piggies" "went" "for" "tea")
                           "piggies"))
(check-expect (find-string-case-sensitive 
               (list "all" "the" "little" "piggies" "went" "for" "tea") 
               "Piggies") 
              (find-string (list "all" "the" "little" "piggies" "went" "for" "tea")
                           "foxes"))


;; [List-of String] String -> Boolean
;; checks if the provided string is in the list (case insensitive)
(define (find-string-case-insensitive los s)
  (generic-find-string los s string-ci=?))

(check-expect (find-string-case-insensitive 
               (list "all" "the" "LIttle" "Piggies" "went" "For" "tea")
               "piggies") true)
(check-expect (find-string-case-insensitive 
               (list "all" "the" "liTTle" "piggies" "weNt" "for" "tea")
               "Piggies") true)
(check-expect (find-string-case-insensitive 
               (list "All" "tHe" "little" "piggIes" "went" "for" "tea")
               "foxes") false)

;; Problem 3

;; A Grade is: (make-grade Symbol Number) 
(define-struct grade (letter num)) 
 
;; The Symbol in a Grade represents 
 
;; 'A >= 90 
;; 'B >= 80 
;; 'C >= 70 
;; 'D >= 60 
;; 'F < 60 
 
;; A [Listof Grades] ... 
(define grades 
 (list (make-grade 'D 62) (make-grade 'C 79) (make-grade 'A 93) 
 (make-grade 'B 84) (make-grade 'F 57) (make-grade 'F 38) 
 (make-grade 'A 90) (make-grade 'A 95) (make-grade 'C 76) 
 (make-grade 'A 90) (make-grade 'F 55) (make-grade 'C 74) 
 (make-grade 'A 92) (make-grade 'B 86) (make-grade 'F 43) 
 (make-grade 'C 73)))

(define grades2 (list (make-grade 'A 99) (make-grade 'B 86)))

;;(a).
;; [List-of Grade] -> [List-of Symbol]
;; returns the letter grade

(define (log->los log)
  (map grade-letter log))

(check-expect (log->los grades) (list 'D 'C 'A 'B 'F 'F 'A 'A 'C 'A 'F 'C 'A 'B 'F 'C))
(check-expect (log->los grades2) (list 'A 'B))

;;(b).
;;[List-of Grade] -> Number
;; finds the average score in the list
(define (average-grade log)
  (/ (foldr (lambda (x a) (+ (grade-num x) a)) 0 log) (length log)))
(check-expect (average-grade grades) 74.1875)
(check-expect (average-grade grades2) 92.5)

;;(c).
;;[List-of Grade] -> [List-of Grade]
;; removes all grades of 79 or less
(define (all-above-79 log)
  (filter (lambda (x) (< 79 (grade-num x))) log))

(check-expect (all-above-79 grades)  
              (list (make-grade 'A 93)  (make-grade 'B 84) (make-grade 'A 90) 
                    (make-grade 'A 95) (make-grade 'A 90)  (make-grade 'A 92) (make-grade 'B 86)))
(check-expect (all-above-79 grades2)
              grades2)


;;(d).
;; [List-of Grade] -> Boolean
;; checks to see if there are no F grades
(define (all-pass? log)
  (andmap (lambda (x) (not (symbol=? 'F (grade-letter x)))) log))
  
  (check-expect (all-pass? grades) false)
  (check-expect (all-pass? (all-above-79 grades)) true)
  (check-expect (all-pass? grades2) true)
;;(e).
;; [List-of Grade] -> [List-of Grade] 
;; adds 5 points to the each grade and updates the letter value
(define (bonus log)
  (local[ ;;Grade-> Number
          ;;Given a grade make it number add 5 and return that number.
         (define (add5 g)
             (+ (grade-num g) 5)) 
          ;;Grade-> Grade
          ;; After add 5 points to each grade, show the new grade of this student.
          (define (fix g)
            (cond [(>= (add5 g) 90) (make-grade 'A (add5 g))]
                  [(>= (add5 g) 80) (make-grade 'B (add5 g))]
                  [(>= (add5 g) 70) (make-grade 'C (add5 g))]
                  [(>= (add5 g) 60) (make-grade 'D (add5 g))]
                  [else (make-grade 'F (add5 g))]))]
         (map fix log)))
  
    (check-expect (bonus grades)  
                  (list (make-grade 'D 67) (make-grade 'B 84) (make-grade 'A 98) 
 (make-grade 'B 89) (make-grade 'D 62) (make-grade 'F 43) 
 (make-grade 'A 95) (make-grade 'A 100) (make-grade 'B 81) 
 (make-grade 'A 95) (make-grade 'D 60) (make-grade 'C 79) 
 (make-grade 'A 97) (make-grade 'A 91) (make-grade 'F 48) 
 (make-grade 'C 78))) 
 (check-expect (bonus grades2) (list (make-grade 'A 104) (make-grade 'A 91)))
    
;; Problem 4
    
(define-struct child (father mother name date hair-color)) 
;; A FTN (Family-tree-node) is one of: 
;; - empty 
;; - (make-child FTN FTN Symbol Number Symbol) 
(define person1 (make-child empty empty 'John 1984 'red))
(define person2 (make-child empty empty 'Jane 1985 'black))
(define person3 (make-child empty empty 'Terry 1995 'black))
(define person4 (make-child empty empty 'Diana 1995 'red))
(define FT1 (make-child person1 person2 'Kate 2009 'black))
(define FT2 (make-child FT1 person3 'Bob 1970 'black))


;;(a).
;; FTN Number -> Number
;; returns the number of people born the given year or earlier
(define (count-older ftn year)
  (cond 
    [(empty? ftn) 0]
    [(>= year (child-date ftn)) (+ 1 (count-older (child-father ftn) year) 
                                   (count-older (child-mother ftn) year))]
    [else (+ (count-older (child-father ftn) year) (count-older (child-mother ftn) year))]))
(check-expect (count-older empty 1990) 0)
(check-expect (count-older FT1 1990) 2)
(check-expect (count-older FT1 2010) 3)
(check-expect (count-older FT1 1800) 0)

;;(b).
;; FTN Number -> [List-of String]
;; returns a list of all people born on or before the given year
(define (search-tree-older ftn n)
     (cond [(empty? ftn) empty]
           [(child? ftn) (if (<= (child-date ftn) n)
                         (cons (child-name ftn) (append (search-tree-older (child-father ftn) n)
                                                        (search-tree-older (child-mother ftn) n)))
                         (append (search-tree-older (child-father ftn) n)
                                                        (search-tree-older (child-mother ftn) n)))]))
            
         
(check-expect (search-tree-older FT1 1990) (list 'John 'Jane))
(check-expect (search-tree-older FT1 1984) (list 'John))
(check-expect (search-tree-older FT1 1800) empty)


;;(c).
;;FTN-> Boolean 
;; Determine whether a family-tree-node contains an ancestor
;;with red hair on the father's side and
;;an ancestor with red hair on the mother's side.
 (define (red-haired-ancestors? ftn)
     (local [;;FTN-> Boolean
             ;;Check one side wether there is a red hair in that side.            
           (define (helper1 ftn)
             (cond [(empty? ftn) false]
                   [else (or 
                   (symbol=? (child-hair-color ftn) 'red)
                   (helper1 (child-father ftn))
                   (helper1 (child-mother ftn)))]))]
       (cond [(empty? ftn) false]
             [(child? ftn) (and (helper1 (child-father ftn))
                              (helper1 (child-mother ftn)))])))
(check-expect (red-haired-ancestors? FT1) false)
(check-expect (red-haired-ancestors? empty) false)




;;(d).
;;FTN FTN -> FTN
;;Given two FTN use the first FTN to subtitude the second one father part.

(define (update-father f1 f2)
  (cond [(empty? f1) f2]      
        [else (make-child f2 (child-mother f1) (child-name f1) 
                          (child-date f1) (child-hair-color f1))]))

(check-expect (update-father FT1 FT2)
              (make-child FT2 (child-mother FT1) 'Kate 2009 'black))
(check-expect (update-father FT1 person2)
              (make-child person2 (child-mother FT1) 'Kate 2009 'black))
(check-expect (update-father empty empty) empty)
(check-expect (update-father empty person2) person2)



           