;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Problem 1:

;;Combination is 
;(list Symbol Number)
;;[Listof Symbol] [Listof Number] -> [Listof Combination]
;;The function consumes a list of symbols and a list of numbers and 
;;produces all possible ordered pairs of symbols and numbers.
(define (cross los lon)
  (local [;;Symbol [Listof number] -> [Listof Combination]
          ;;Given a symbol and a list of number. Make each number in the list
          ;;pair with that symbol.
          (define (pair s lon)
             (map (lambda (x) (list s x)) lon))]
  (cond [(and (empty? los) (empty? lon)) empty]
        [(and (empty? los) (cons? lon))  empty]
        [(and (cons? los)
              (empty? lon)) empty]
        [(and (cons? los)
              (cons? lon)) 
        (apply append (map (lambda (x) (pair x lon)) los))])))
(check-expect (cross empty empty) empty)
(check-expect (cross '(a b c) empty) empty)
(check-expect (cross empty '(1 2)) empty)
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))


;;Problem 2:


(define-struct employee (n s p))
;;An employee is (make-employee String Number Number) 
;;Interpretation:
;;(make-employee n s p) which n is the name of the employee,
;;s is the social security number, p is the pay rate of the employee in hour.

(define-struct work-record (n h))
;; A work-record is (make-work-record String Number)
;;Interpretation:
;;(make-work-record n h) which n is the name of the employee, h is the 
;;number of hours work in a week.

(define-struct employee-wage (n w))
;;A newemployee is (make-newemployee String Number)
;;Interpretation:
;;(make-newemployee n w) which n is the name of the employee, w is the weekly wage 
;;of the employee
(define employee1 (make-employee "Terry" 001925096 20))
(define employee2 (make-employee "Jack" 194833989 10))
(define record1 (make-work-record "Terry" 40))
(define record2 (make-work-record "Jack" 30))
(define loe0 empty)
(define loe1 (list employee1 employee2))                
(define low0 empty)
(define low1 (list record1 record2))
                   

(define lon1 (list (make-employee-wage "Terry" 800) (make-employee-wage "Jack" 300)))
;;[Listof employee] [Listof work-record] -> [Listof newemployee]
;;Given a list of employee and a list of word-record returns a list of newemployee.

(define (wages*.v2 loe low)
  (local [;;Employee [Listof work-record] -> [Listof newemployee]
          ;;Given a employee then search the respons
          (define (search e low)
             (cond[(string=? (employee-n e) (work-record-n (first low)))
                   (make-employee-wage (employee-n e) 
                                     (* (work-record-h (first low)) (employee-p e)))]
                  [else (search e (rest low))]))]
    (cond [(empty? loe) empty]
          [(empty? low) (map (lambda (x) (make-employee-wage (employee-n x) 0)) loe)]                        
          [else (map (lambda (x) (search x low)) loe)])))
(check-expect (wages*.v2 loe1 low1) lon1)
(check-expect (wages*.v2 loe0 low1) empty)
(check-expect (wages*.v2 loe1 low0) (list (make-employee-wage "Terry" 0)
                                          (make-employee-wage "Jack" 0)))


;;Problem 3:


(define-struct phone-record (name number))
; A PhoneRecord is (make-phone-record String String).
(define p1  (make-phone-record "Terry" "3848303890"))
(define p2  (make-phone-record "Jason" "2394947590"))
(define lon0 empty)
(define lop0 empty)
(define lon01 (list "Terry" "Jason"))
(define lop01 (list "3848303890" "2394947590"))
;;[Listof String] [Listof String] -> [Listof Phone-record]
;;Given  a list of names and a list phone numbers,
;;then combines those equally long lists into a list of phone records.

(define (zip lon lop)
  (cond [(and (empty? lon) (empty? lop)) empty]
        [else (cons (make-phone-record (first lon) (first lop))
                    (zip (rest lon) (rest lop)))]))

(check-expect (zip lon0 lop0) empty)
(check-expect (zip lon01 lop01) (list p1 p2))



;;Problem 4:


(define-struct punch-card (number hours))
;; A punch-card is (make-punch-card Number Number)
;;Interpretation:
;;(make-punch-card number hours) which number is the employee number,
;;hours is the number of hours worked per week.

(define-struct employee-record (name number rate))
;;A employee-record is (make-employee-record String Number Number)
;;Interpretation:
;;(make-employee-record name number rate) which name is the name of 
;;the employee, number is the number of the employee, rate is the pay
;;rate counted in hours.

(define-struct wage-record (n w))
;; A work-record is (make-wage-record String Number)
;;Interpretation:
;;(make-wage-record n h) which n is the name of the employee, w is 
;; the weekly wage of an employee.

(define punch-card1 (make-punch-card 12 30))
(define punch-card2 (make-punch-card 13 40))
(define employee-record1 (make-employee-record "Terry" 12 20))
(define employee-record2 (make-employee-record "Jack" 13 30))
(define employee-record3 (make-employee-record "Ace" 14 20))

(define wage-record1 (make-wage-record "Terry" 600))
(define wage-record2 (make-wage-record "Jack" 1200))
(define lop000 empty)
(define lop001 (list punch-card1 punch-card2))

(define loe000 empty)
(define loe001 (list employee-record1 employee-record2))
(define loe002 (list employee-record3 employee-record2 employee-record1))
                  

;;ASSUME: THERE IS AT MOST ONE PUNCH-CARD RECORD PER EMPLOYEE NUMBER.

;;[Listof employee] [Listof punch-card] -> [Listof wage-record]
;;Given a list of employee 
;; list of punch-card records.
;; produces a list of wage records. If no punch card match the employee
;;it is a error.
(define (wages*.v3 loe lop)
  (local [;;Employee-record [Listof punch-card] -> [Listof wage-record]
          ;;Given a employee then search the respons
           (define (search e lop)
             (cond
                  [(and (cons? lop) (= (employee-record-number e) (punch-card-number (first lop))))
                   (make-wage-record (employee-record-name e) 
                                    (* (punch-card-hours (first lop)) (employee-record-rate e)))]
                  [else (search e (rest lop))]))
          ;;Employee-record [Listof punch-card]-> Boolean
          ;;Check wether the people information is on the punch-card.
           (define (exist? e lop)
             (cond [(empty? lop) false]
                   [else (or (= (employee-record-number e) (punch-card-number (first lop)))
                          (exist? e (rest lop)))]))]
        (if (ormap (lambda (x) (exist? x lop)) loe)
            (map (lambda (x) (search x lop)) loe)
            (error 'wages*.v3 "No Punch-card Matches The Staff."))))


(check-error  (wages*.v3 loe000 lop000))
(check-expect (wages*.v3 loe001 lop001) (list  wage-record1 wage-record2))
(check-error (wages*.v3 loe002 lop001))

;; Problem 5
;; [Listof Number] [Listof Number] -> Number
;; calculates the value for the combination of these numbers
(define (value coeff num)
  (cond 
    [(empty? coeff) 0]
    [else (+ (* (first coeff) (first num)) (value (rest coeff) (rest num)))]))
(check-expect (value empty empty) 0)
(check-expect (value (list 1 2 3 4) (list 4 3 2 1)) 20)
(check-expect (value (list 50) (list 2)) 100)


;; Problem 6:
;DNAprefix: [List-of Symbol] [List-of Symbol]-> Boolean
;;check if the pattern is a identical to the initial part of the search string.
;;and for [List-of Symbol], the symbol in the list only have 'a, 't, 'c, 'g. 
(define (DNAprefix pattern search)
  (cond
    [(empty? pattern) true]
    [(empty? search) false]
    [else (and (symbol=? (first pattern) (first search)) 
                   (DNAprefix (rest pattern) (rest search)))]))

(check-expect (DNAprefix (list 'a 'c 't 'g) (list 'a 'c 't 'g 'a 'c)) true)
(check-expect (DNAprefix empty empty) true)
(check-expect (DNAprefix (list 'a) empty) false)
(check-expect (DNAprefix empty (list 'a 't)) true)
(check-expect (DNAprefix (list 'a 'c 'g 't)
              (list 'a 'b 'e 't)) false)
(check-expect (DNAprefix (list 'a 'c 'g 't)
                         (list 'e 'c 'g 't)) false)





;same-dna: [List-of Symbol] [List-of Symbol]-> Boolean
;;Whether the two list are same.
(define (same-dna los1 los2)
  (cond [(and (empty? los1) (empty? los2)) true]
        [(or (empty? los1) (empty? los2)) false]
        [(and (cons? los1) (cons? los2))
         (and (symbol=? (first los1) (first los2))
              (same-dna (rest los1) (rest los2)))]))
(check-expect (same-dna (list 'a 't) (list 'a 't)) true)
(check-expect (same-dna (list 'a 't) (list 'a 't 't)) false)

;;Check is one of:
;; -false
;; -error
;; -Symbol

;;Assume the length of search is always equal or longer than pattern

;;DNAdelta: [List-of Symbol] [List-of Symbol]-> Check 

;;returns the first item in the search string beyond the pattern. 
;;if the lists are identical and there is no DNA letter beyond the pattern,
;;produece string:"error: indentical"
;;If the pattern does not match the beginning of the search string, 
;;the function returns false.

(define (DNAdelta pattern search)
  (local [;;[List-of Symbol] [List-of Symbol]-> Answer
          ;;Answer is one of:
          ;; -false
          ;; -Symbol
          ;;return the first item in new search string beyond new pattern 
          ;;or false when same length of new pattern and search 
          ;;but the element in new pattern does not match new search string
          (define (again-check p s)
            (cond [(and (empty? p) (not (empty? s))) (first s)]
                  [(and (empty? p) (empty? s)) false]
                  [(symbol=? (first pattern) (first search)) (again-check (rest p) (rest s))]))]
    (cond [(same-dna pattern search) (error "error: identical")]
          [(or (empty? pattern) (empty? search)) false]
          [(symbol=? (first pattern) (first search)) (again-check (rest pattern) (rest search))]
          [(not (symbol=? (first pattern) (first search))) false])))
          
        
             
(check-expect (DNAdelta empty (list 'a 't 'c)) false)
(check-expect (DNAdelta (list 'a 't 'c 't) empty) false)
(check-expect (DNAdelta (list 'a 't 'c 'g) (list 'a 't 'c 'g 'a)) 'a)
(check-expect (DNAdelta (list 't 'a 'c 't 'g) (list 't 'a 'c 't 'g 't 't 'a)) 't)
(check-expect (DNAdelta (list 't 'a 'c 't 'g) (list 'a 't 'c 't 'g 't)) false)
(check-expect (DNAdelta (list 'a 't 'c 'g) (list 'a 't 'c 't)) false)
(check-expect (DNAdelta (list 'a 't 'g 'c) (list 'a 't 'c 'g)) false)

(check-error (DNAdelta empty empty))
(check-error (DNAdelta (list 'g 'a 'c 't 'a 't) (list 'g 'a 'c 't 'a 't)))

;;I think may be we can use DNAprefix as a very important helper function to define
;;DNAdelta.It is a way to simplify the function.




;; Problem 7
;;The same to the problem 4.



;;Problem 8:

(define-struct leaf ())
(define-struct growth (next))
(define-struct fork (left right))
;; Tree is one of:
;; -- (make-leaf)
;; -- (make-growth Tree)
;; -- (make-fork Tree Tree)
;; Direction is one of:
;; -- 'left
;; -- 'right

(define tree1 (make-leaf))
(define tree2 (make-growth tree1))
(define tree3 (make-fork tree2 tree1))
(define tree4 (make-fork tree3 tree3))
(define tree5 (make-fork tree4 tree3))
(define lod1 empty)
(define lod2 (list 'left))
(define lod3 (list 'right))
(define lod4 (list 'left 'right 'right 'left 'left 'left 'right 'right))
(define lod5 (list 'left 'right))
;;(a).
;;Template:
#;(define (tree-template tree lod)
  (cond [(and (leaf? tree) (empty? lod)) ...]
        [(and (growth? tree) (empty? lod)) ...]
        [(and (fork? tree) (empty? lod)) ...]
        [(and (leaf? tree) (cons? lod)) ...(first lod)...(rest lod)..]
        [(and (growth? tree) (cons? lod))...(first lod)...
                                        ...(tree-template (growth-next tree) (rest lod))...
        [(and (fork? tree) (cons? lod)) ...(first lod)...
                                        ...(tree-template (fork-left tree) (rest lod))...
                                        ...(tree-template (fork-right tree) (rest lod)...)]]))


;; Tree [Listof Direction] -> Tree
;;The list of direction navigates the tree at the specified places.
(define (navigate tree lod)
  (cond [(and (leaf? tree) (empty? lod)) tree]
        [(and (growth? tree) (empty? lod)) tree]
        [(and (fork? tree) (empty? lod)) tree]
        [(and (leaf? tree) (cons? lod)) (make-leaf)]
        [(and (growth? tree) (cons? lod)) (navigate (growth-next tree) (rest lod))]
        [(and (fork? tree) (cons? lod))
         (cond [(symbol=? (first lod) 'left)
                (navigate (fork-left tree) (rest lod))]
               [(symbol=? (first lod) 'right)
                (navigate (fork-right tree) (rest lod))])]))
(check-expect (navigate tree1 lod4) (make-leaf))
(check-expect (navigate tree2 lod4) (make-leaf))
(check-expect (navigate tree3 lod2) tree2)
(check-expect (navigate tree4 lod5) tree1)
(check-expect (navigate tree4 lod1) tree4)

;;Problem 9:

; An S-expr (S-expression) is one of: 
; – Atom 
; – SL 
; An SL [List-of S-expr] is one of: 
; – empty 
; – (cons S-expr SL) 
; An Atom is one of: 
; – Number 
; – String 
; – Symbol

;;S-expression -> Boolean
;;check whethe S-expression is atom.
(define (atom? s)
  (or (string? s) (symbol? s) (number? s)))
(check-expect (atom? "hi") true)
(check-expect (atom? 12) true)
;;Atom Atom -> Boolean
;;Check whether two atoms are equal.
(define (atom=? a1 a2)
  (cond [(and (number? a1) (number? a2)) (= a1 a2)]
        [(and (string? a1) (string? a2)) (string=? a1 a2)]
        [(and (symbol? a1) (symbol? a2)) (symbol=? a1 a2)]
        [else false]))
(check-expect (atom=? 12 '13) false)
(check-expect (atom=? 'hi 12) false)
(check-expect (atom=? "Terry" 'Good) false)
(check-expect (atom=? "H" "H") true)
        
      




;;(a). S-expressions S-expressions -> Any
#;(define (sl-template sl)
    (cond [(empty? sl) ...]
          [(cons? sl) ... (first sl)..
                      ...(sl-temp (rest sl))...]))
    
    
    
    
#;(define (template-s-expressions s1 s2)
     (cond [(and (atom? s1) (atom? s2)) ...]
           [(and (atom? s1) (sl? s2)) ...(sl-template s2)...]
           [(and (sl? s1) (atom? s2)) ...(sl-template s1)...]
           [(and (sl? s1) (sl? s2))
            ... (sl-template (first s1))... (sl-template (first s2))...
            ...(template-s-expression (rest s1) (rest s2))...]))


;;(b).
;;atom [Listof S-expression] -> Boolean
;;Check whether the atom is in the [Listof S-expression].
(define (check atom los)
  (cond [(empty? los) false]
        [(cons? los) (if (atom? (first los))
                         (or (atom=? atom (first los)) (check atom (rest los)))
                         (or (check atom (first los)) (check atom (rest los))))]))
                                     
                                              
(check-expect (check 12 '(1 2 3 '(15 12))) true)
(check-expect (check 12 empty) false)
(check-expect (check 12 '(1 '(2 3) 12)) true)

;;S-expression S-expression -> Boolean
;;Whether the second  S-expressions contain the first S-expression but
;;regardless of the ordering.
(define (contains? s1 s2)
  (cond [(empty? s1) true]
        [(cons? s1) (if (atom? (first s1))
                        (and (check (first s1) s2) (contains? (rest s1) s2))
                        (and (contains? (first s1) s2) (contains? (rest s1) s2)))]))
(check-expect (contains? '(1 2) '(1 2 3)) true)
(check-expect (contains? '(3 4) '(1 2 3)) false)
(check-expect (contains? '(1 2 3) '(3 4)) false)
(check-expect (contains? '(1 '(2 3)) '(1 2 '(3 4))) true)


;;S-expression S-expression -> Boolean
;; Whether the elements in two lists are the same.
(define (contains-same-atoms? s1 s2)
  (and (contains? s1 s2)
       (contains? s2 s1)))

(check-expect (contains-same-atoms? '(1 2 3 () ("r" b))
'("r" 1 (2) 3 b)) true)
(check-expect (contains-same-atoms? '(1 2 3 4 '(2 3)) '(1 '(2 3) 4 2 3)) true)
(check-expect (contains-same-atoms? '(1 2 3 4 5) '(1 2 3 4 5 5 4)) true)
(check-expect (contains-same-atoms? empty empty) true)









         

        






















