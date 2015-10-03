;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;Problem 1:
;;(a).
(define-struct friend (n l r))
;;A friend is a (make-friend String String String)
;;Interpretation:(make-friend n l r) which n is the name of the friend,
;;location is the location of this friend, the relationship is the relationship
;;between the user and this friend.

(define-struct profile (n l r lof))
;;Profile is (make-struct String String String List)
;;Interpretation: (make-profile n l r lof) which n is the name of the user name,
;;l is the location of the user
;;r is the relationship status of the user
;;lof is one of:
;;-empty
;;-(cons friend lof)
;;(b).
;;template for friend:
#;(define (friend-temp a)
    ...(friend-n a)...
    ...(friend-l a)...
    ...(friend-r a)...)

;;template for lof:
#;(define (list-temp a)
    (cond[(empty? a)...]
         [(cons? a)...(first a)...
                   ...(list-temp (rest a))...]))

;;template for profile:
#;(define (profile-temp a)
    ...(profile-n a)...(profile-l a)...(profile-r a)...
    ...(profile-lof)...)

;;(c).
;;lof is one of:
;;-empty
;;-(cons Number lof)
;;lof->Number
;; Takes a lof and calculates the number of friends in it.
(define (count a)
  (cond[(empty? a) 0]
       [(cons? a)
        (+ 1 (count (rest a)))]))

;;Profile->Number
;;Takes a profile, calculates the total number of friends the profile has.
(define (total-friends a)
  (count (profile-lof a)))
(define f1 (make-friend "Alex" "New York" "Married"))
(define f2 (make-friend "Claire" "Boston" "Single"))
(define f3 (make-friend "May" "Chicago" "Single"))
(define f4 (make-friend "Jack" "Miami" "Married"))
(define f5 (make-friend "Anna" "New York" "Single"))

(define lof1 empty)
(define lof2 (cons f1 empty))
(define lof3 (cons f1 (cons f2 empty)))
(define lof4 (cons f1 (cons f2 (cons f3 empty))))
(define lof5 (cons f1 (cons f2 (cons f3 (cons f4 empty)))))
(define lof6 (cons f1 (cons f2 (cons f3 (cons f4 (cons f5 empty))))))

(define p1 (make-profile "Terry" "Boston" "Single" lof1 ))
(define p2 (make-profile "Zee" "Chicago" "Married" lof2 ))
(define p3 (make-profile "John" "Indianapolis" "Married" lof3))
(define p4 (make-profile "Jack" "Miami" "Married" lof6))
(define p5 (make-profile "Anna" "New York" "Single" lof5))



(check-expect (total-friends p1) 0)
(check-expect (total-friends p2) 1)
(check-expect (total-friends p3) 2)



;;(d).
;;lof Friend->Boolean
;;Takes a lof and a friend. Determines whether a friend is in the list of friends of a profile.
(define (in? a b)
  (cond[(empty?  a) false]
       [(cons? a) (or (and (string=? (friend-n (first a)) (friend-n b))
                           (string=? (friend-l (first a)) (friend-l b))
                           (string=? (friend-r (first a)) (friend-r b)))
                      (in? (rest a) b))]))

(check-expect (in? lof2 f1) true)
(check-expect (in? lof3 f4) false)

;;Profile Friend->Profile 
;;Takes a profile and a friend. If the friend is in the lof, nothing changes.
;;If the friend is not in the lof, adds it.
(define (add-friend a b)
  (cond[(in? (profile-lof a) b)
        a]
       [else
        (make-profile (profile-n a) (profile-l a) (profile-r a)
                      (cons b (profile-lof a)))]))

(check-expect (add-friend p2 f1) p2)
(check-expect (add-friend p3 f3) (make-profile (profile-n p3) (profile-l p3) (profile-r p3)
                                               (cons f3 (profile-lof p3))))



;;(e).
;;Lof Friend->Lof
;;Takes a lof and a friend. If the friend is not in the lof, nothing changes.
;;If the friend is in the lof, deletes it. a is a lof, b is a friend.
(define (un-friendhelper a b)
  (cond
    [(empty? a) empty]
    [else (if (string=? (friend-n b) (friend-n (first a)))
              (rest a)
              (cons (first a) (un-friendhelper (rest a) b)))]))
      

(check-expect (un-friendhelper lof4 f3) (cons f1 (cons f2 empty)))
(check-expect (un-friendhelper lof6 f2) (cons f1 (cons f3 (cons f4 (cons f5 empty)))))


;;Profile Friend->Profile
;;Takes one profile and friend, if the friend is not in the lof, nothing changes.
;;if the friend is in the lof, deletes it. p is a lof, f is a friend.
(define (un-friend p f)
  (make-profile (profile-n p) (profile-l p) (profile-r p)
                (un-friendhelper (profile-lof p) f)))

(check-expect (un-friend p2 f3) (make-profile (profile-n p2) (profile-l p2) (profile-r p2)
                                              (profile-lof p2)))
(check-expect (un-friend p3 f2) (make-profile (profile-n p3) (profile-l p3) (profile-r p3)
                                              (cons f1 empty)))
;;(f).
;;Profile Profile->Boolean
;;Takes two profiles and determines whether they are friends.
(define (friends? p1 p2)
  (or (in? (profile-lof p1) (make-friend (profile-n p2) (profile-l p2)
                                         (profile-r p2)))
      (in? (profile-lof p2) (make-friend (profile-n p1) (profile-l p1)
                                         (profile-r p1)))))

(check-expect (friends? p2 p3) false)
(check-expect (friends? p4 p5) true)
;;(g).
;;lof->String
;;Takes a lof, produces a string.
(check-expect (print-friendshelper lof2) "Alex")
(check-expect (print-friendshelper lof3) "Alex Claire")

(define (print-friendshelper lof)
  (cond[(empty? (rest lof))
        (friend-n (first lof))]
       [else (string-append (friend-n (first lof)) " " (print-friendshelper (rest lof)))]))
;;Profile->String
;;Takes a profile, produces a string
(check-expect (print-friends p2) "Alex")
(check-expect (print-friends p3) "Alex Claire")

(define (print-friends p)
  (print-friendshelper (profile-lof p)))


;;Problem 2:
;; A LOS is one of
;; empty
;; (cons String LOS)
;; 
;; A Table is one of
;; empty
;; (cons (cons String (cons String empty)) Table)

;; build-table: LOS LOS -> Table
;; build a table from two lists of strings 
;; assume: lists are equally long 
(define (build-table domain range)
  (cond
    [(empty? domain) empty]
    [else (cons (list (first domain) (first range)) 
                (build-table (rest domain) (rest range)))]))

(check-expect (build-table (list "a") (list "b")) 
              (list (list "a" "b")))

(define table-1
  (build-table 
   (explode "abcdefghijklmnopqrstuvwxyz?!’")
   (explode "ɐqɔpǝɟƃɥıɾʞןɯuodbɹsʇnʌʍxʎz¿¡,")))

;;String Table->String
;;Takes a string and a table. Shows the inverted character.

(define (invert l table)
  (cond[(empty? table) l]
       [(string=? l (first (first table)))
        (first (rest (first table)))]
       [else (invert l (rest table))]))
(define table1 (list
                (list "a" "ɐ")))
(define table2 (list
                (list "a" "ɐ")
                (list "b" "q")))

(check-expect (invert "a" table1)  "ɐ")
(check-expect (invert "b" table2)  "q")

;;LOS->LOS
;;Takes a LOS and produces a LOS with inverted versions of the characters 
;;in the first list.
(define (explode-invert s)
  (cond[(empty? s) empty]
       [(cons? s)
        (cons (invert (first s) table-1)
              (explode-invert (rest s)))]))

(check-expect (explode-invert (list "a" "b" "c"))
              (list "ɐ" "q" "ɔ"))

;;String->String
;;Takes a string and produces an inverted string.
(define (invert-string a)
  (implode (explode-invert (explode a))))

(check-expect (invert-string "zeqing") "zǝbıuƃ")
(check-expect (invert-string "zeqing zhang")  "zǝbıuƃ zɥɐuƃ")