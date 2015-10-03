;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;;Problem 1:
;;(a).
;;A LOP is one of:
;; -empty
;; -(cons String LOP)


;;check-pass: LOP->Boolean
;; whether all the string in list are between 6 and 10 characters, trrue
;;is yes, false is no.

(define (check-pass-6-10 a-lop)
  (cond[(empty? a-lop) true]
       [(cons? a-lop)
        (cond[ (<= 6 (string-length (first a-lop)) 10) 
        (check-pass-6-10 (rest a-lop))]
       [else false])]))
        
(define lop1 empty)
(define lop2 (cons "terryclair" empty))
(define lop3 (cons "terryclair" (cons "clai" empty)))
(define lop4 (cons "terryclai" (cons "terryclair" empty)))
(define lop5 (cons "aa" empty))


(check-expect (check-pass-6-10 lop1) true)
(check-expect (check-pass-6-10 lop2) true)
(check-expect (check-pass-6-10 lop3) false)
(check-expect (check-pass-6-10 lop4) true)
(check-expect (check-pass-6-10 lop5) false)



;;(b).
;;check-pass: List-of-passwords Number Number->Boolean
;;Interpretation:
;;Determine whether all passwords are within the allowed length span.
;;b is always bigger or equal than a.
(define (check-pass? a-lop a b)
  (cond[(empty? a-lop) true]
       [(cons? a-lop)
        (cond[ (<= a (string-length (first a-lop)) b) 
        (check-pass? (rest a-lop) a b)]
       [else false])]))
(check-expect (check-pass? lop1 2 9) true)
(check-expect (check-pass? lop2 2 13) true)
(check-expect (check-pass? lop3 50 51) false)
(check-expect (check-pass? lop2 10 10) true)



;;2
;;(a).
(define-struct entry (name image))
;;A Entry is a (make-entry Symbol Image)
;;interp:
;;A (make-entry n i)
;;where n represent the name of entry( as a Symbol)
;;it can be 'circle, 'rectangle, 'triangle, 'ellipse, 'line and so on
;;and i represent a corrspoding example of entry, which is a image
;;it can be a circle, rectange, triangle, ellipse, line and so on

;;(b).
;;A Catalog is one of:
;; -empty
;; -(cons Entry Catalog)
;;interp: a catalog is cons, contains any number of entries
 

;;Example:
;;For Catalog:
(define c1 empty)
(define c2 (cons(make-entry 'circle (circle 10 'solid 'red)) empty))
(define c3 (cons 
            (make-entry 'circle (circle 21 'solid 'red))
            (cons
             (make-entry 'line (line 15 14 'red)) empty)))
(define c4 (cons 
            (make-entry 'line (line 15 15 'black)) 
            (cons
             (make-entry 'triangle (triangle 30 'solid 'green))
             (cons
              (make-entry 'square (square 10 'solid 'green)) empty))))
(define c5 (cons 
            (make-entry 'line (line 20 10 'red)) empty))
;;(c}.
(define e1 (make-entry 'circle (circle 10 'solid 'red)))
(define e2 (make-entry 'circle (circle 21 'solid 'red)))
(define e3 (make-entry 'ellipse (ellipse 30 20 'solid 'green)))
(define e4 (make-entry 'triangle (triangle 30 'solid 'blue)))
(define e5 (make-entry 'line (line 15 15 'red)))


(check-expect (show-example c1 e1) false)
(check-expect (show-example c1 e2) false)
(check-expect (show-example c3 e2) (circle 21 'solid 'red))
(check-expect (show-example c4 e4) (triangle 30 'solid 'green))

;;show-example: Entry Catalog-> Image/ Boolean
;; produces the corresponding image of an entry and returns false 
;; if the named figure was not in the catalog
(define (show-example c e)
  (cond [(empty? c) false]
        [(cons? c) 
         (cond 
           [(symbol=? (entry-name (first c)) (entry-name e))
            (entry-image (first c))]
           [else (show-example (rest c) e)])]))
        
        

;;Problem 3:

;;a list-of-symbol is one of:
;;-empty
;;-(cons symbol list-of-symbol)

(define ex4 (cons 'wurst (cons 'huevos (cons 'pizza (cons 'pants empty)))))
(define ex5 empty)

;;cesarify: list-of-symbol->list-of-symbol
;;given a list of symbols and returns the same list but 
;;with every instance of 'pizza doubled
(define (cesarify a)
  (cond [(empty? a) empty]
        [(cons? a) 
         (cond [(symbol=? (first a) 'pizza)
                (cons 'pizza (cons 'pizza (rest a)))]
               [else (cons (first a)
                           (cesarify (rest a)))])]))
(check-expect (cesarify ex4) (cons 'wurst 
                                   (cons 'huevos 
                                                (cons 'pizza (cons 'pizza (cons 'pants empty))))))
(check-expect (cesarify ex5) empty)

;;Problem 4:
;;(a).

;;A LOS is one of:
;;-empty
;;-(cons shape LOS)

(define-struct circl (x y r outline c))
(define-struct rect (x y width height outline c))
(define-struct squar (x y size outline c))

;;A shape is one of:
;;-Circle
;;-Square
;;-Rectangle

;; A Circle is a
;; (make-circl Number Number Number Boolean Symbol)
;; interpretation: x and y determine the center of the circle,
;; r the radius, outline whether it's outlined or solid,
;; and c its color

;; A Square is a
;;A square is a (make-squar Number Number Number Boolean String)
;;interpretation: x y determine the x and y position of the center of the square. 
;;s is the size of the 
;;square, true or false is whether square is solid or outline, c is the color of the square.

;; A Rectangle is a
;; (make-rect Number Number Number Number Boolean Symbol)
;; interpretation: (make-rect x y w h o c)
;; x determine the center of the square, w is the width of the triangle
;; h is the height of the square.
;; o is whether the triangle is outline or solid, c is the color of the triangle.
 
;;(b).
;;temp: list-of-shapes->Any
#; (define (temp a-los)
     (cond[(empty? a-los)...]
          [(cons? a-los)...(first a-los)...
                        ...(temp (rest a-los))...]))

;;(c).
 
;;shape-list-length: LOS->Number
;;counts how many Shapes
;;are on a given List of Shapes.
(define ee1 empty)
(define ee2 (cons 'square (cons 'circle empty)))

(define (shape-list-length a)
  (cond[(empty? a) 0]
       [(cons? a)
        (+ 1 (shape-list-length (rest a)))]))

(check-expect (shape-list-length ee1) 0)
(check-expect (shape-list-length ee2) 2)

;;(d).
;;yellow-shapes: LOS->LOS
;;changes the color of all of the
;;shapes in a List of Shapes to yellow.

(define y1 (cons (make-circl 50 50 20 true 'red) empty))
(define y2 empty)
(define y3 (cons (make-rect 100 100 30 40 true 'blue) y1))
(define y4 (cons (make-squar 100 100 50 true 'blue) y3))



(define (yellow-shapes a)
  (cond [(empty? a) empty]
        [(cons? a) 
       (cond [(circl? (first a)) (cons (make-circl (circl-x (first a)) 
                                             (circl-y (first a))
                                             (circl-r (first a))
                                             (circl-outline (first a))
                                             'yellow) (yellow-shapes (rest a)))]
             
             [(rect? (first a)) (cons (make-rect (rect-x (first a)) 
                                             (rect-y (first a))
                                             (rect-width (first a))
                                             (rect-height (first a))
                                             (rect-outline (first a))                                          
                                             'yellow) (yellow-shapes (rest a)))]
              
             [(squar? (first a)) (cons (make-squar (squar-x (first a))
                                             (squar-y (first a))
                                             (squar-size (first a))
                                             (squar-outline (first a))
                                             'yellow) (yellow-shapes (rest a)))])]))
             
(check-expect (yellow-shapes y1) (cons 
                                  (make-circl 50 50 20 true 'yellow) empty))
(check-expect (yellow-shapes y2) empty)
(check-expect (yellow-shapes y3) (cons 
                                  (make-rect 100 100 30 40 true 'yellow)
                                  (cons 
                                   (make-circl 50 50 20 true 'yellow) empty)))
(check-expect (yellow-shapes y4) (cons 
                                  (make-squar 100 100 50 true 'yellow)
                                                   (cons 
                                                    (make-rect 100 100 30 40 true 'yellow)
                                                         (cons 
                                                          (make-circl 50 50 20 true 'yellow) empty))))
 
;;(e).
;;return: Boolean->String
;;it takes a boolean to a string, is the "ture" , return "outline", if is "false", return "solid"
 (define (return a)
  (cond[ (boolean=? a true)
         "outline"]
       [(boolean=? a false)
        "solid"]))
 (check-expect (return true) "outline")
 (check-expect (return false) "solid")
 
;;draw-shape: LOS->Image
;;consumes a List of Shapes and
;;adds them to an empty scene of 500 x 500.
(define scene (empty-scene 500 500))
(define (draw-shapes los)
  (cond [(empty? los) scene]
        [(cons? los) (cond [(circl? (first los))
                            (place-image (circle (circl-r (first los))
                                          (return (circl-outline (first los))) 
                                          (circl-c (first los))) 
                                         (circl-x (first los)) (circl-y (first los))
                                         (draw-shapes (rest los)))]
                           [(squar? (first los)) 
                            (place-image (square (squar-size (first los))
                                          (return (squar-outline (first los)))
                                          (squar-c (first los)))
                                         (squar-x (first los)) (squar-y (first los))
                                         (draw-shapes (rest los)))]
                           [(rect? (first los))
                            (place-image (rectangle (rect-width (first los))
                              (rect-height (first los))
                              (return (rect-outline (first los)))
                              (rect-c (first los)))
                              (rect-x (first los)) (rect-y (first los))
                              (draw-shapes (rest los)))])]))
(check-expect (draw-shapes y1)
              (place-image (circle 20 "outline" 'red) 50 50
                           (draw-shapes empty)))
(check-expect (draw-shapes y2) scene)
(check-expect (draw-shapes y3)
              (place-image (rectangle 30 40 "outline" 'blue) 100 100
                           (draw-shapes (cons (make-circl 50 50 20 true 'red) empty))))
(check-expect (draw-shapes y4)
              (place-image (square 50 "outline" 'blue) 100 100
                           (draw-shapes  (cons (make-rect 100 100 30 40 true 'blue)
                                               (cons (make-circl 50 50 20 true 'red) empty)))))
;; f)
;; shape-member? : LoS s -> Boolean
;; Determines if the shape s is in list LoS (list of shapes)
(define (shape-member? losh s)
  (cond [(empty? losh) false]
        [(equal? (first losh) s) true]
        [else (shape-member? (rest losh) s)]))

(define s1 (make-circl 100 100 10 true 'red))
(define s2 (make-rect 80 50 40 50 true 'green))
(define s3 (make-squar 8 8 20 false 'red))
(define losh1 empty)
(define losh2 (cons s1 empty))
(define losh3 (cons s1 (cons s2 empty)))
(define losh4 (cons s2 (cons s3 empty)))
(check-expect (shape-member? losh1 s1) false)
(check-expect (shape-member? losh2 s1) true)
(check-expect (shape-member? losh3 s3) false)
(check-expect (shape-member? losh4 s1) false)



;;5.
(define EM (empty-scene 400 400))

;; txt is (make-txt String Number Number)
(define-struct txt (content x y))

(define txt1 (make-txt "On your mark." 200 100))
(define txt2 (make-txt "Get set." 200 200))
(define txt3 (make-txt "Go!" 200 300))

;; LoTxt is one of: 
;;  - empty
;;  - (cons Txt LoTxt)

;; world is (make-world Image LoTxt)
(define-struct world (image hidden))

;; list of hidden txt, namely, initial list
(define lotxt (cons txt1 (cons txt2 (cons txt3 empty))))

;; define the initial world
(define world0
  (make-world EM lotxt))

;; Interpretation: 
;; The world's image represents the image that the
;; audience can see.
;; The world's list of txt represents the 
;; yet-to-be-revealed elements.

;; display: world -> scene
;; to display the current world
(define (display ws)
  (cond
    [(empty? (world-hidden ws))
     (world-image ws)]
    [else
     (place-image (text (txt-content (first (world-hidden ws))) 30 "blue")
                  (txt-x (first (world-hidden ws)))
                  (txt-y (first (world-hidden ws)))
                  (world-image ws))]))
(define ws1 (make-world EM empty))

(check-expect (display ws1) EM)
(check-expect (display world0) 
              (place-image (text (txt-content (first (world-hidden world0))) 30 "blue")
  (txt-x (first (world-hidden world0)))
  (txt-y (first (world-hidden world0)))
  (world-image world0)))



;; next: world -> world 
;; update the world
(define (next ws)
  (cond
    [(empty? (world-hidden ws)) ws]
    [else 
     (make-world (display ws) (rest (world-hidden ws)))]))
(check-expect (next ws1) ws1)
(check-expect (next world0) (make-world (display world0) (rest (world-hidden world0))))

(big-bang world0
          (on-tick next 1)
          (to-draw display))

