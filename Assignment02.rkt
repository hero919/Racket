;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;problem 1

(define (flu-activity level)
  (cond [(and (>= level 1) (<= level 3))"minimal"]
        [(<= level 5) "low"]
        [(<= level 7) "moderate"]
        [(<= level 10) "high"]))

(check-expect (flu-activity 2) "minimal")
(check-expect (flu-activity 4) "low")
(check-expect (flu-activity 7) "moderate")
(check-expect (flu-activity 8) "high")

 

;;problem 2

(define (A x) 
   (cond [(= (- (* 8 x) 2) 14) "Yes"]
        [else "No"]))



(define (B x)

  (cond[(= (- (* 10 x) 6) (+ (* 7 x) 9)) "Yes"]
       [else "No"]))




(define (C x) 
   (cond [(= (- 2 (/ -12 x)) -4) "Yes"]
        [else "No"]))

(check-expect (A 2) "Yes")
(check-expect (A 3) "No")
(check-expect (B 5) "Yes")
(check-expect (B 10) "No")
(check-expect (C -2) "Yes")
(check-expect (C 7) "No")

;;problem 3

(define (input1 x) 
  (cond [(and (>= x 4) (< x 16)) "true"]
        [else "false"]))
(check-expect (input1 16) "false")
(check-expect (input1 4) "true")



(define (input2 x)
  (cond [(or (and (> x 20) (< x 30)) (and (> x 40) (< x 50))) "true"]
        [else "false"]))
(check-expect (input2 21) "true")
(check-expect (input2 48) "true")
(check-expect (input2 22) "true")
(check-expect (input2 1) "false")


(define (input3 x)
  (cond [(not (and (>= x 40) (<= x 60))) "true"]
        [else "false"]))

(check-expect (input3 45) "false")
(check-expect (input3 120) "true")






;;prbblem 4

(define (happygauge k)
  (cond [(<= 0 k 100) (- k 0.1)]
        [else k]))
(define (happychange k key)
  (cond [(key=? key "up") (+ k (/ 1 3))]
        [(key=? key "down") (- k (/ 1 5))]
        [else k]))
(define (happygauge->de k)
  (place-image (rectangle (* 4 k) (* 6 k) "solid" "red") 200 300 (empty-scene 400 600)))

(big-bang 100
          (on-tick happygauge)
          (to-draw happygauge->de)
          (on-key happychange))

               

;;problem 5
;;a
(define (graws->image t)
  (place-image (text "Hello World" t "red") 250 150 (empty-scene 500 300)))
(define (graws->graws t)
  (cond [(<= 1 t 80) (+ t 1)]
        [(> t 80) (+ t 0)]))


(big-bang 1
          (to-draw graws->image)
          (on-tick graws->graws))



;;b

(define (graws->ima a)
  (place-image (text "Hello World" a "red") 250 150 (empty-scene 500 300)))
(define (graws->gr a)
  (cond [(<= 1 a 80) (+ a 1)]
        [(> a 80) (+ a 0)]))

(define (graws->click a x y g)
  (cond [(string=? g "button-down") (graws->gr 1)]
        [else a]))
(big-bang 1
          (to-draw graws->ima)
          (on-tick graws->gr)
          (on-mouse graws->click))

           



     


        



  