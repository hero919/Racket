;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)




;; A Sprite is a (make-sprite Posn Posn Number String)
;; interp. (make-sprite p1 p2 r c) is a sprite with p1
;; as its location, p2 is its velocity, r is the size
;; of the radius of the sprite and c is its color. Location,
;; velocity and size are in computer-graphic/pixel coordinates.
;; A sprite represents either an attacker's missile or a defender's
;; anti-missile bullet.
(define-struct sprite (loc vel size color))


;; A Los (list of sprites) is one of:
;; - empty
;; - (cons Sprite LOS)

;; A World stucture is a (make-world LOS LOS Number)
;; - interp. (make-world m b h) is a world with m missiles
;; (the missiles attacking the player), b bullets
;; (bullets launched by the player) and h health (current
;; health of the player -- game-over if health <=0)
(define-struct world (missiles bullets health))

(define BG (empty-scene 500 500))
(define c1 empty)

(define m1 (make-sprite (make-posn 20 30)
                        (make-posn 1 1)
                        10 'red))
(define b1 (make-sprite (make-posn 21 30)
                        (make-posn 3 3)
                        5 'green))
(define b2 (make-sprite (make-posn 40 50)
                        (make-posn 3 3)
                        5 'green))
(define b3 (make-sprite (make-posn 700 800) (make-posn 3 3) 5 'green))
(define lom0 (cons m1 empty))
(define lom1 (cons m1 (cons (make-sprite (make-posn 200 300) (make-posn 1 1) 10 'red) empty)))
(define lom2 (cons m1 (cons (make-sprite (make-posn 700 800) (make-posn 1 1) 10 'red) empty)))

(define lob0 (cons b1 empty))
(define lob1 (cons b1 (cons (make-sprite (make-posn 80 90) (make-posn 3 3) 5 'green) empty)))
(define lob2 (cons (make-sprite (make-posn 10 20) (make-posn 3 3) 5 'green)
                    (cons (make-sprite (make-posn -10 12) (make-posn 3 3) 5 'green) 
                        (cons (make-sprite (make-posn 15 20) (make-posn 3 3) 5 'green) empty))))

(define w1 (make-world lom1 lob1 20))
(define w2 (make-world lom1 lob2 20))
(define w3 (make-world lom2 lob1 20))


;; bullet-helper: LOS -> LOS
;; takes in a list of sprites and moves them depening on their location.
(define (bullet-helper alos)
  (cond
    [(empty? alos) empty]
    [(or
      (or
       (> (posn-x (sprite-loc (first alos))) 500)
       (< (posn-x (sprite-loc (first alos))) 0))
      (< (posn-y (sprite-loc (first alos))) 0)) (bullet-helper (rest alos))]
    [else (cons (make-sprite
                 (make-posn (+ (posn-x (sprite-loc (first alos)))
                               (posn-x (sprite-vel (first alos))))
                            (+ (posn-y (sprite-loc (first alos)))
                               (posn-y (sprite-vel (first alos)))))
                 (make-posn (posn-x (sprite-vel (first alos)))
                            (posn-y (sprite-vel (first alos))))
                 (sprite-size (first alos))
                 (sprite-color (first alos)))
                (bullet-helper (rest alos)))]))

(check-expect (bullet-helper c1) empty)
(check-expect (bullet-helper lob2)
              (cons
               (make-sprite
                (make-posn 13 23)
                (make-posn 3 3) 5 'green)
               (cons
                (make-sprite
                 (make-posn 18 23)
                 (make-posn 3 3) 5 'green) empty)))


;; move-bullets: World -> World
;; Move the bullets one time step and remove any that have gone off
;; screen.
(define (move-bullets w)
  (cond
    [(empty? (world-bullets w)) w]
    [else (make-world (world-missiles w)
                      (bullet-helper (world-bullets w))
                      (world-health w))]))
(check-expect (move-bullets (make-world lom1 empty 20))
                            (make-world lom1 empty 20))
(check-expect (move-bullets w2)
             (make-world lom1
               (cons
                (make-sprite
                 (make-posn 13 23)
                 (make-posn 3 3) 5 'green)
                (cons
                 (make-sprite
                  (make-posn 18 23)
                  (make-posn 3 3) 5 'green) empty))
               20))

;; missile-helper: LOS -> LOS
;; takes in a list of sprites and moves them based on the previous location
(define (missile-helper alos)
  (cond
    [(empty? alos) empty]
    [(or
      (or
       (> (posn-x (sprite-loc (first alos))) 500)
       (< (posn-x (sprite-loc (first alos))) 0))
      (> (posn-y (sprite-loc (first alos))) 500)) 
     (bullet-helper (rest alos))]
    [else (cons (make-sprite
                 (make-posn (+ (posn-x (sprite-loc (first alos)))
                               (posn-x (sprite-vel (first alos))))
                            (+ (posn-y (sprite-loc (first alos)))
                               (posn-y (sprite-vel (first alos)))))
                 (make-posn (posn-x (sprite-vel (first alos)))
                            (posn-y (sprite-vel (first alos))))
                 (sprite-size (first alos))
                 (sprite-color (first alos)))
                (missile-helper (rest alos)))]))

(check-expect (missile-helper c1) empty)
(check-expect (missile-helper lob2)
              (cons (make-sprite (make-posn 13 23) (make-posn 3 3) 5 'green)
 (cons (make-sprite (make-posn 18 23) (make-posn 3 3) 5 'green)  empty)))

;; move-missiles: World -> World
;; Move the missiles one time step.
(define (move-missiles w)
  (cond
    [(empty? (world-missiles w)) w]
    [else (make-world (missile-helper (world-missiles w))
                      (world-bullets w)
                      (world-health w))]))
(check-expect (move-missiles (make-world empty lob1 20))
                             (make-world empty lob1 20))
(check-expect (move-missiles w3)
              (make-world (cons (make-sprite (make-posn 21 31)
                        (make-posn 1 1) 
                        10 'red) empty)
                          lob1
                          20))

;; intercept-projectile?: Sprite Sprite -> Boolean
;; Takes in a missile and a bullet and determines whether they will hit one another.
(define (intercept-projectile? m b)
  (> (+ (sprite-size m) (sprite-size b))
     (sqrt 
      (+ 
       (sqr (- (posn-x (sprite-loc m)) (posn-x (sprite-loc b))))
       (sqr (- (posn-y (sprite-loc m)) (posn-y (sprite-loc b))))))))

(check-expect (intercept-projectile? m1 b1) true)
(check-expect (intercept-projectile? m1 b2) false)

;; projectile-contact: Sprite LOS -> LOS
;; Takes in a projectile and a list of projectiles and determines whether or not
;; they are currently touching.
(define (projectile-contact s los)
  (cond
    [(empty? los) empty]
    [(intercept-projectile? s (first los)) (projectile-contact s (rest los))]
    [else (cons (first los) (projectile-contact s (rest los)))]))
(check-expect (projectile-contact b1 empty) empty)
(check-expect (projectile-contact b1 lom1) 
              (cons (make-sprite (make-posn 200 300)
                                 (make-posn 1 1) 10 'red) empty))

;; list-maker: LOS LOS -> LOS
;; Takes in two LOS's and determines if they are empty. If not
;; applies projectile-contact to determine whether they are touching.
(define (list-maker los1 los2)
  (cond 
    [(empty? los1) los2]
    [else (list-maker (rest los1) (projectile-contact (first los1) los2))]))
(check-expect (list-maker empty lom2) lom2)
(check-expect (list-maker lob1 lom1) 
              (cons (make-sprite (make-posn 200 300) (make-posn 1 1) 10 'red) empty))
(check-expect (list-maker lom1 lob1) 
              (cons (make-sprite (make-posn 80 90) (make-posn 3 3) 5 'green) empty))

;; remove-dead-missiles-and-bullets: World -> World
;; Remove every missile that is touching some bullet and vice-versa.
(define (remove-dead-missiles-and-bullets x)
  (make-world (list-maker (world-bullets x) (world-missiles x)) 
              (list-maker (world-missiles x) (world-bullets x))
              (world-health x)))

(check-expect (remove-dead-missiles-and-bullets w1)
              (make-world (cons (make-sprite (make-posn 200 300) (make-posn 1 1) 10 'red) empty)
                          (cons (make-sprite (make-posn 80 90) (make-posn 3 3) 5 'green) empty)
                          20))

;; destroy-missiles: LOS -> LOS
;; Takes in a list of sprites and creates a new list without missiles that have been destroyed. 
(define (destroy-missiles alos)
  (cond
    [(empty? alos) empty]
    [(and (cons? alos)
          (> (posn-y (sprite-loc (first alos))) 500))
     (destroy-missiles (rest alos))]
    [else (cons (make-sprite
                 (make-posn 
                  (posn-x (sprite-loc (first alos)))
                  (posn-y (sprite-loc (first alos))))
                 (make-posn 
                  (posn-x (sprite-vel (first alos)))
                  (posn-y (sprite-vel (first alos))))
                 (sprite-size (first alos))
                 (sprite-color (first alos)))
               (destroy-missiles (rest alos)))]))
(check-expect (destroy-missiles empty) empty)
(check-expect (destroy-missiles lom2)
              (cons m1 empty))


;; calc-health: LOS -> Number
;; Takes in a list of missiles, determiness how many have hit the base
;; and keeps count of all hits.
(define (calc-health alos)
  (cond
    [(empty? alos) 0]
    [(and (cons? alos)
          (> (posn-y (sprite-loc (first alos))) 500))
     (+ 1 (calc-health (rest alos)))]
    [else (calc-health (rest alos))]))
(check-expect (calc-health lom2) 1)
(check-expect (calc-health empty) 0)


;; detonate-missiles: World -> World
;; Remove missiles that landed... and decrement the player's health if
;; any did.
(define (detonate-missiles w)
  (cond
    [(empty? (world-missiles w)) w]
    [else (make-world 
           (destroy-missiles (world-missiles w))
           (world-bullets w)
           (- (world-health w) 
              (calc-health (world-missiles w))))]))

(check-expect (detonate-missiles (make-world empty lob1 20))
                                 (make-world empty lob1 20))
(check-expect (detonate-missiles w3)
              (make-world (cons m1 empty)
                          lob1 19))


;; fire-missile: World Number Number -> Los
;; Takes in the current number of missiles in the world. If greater than or equal to 7,
;; simply returns the current state of the world. If it is less than 7, it adds new missiles
;; to the world until there are at least 7 missiles.
(define (fire-missile w p1 p2)
  (cond
    [(>= (length (world-missiles w)) 7) (world-missiles w)]
    [else (append 
           (cons (make-sprite 
                  (make-posn p1 -5)
                  (make-posn (/ (- p2 p1) 255) 2) 
                  5
                  'red)
                 empty)
           (world-missiles w))]))
(check-expect (fire-missile w2 45 300)
              (cons (make-sprite (make-posn 45 -5) (make-posn 1 2)
                                 5 'red) (world-missiles w2)))
(check-expect (fire-missile 
               (make-world (cons m1 
                                   (cons m1 (cons m1 (cons m1 (cons m1 
                                                                    (cons m1 
                                                                            (cons m1 empty)))))))
                                        lob1
                                        20) 60 70)
         (cons m1 (cons m1(cons m1 (cons m1 (cons m1 (cons m1 (cons m1 empty))))))))
                                                           
                         
                                        


;; maybe-fire-missile: World -> World
;; If we haven't maxed out on missiles, launch another one.
(define (maybe-fire-missile w)
  (make-world (fire-missile w (random 499) (random 499))
              (world-bullets w)
              (world-health w)))
(check-within (posn-x (sprite-loc (first (world-missiles (maybe-fire-missile w1))))) 0 499)
(check-within (posn-x (sprite-loc (first (world-missiles (maybe-fire-missile w1))))) 250 250)
(check-expect (world-bullets (maybe-fire-missile w2))
              (world-bullets w2))
(check-expect (world-health (maybe-fire-missile w2))
              20)



;; shoot-cannon: World Posn Posn MouseEvent -> World
;; Takes in a world state, an x and y coordinate and a mouse event. 
;;Fires the cannon in the given direction at the time.
(define (shoot-cannon w x y me)
  (cond
    [(string=? me "button-down")
     (make-world (world-missiles w)
                 (append (cons 
                          (make-sprite
                           (make-posn 250 490)
                           (make-posn (* .1 (- x 250))
                                      (* .1 (- y 480)))
                           5
                           "green") 
                          empty)
                         (world-bullets w))
                 (world-health w))]
    [else w]))
(check-expect (shoot-cannon w2 200 300 "button-down")
              (make-world (world-missiles w2)
                          (cons (make-sprite (make-posn 250 490)
                                             (make-posn -5 -18)
                                             5 "green") (world-bullets w2))
                          20))

(check-expect (shoot-cannon w2 200 400 "up")
              w2)
;; On-tick handler
;; update-world: World -> World
;; Step the world one tick.
(define (update-world w)
  (move-missiles
   (move-bullets
    (remove-dead-missiles-and-bullets
     (detonate-missiles
      (maybe-fire-missile w))))))
(check-within (posn-x (sprite-loc (first (world-missiles (update-world w2))))) 0 499)
(check-within (posn-x (sprite-loc (first (world-missiles (update-world w2))))) 250 250)


              

;; draw-cannon-and-missiles: LOS -> Image
;; Takes in a list of sprites draws the cannon and missiles.
(define (draw-cannon-and-missiles los)
  (cond
    [(empty? los)
     (place-image (rectangle 10 10 "solid" "green")
                  250
                  495
                  BG)]
    [else (place-image (circle (sprite-size (first los))
                               'solid
                               (sprite-color (first los)))
                       (posn-x (sprite-loc (first los)))
                       (posn-y (sprite-loc (first los))) 
                       (draw-cannon-and-missiles (rest los)))]))
(check-expect (draw-cannon-and-missiles lob0)  (place-image 
                                               (circle 5 'solid 'green) 21 30
                                               (place-image (rectangle 10 10 "solid" "green") 250 495
                                                            BG)))

;; draw-world: World -> Image
;; Checks to see if the player is dead, if true, displays base destroyed. If not,
;; displays current world state
(define (draw-world w) 
  (cond
    [(<= (world-health w) 0) 
     (place-image
      (text "Base Destroyed" 70 "green")
      250 250
      BG)]
    [(empty? (world-bullets w))
     (draw-cannon-and-missiles (world-missiles w))]
    [else (place-image
           (circle 5 'solid 'green)
           (posn-x (sprite-loc (first (world-bullets w))))
           (posn-y (sprite-loc (first (world-bullets w))))
           (draw-world (make-world (world-missiles w)
                                   (rest (world-bullets w))
                                   (world-health w))))]))

(check-expect (draw-world (make-world lom1 lob1 -1))
              (place-image (text "Base Destroyed" 70 "green")
               250 250
               BG))
(check-expect (draw-world (make-world lom0 empty 10))
                          (place-image (circle 10 'solid 'red) 20 30 
                                               (place-image (rectangle 10 10 'solid 'green) 250 495
                                                            BG)))
(check-expect (draw-world (make-world lom0 lob0 10))
              (place-image  (circle 5 'solid 'green)
                            21 30 (place-image (circle 10 'solid 'red) 20 30
                                   (place-image  (rectangle 10 10 'solid 'green) 250 495
                                                            BG))))
                          
              
;; main: Number -> World
;; Starts the game.
(define (main x)
  (big-bang (make-world empty empty 20)
            (to-draw draw-world)
            (on-tick update-world)
            (on-mouse shoot-cannon)))