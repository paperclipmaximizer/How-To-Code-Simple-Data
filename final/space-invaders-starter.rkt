;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

 

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))




(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of space invaders
(define LOI0 empty)
(define LOI2 (list I1 I2))
(define LOI3 (list I1 I2 I3))
#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

(define M0 (make-missile 0 0))
(define M-EDGE (make-missile WIDTH HEIGHT))
(define M-OOB  (make-missile WIDTH (+ HEIGHT 1)))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM2 (list M1 M2))
(define LOM3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom)
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; GameState -> GameState
;; start the world with (main G0)
(define (main g)
  (big-bang g                         ; GameState
            (on-tick   next-game)     ; GameState -> GameState
            (to-draw   render-game)   ; GameState -> Image
            (stop-when gameover?)     ; GameState -> Boolean
            (on-key    handle-key)))  ; GameState KeyEvent -> GameState

;; GameState -> GameState
;; produce the next GameState
(check-expect (next-game G0) (make-game empty empty T0))
(check-expect (next-game G1) (make-game empty empty T1))
(check-expect (next-game G2) (make-game (list I1) (list M1) T1))
(check-expect (next-game G3) (make-game (list I1 I2) (list M1 M2) T1))

(define (next-game g)
  (make-game (next-invaders (game-invaders g))
             (next-missiles (game-missiles g))
             (next-tank  (game-tank g))))

;; Tank -> Tank
;; consumes tank produces tank with new x
(check-expect (next-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))   ;center going right
(check-expect (next-tank T1) (make-tank (+ 50 TANK-SPEED) 1))            ;going right
(check-expect (next-tank T2) (make-tank (- 50 TANK-SPEED) -1))           ;left

(define (next-tank g)
  (make-tank (+ (tank-x g)
                (* TANK-SPEED (tank-dir g)))
                (tank-dir g)))

;; ListOfInvaders -> ListOfInvaders
;; produces a list of moved, filters invaders
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders (list I1)) (list
                                    (make-invader (+ (invader-x I1)
                                                     INVADER-X-SPEED)
                                                  (+ (invader-y I1)
                                                     INVADER-Y-SPEED)
                                                  (invader-dx I1))))
(define (next-invaders loi)
  (advance-invaders (filter-invaders loi)))

;; ListOfInvaders -> ListOfInvaders
;; produces list of moved invaders
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders (list I1)) (list
                                    (make-invader (+ (invader-x I1)
                                                     INVADER-X-SPEED)
                                                  (+ (invader-y I1)
                                                     INVADER-Y-SPEED)
                                                  (invader-dx I1))))
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (... (next-invader (first loi)
                            (next-invaders (rest loi))))]))

;; Invader -> Invader
;; Consumes an invader produces a moved invader
;; !!!
(define (next-invader i) i)

;; ListOfInvader -> ListOfInvader
;; Consumes list of invaders and sorts them for collision
;;!!!
(define (filter-invaders loi) loi)

;; ListOfMissile -> ListOfMissile
;;!!!
(define (next-missiles lom) lom) 

;; Missile -> Missile
;;  
;;!!!
(define (next-missile m) m)

;; GameState -> Image
;; render GameWorld 
;; !!!
(define (render-game g) BACKGROUND)

;; GameState -> Boolean
;; stops the gameworld when an invader x position is 0
;; !!!
(define (gameover? g) #f)

;; GameState KeyEvent -> GameState
;; Moves tank left and right and fires with "space"
(check-expect (handle-key G3 "left")(make-game (list I1 I2) (list M1 M2) T2))
(check-expect (handle-key G3 " ")   (make-game (list I1 I2) (list M1 M2 (make-missile 50 4)) T1))
(check-expect (handle-key G3 "up")  G3)
(check-expect (handle-key G3 "right") G3)
;(define (handle-key g ke) " ")
(define (handle-key g ke)
  (cond [(key=? ke " ")     (make-game (game-invaders g)
                                       (append (make-missile (game-tank (tank-x g))(game-missiles g)))
                                       (game-tank g))]
        [(key=? ke "right") (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x g) 1))]
        [(key=? ke "left")  (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x g) -1))]
        [else g]))



;; Game -> ListOfMissile
;; shoots missile from tank
;; !!!
;(define (shoot-tank g) (make-game (game-missile (make-missile 0 0))))