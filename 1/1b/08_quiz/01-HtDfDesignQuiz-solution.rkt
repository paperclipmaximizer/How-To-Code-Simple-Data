;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-HtDfDesignQuiz-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Image Image -> Boolean
;; consumes two images and produces true if the first is larger than the second, else false.
(check-expect (larger? (square 20 "solid" "red")(square 2 "solid" "red"))  #t)
(check-expect (larger? (square 2  "solid" "red")(square 20 "solid" "red")) #f)
(check-expect (larger? (square 20 "solid" "red")(square 20 "solid" "red")) #f)

;stub
;(define (larger? img1 img2) #f)

;(define (larger? img1 img2)      ;template
;  (... t))

(define (larger? img1 img2)     
  (not
   (<=
        (+ (image-height img1)
           (image-width img1))
        (+ (image-height img2)
           (image-width img2)))))
