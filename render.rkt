#lang typed/racket ;#:with-refinements

(require typed/2htdp/image)


(define BLOCK-SIZE 22)

(: block (-> BlockColor Image))
(define (block color)
  ;; TODO should typecheck without the assert when #:with-refinements is on!!!
  (overlay (square (assert (- BLOCK-SIZE 2) positive?) 'solid color)
           (square BLOCK-SIZE 'solid "white")))

(define CYAN-BLOCK (block "light blue"))
(define BLUE-BLOCK (block "cornflowerblue"))
(define VIOLET-BLOCK (block "violet"))
(define GOLD-BLOCK (block "goldenrod"))
(define GREEN-BLOCK (block "darkseagreen"))
(define PURPLE-BLOCK (block "purple"))
(define PINK-BLOCK (block "pink"))

;; Board : 10 wide, 20 high


;; Tetrominos:
;; I :  XXXX  (cyan)
;;
;; J : JJJ    (blue)
;;       J
;;
;; L : LLL    (orange)
;;     L
;;
;; O : OO     (yellow)
;;     OO
;;
;; S :  SS    (green)
;;     SS
;;
;; T : TTT    (purple)
;;      T
;;
;; Z : ZZ     (red)
;;      ZZ