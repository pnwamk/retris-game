#lang typed/racket #:with-refinements

(require typed/2htdp/image
         "model.rkt")

(define BORDER-SIZE 2)
(define BLOCK-SIZE 22)

(define base-board-image (empty-scene (+ (* 2 BORDER-SIZE)
                                         (* 10 BLOCK-SIZE))
                                      (+ (* 2 BORDER-SIZE)
                                         (* 20 BLOCK-SIZE))))

(: render-board (-> GameState Image))
(define (render-board b)
  (render-current-block
   (GameState-block b)
   (render-blocks (GameState-board b)
                  base-board-image)))

(: render-blocks (-> Board Image Image))
(define (render-blocks b initial-image)
  (let do-row : Image ([row : Row 1]
                       [img : Image initial-image])
    (define row-hash  (hash-ref b row #f))
    (cond
      [(not row-hash)
       (if (< row 20)
           (do-row (add1 row) img)
           img)]
      [else
       (let do-col : Image ([col : Col 1]
                            [img : Image img])
         (define x (hash-ref row-hash col #f))
         (let ([img (if x
                        (render-square img x row col)
                        img)])
           (cond
             [(< col 10)  (do-col (add1 col) img)]
             [(< row 20) (do-row (add1 row) img)]
             [else img])))])))

(: render-square (-> Image Image Row Col Image))
(define (render-square scene img row col)
  (place-image img (col->img-x col) (row->img-y row) scene))

(: row->img-y (-> Row Real))
(define (row->img-y r)
  (+ BORDER-SIZE (* BLOCK-SIZE (- 20 r)) (/ BLOCK-SIZE 2)))

(: col->img-x (-> Col Real))
(define (col->img-x c)
  (- (+ BORDER-SIZE (* BLOCK-SIZE c)) (/ BLOCK-SIZE 2)))

(: render-current-block (-> Block Image Image))
(define (render-current-block b initial-image)
  initial-image)

(define-type BlockColor (U "light blue"
                           "cornflowerblue"
                           "violet"
                           "goldenrod"
                           "darkseagreen"
                           "purple"
                           "pink"))

(: block-color (-> Block BlockColor))
(define (block-color b)
  (case (Block-shape b)
    [(I) "light blue"]
    [(J) "cornflowerblue"]
    [(L) "violet"]
    [(O) "goldenrod"]
    [(S) "darkseagreen"]
    [(T) "purple"]
    [(Z) "pink"]))

(: block (-> BlockColor Image))
(define (block color)
  ;; TODO should typecheck without the assert when #:with-refinements is on!!!
  (overlay (square (assert (- BLOCK-SIZE BORDER-SIZE) positive?) 'solid color)
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

(render-board (GameState (Block 'I 20 1 0) (hash 1 ((inst hash Col Image)
                                                    1 CYAN-BLOCK
                                                    2 BLUE-BLOCK
                                                    3 CYAN-BLOCK
                                                    4 BLUE-BLOCK
                                                    5 CYAN-BLOCK
                                                    6 BLUE-BLOCK
                                                    7 CYAN-BLOCK
                                                    8 BLUE-BLOCK
                                                    9 CYAN-BLOCK
                                                    10 BLUE-BLOCK))))