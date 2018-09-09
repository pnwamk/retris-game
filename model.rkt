#lang typed/racket #:with-refinements

(require (only-in typed/2htdp/image Image))

(provide (all-defined-out))

(define-type BlockColor (U "light blue"
                           "cornflowerblue"
                           "violet"
                           "goldenrod"
                           "darkseagreen"
                           "purple"
                           "pink"))

(define-type Z4  (Refine [n : Natural] (<= n 3)))
(define-type Z10 (Refine [n : Natural] (<= n 9)))
(define-type Z20 (Refine [n : Natural] (<= n 19)))

(struct Loc ([x : Z10] [y : Z20]) #:transparent)

(struct I ([center : Loc] [orientation : Z4]) #:transparent)
(struct J ([center : Loc] [orientation : Z4]) #:transparent)
(struct L ([center : Loc] [orientation : Z4]) #:transparent)
(struct O ([center : Loc] [orientation : Z4]) #:transparent)
(struct S ([center : Loc] [orientation : Z4]) #:transparent)
(struct T ([center : Loc] [orientation : Z4]) #:transparent)
(struct Z ([center : Loc] [orientation : Z4]) #:transparent)

(define-type Block (U I J L O S T Z))

(: block-color (-> Block BlockColor))
(define (block-color b)
  (cond
    [(I? b) "light blue"]
    [(J? b) "cornflowerblue"]
    [(L? b) "violet"]
    [(O? b) "goldenrod"]
    [(S? b) "darkseagreen"]
    [(T? b) "purple"]
    [(Z? b) "pink"]))

(struct GameState (;; current block player is controlling
                   [current : Block]
                   ;; board locations currently occupied by blocks
                   [board : (Immutable-HashTable Loc Image)])
  #:transparent)


(: board-ref-image (case->
                    [-> GameState Loc (U Image #f)]
                    [->  GameState Z10 Z20 (U Image #f)]))
(define board-ref-image
  (case-lambda
    [(s l) (hash-ref (GameState-board s) l #f)]
    [(s x y) (hash-ref (GameState-board s) (Loc x y) #f)]))


(: occupied? (case->
              [-> GameState Loc Boolean]
              [->  GameState Z10 Z20 Boolean]))
(define occupied?
  (case-lambda
    [(s l) (and (hash-ref (GameState-board s) l #f) #t)]
    [(s x y) (and (hash-ref (GameState-board s) (Loc x y) #f) #t)]))


