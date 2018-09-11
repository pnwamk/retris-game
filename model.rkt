#lang typed/racket #:with-refinements

;; TODO make `equal?` and `eqv?` and `eq?` have
;; linear-integer-propositions when #:with-refinements
;; is on

(require (only-in typed/2htdp/image Image))

(provide (all-defined-out))

(define-type Z4  (Refine [n : Integer] (<= 0 n 3)))
(define-type Col (Refine [n : Integer] (<= 1 n 10)))
(define-type Row (Refine [n : Integer] (<= 1 n 20)))

(define-type Shape (U 'I 'J 'L 'O 'S 'T 'Z))

(struct Block ([shape : Shape]
               [row : Row]
               [col : Col]
               [orientation : Z4])
  #:transparent)

(: row? (-> Integer Boolean : Row))
(define (row? r) (<= 1 r 20))

(: col? (-> Integer Boolean : Col))
(define (col? c) (<= 1 c 10))

(: valid-loc? (-> (Pair Integer Integer) Boolean
                  : (Pairof Row Col)))
(define (valid-loc? p)
  (and (row? (car p)) (col? (cdr p))))

(: block-locs (-> Block (U #f (Listof (Pairof Row Col)))))
(define (block-locs b)
  (match-define (Block shape row col dir) b)
  (define-syntax-rule (coords (r c) ...)
    (let ([l (list (cons (+ row r) (+ col c)) ...)])
      (if (andmap valid-loc? l)
          l
          #f)))
  (case shape
    ;; XXXX
    [(I)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]
    ;; JJJ
    ;;   J
    [(J)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]
    ;; LLL
    ;; L
    [(L)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]
    ;; OO
    ;; OO
    [(O) (coords (0 0) (0 1) (1 0) (1 1))]
    ;;  SS
    ;; SS
    [(S)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]
    ;; TTT
    ;;  T
    [(T)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]
    ;; ZZ
    ;;  ZZ
    [(Z)
     (match dir
       [0 (error "TODO")]
       [1 (error "TODO")]
       [2 (error "TODO")]
       [3 (error "TODO")])]))



(define-type Board (Immutable-HashTable
                    Row (Immutable-HashTable
                         Col Image)))

(struct GameState (;; current block player is controlling
                   [block : Block]
                   ;; board locations currently occupied by blocks
                   [board : Board])
  #:transparent)


(: board-ref (-> GameState Row Col (U #f Image)))
(define (board-ref s r c)
  (define row (hash-ref (GameState-board s) r #f))
  (and row (hash-ref row c #f)))

(: occupied? (case->
              (-> GameState Row Col Boolean)
              (-> GameState (Listof (Pairof Row Col)) Boolean)))
(define occupied?
  (case-lambda
    [(s rcs) (andmap (λ ([p : (Pairof Row Col)]) (occupied? s (car p) (cdr p)))
                     rcs)]
    [(s r c) (and (board-ref s r c) #t)]))

