(load "ex-2-69.lisp")

(define pairs '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define tree (generate-huffman-tree pairs))

(load "ex-2-68.lisp")

(define song
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))

(encode song tree)

(length (encode song tree))  ; 84 bits

;; length of each symbol as a fixed-length string of bits
(define fixed-length-bits-per-symbol
  (ceiling (/ (log (length pairs)) (log 2))))  ; 3 bits

;; length of song in bits, assuming fixed-length
(* fixed-length-bits-per-symbol (length song))  ; 108 bits
