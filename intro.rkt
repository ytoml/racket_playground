; Reference: http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/pl/13-lisp.html
#lang racket

; --------------------- Control Flow -----------------------
; Cond 式
(define (fact n) 
    (cond
        [(<= n 0) 0]
        [(= n 1) 1]
        [else (* n (fact (- n 1)))]
    )
)

; --------------------- Function -----------------------
; Local 変数定義
(define (gcd a b)
    (define s (min a b))
    (define l (max a b))
    (
        if (zero? (remainder l s)) s
        (gcd s (remainder l s))
    )
)

; 内部で利用できる関数を使用した関数定義
(define (cube-sum n) 
    (define (cube n) (* n n n))
    (if (<= n 0) 0 (+ (cube n) (cube-sum(- n 1))))
)

(define (cube-sum2 n)
    (letrec ((cube (lambda (n) (* n n n))))
        (if (<= n 0) 0
            (+ (cube n)(cube-sum2 (- n 1)))
        )
    )
)

; Python 等と同様、 top-level 変数を参照する関数はその変数の値の変化に応じて挙動が変化する。
; 関数定義時に内部で(定義時に)存在しない変数を参照するのもアリらしい
; コンパイルする場合は再宣言不可
(define pi 3.14)
(define (circ_area r)
    (* r r pi)
)
(circ_area 10) ; -> 314.0
(define pi 3.0) ; cannot compile, only valid for REPL
(circ_area 10) ; -> 300.0

; --------------------- List -----------------------
(define l (list 1 2 3 4 5 6 7))
; head
(car l) ; -> 1
; tail
(cdr l) ; -> (2 3 .. 7)
; ちなみに set-car! とか set-cdr! は Racket には無いらしい、 immutability のおかげで list? とか list の長さの取得が O(1) で済むようになるとかなんとか
; https://stackoverflow.com/questions/9475366/set-car-set-cdr-unbound-in-racket

; push_front
(cons 100 l) ; -> (100 1 2 .. 7) 
; empty?
(null? (list)) ; -> #t
(null? (list 1 2 3)) ; -> #f
; List like program
(define sum123 (list '+ 1 2 3))
(eval sum123) ; equivalent to (+ 1 2 3)

; --------------------- Quotation/Quasi-quotation -----------------------
; quotation
(define complex-list '(+ (/ 2 4) a b)) ; same as (list '+ (list '/ 2 4) 'a 'b)
; quasi-(略)
(define fruit 'apple)
(define stmt `(the ,fruit is delicious)) ; stmt -> '(the apple is delicious)
; quasi- を使用した関数定義
(define (square n) `(* ,n ,n)) ; make list (* n n) (つまり (eval (square 100)) -> 10000)