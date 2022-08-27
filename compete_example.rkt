#lang racket

; https://atcoder.jp/contests/jsc2019-qual/tasks/jsc2019_qual_b
; 転倒数求めるだけ

; IO
(define (read_arr n)
  (if (= n 0) '() (let ([elem (read)]) (cons elem (read_arr (- n 1))))))

; util
(define (max-list lis m)
  (if (empty? lis) m (max-list (cdr lis) (max (car lis) m))))

(define MOD (exact-round (+ 7 1e+9)))
(define (mod i)
  (remainder i MOD))

; Fenwick Tree: '(len hash-table)
; index -> value なリストを Hash Table で扱う
; new-empty-hash n で keys 0..n に対する value 0 の hash table ができる
; 特に (car tree) はサイズを、(cadr tree) は Hash Table を返す

(define (new-empty-hash n)
  (if (< n 0) (hash) (hash-set (new-empty-hash (- n 1)) n 0)))

(define (new-fenwick n)
  (list n (new-empty-hash n)))

(define (fw-add tree index value)
  (if (> index (car tree))
      tree
      (let* ([old-table (cadr tree)]
             [old-value (hash-ref old-table index)] ; 0..=N な index なら OK
             [new-table (hash-set old-table index (+ old-value value))]
             [new-tree (list (car tree) new-table)])
        (fw-add new-tree (+ index (bitwise-and index (- index))) value))))

; 1..=index の和: index の一番小さい 1 の桁を引いていく
; 途中に通ったインデックスの値を全て足す
(define (fw-sum tree index)
  (if (= index 0)
      0
      (let ([value (hash-ref (cadr tree) index)])
        (+ value (fw-sum tree (- index (bitwise-and index (- index))))))))

(define (fw-sum-range tree l r)
  ((- (fw-sum tree r) (fw-sum tree (-1 l 1)))))

; 各要素の転倒数を求める
; i は 0-indexed, fw-sum で k in 0..=i-1 で A[i] 以下の A[k] を数えるので、 i - fw-sum 個
(define (count-online-inversion tree lis i sum)
  (if (empty? lis)
      (list sum tree)
      (let ([new-tree (fw-add tree (car lis) 1)] [le (fw-sum tree (car lis))])
        (count-online-inversion new-tree (cdr lis) (+ i 1) (remainder (+ sum (- i le)) MOD)))))

(define (count-offline-inversion tree lis n sum)
  (if (empty? lis)
      sum
      ; offline では自分自身も fw-sum に含まれるので -1 しない
      (count-offline-inversion tree
                               (cdr lis)
                               n
                               (remainder (+ sum (- n (fw-sum tree (car lis)))) MOD))))

; main
(define N (read))
(define K (read))
(define A (read_arr N))
(define tree (new-fenwick (max-list A 0)))

(define res (count-online-inversion tree A 0 0))
(define x (car res))
(define tree2 (cadr res))
(define y (count-offline-inversion tree2 A N 0))

; x * K + y * K(K-1)/2
(define ans (mod (+ (mod (* x K)) (mod (* y (mod (/ (* K (- K 1)) 2)))))))

(println ans)