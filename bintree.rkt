; Reference: http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/pl/13-lisp.html
#lang racket

; 枝: 'Br, 葉: 'Lf
(define (newbranch v l r)
  (list 'Br v l r)) ; val, left, right
(define (newleaf)
  (list 'Lf)) ; 葉といっても、値は持たないので持たないどちらかと言うと Nil

; 種類チェック
(define (leaf? node)
  (eq? (car node) 'Lf))
(define (branch? node)
  (eq? (car node) 'Br))

; メンバアクセス
(define (get-value node)
  (cadr node))
(define (get-left node)
  (caddr node))
(define (get-right node)
  (cadddr node))

(define invalid-type-msg "Invalid type, please pass \'Br or \'Lf")

(define (find root value)
  (cond
    [(leaf? root) #f]
    [(branch? root)
     (let ([v (get-value root)] [l (get-left root)] [r (get-right root)])
       (cond
         [(= value v) #t]
         [(< value v) (find l value)]
         [else (find r value)] ; v > value
         ))]
    [else invalid-type-msg]))

; 値が既に入っている場合は挿入なし
; return は (tree のルート, 挿入されたか否か) のペア
(define (insert root value)
  (car (insert? root value)))
(define (insert? root value)
  ; root is needed to specify which tree to insert.
  (cond
    [(leaf? root) (list (newbranch value (newleaf) (newleaf)) #t)]
    [(branch? root)
     (let ([v (get-value root)] [l (get-left root)] [r (get-right root)])
       (cond
         [(= value v) (list root #f)]
         [(< value v)
          (let ([l-res (insert? l value)]) (list (newbranch v (car l-res) r) (cadr l-res)))]
         [else
          (let ([r-res (insert? r value)])
            (list (newbranch v l (car r-res)) (cadr r-res)))] ; v < value
         ))]
    [else
     error
     invalid-type-msg]))
(define (newtree value)
  (insert (newleaf) value))

(define (get-min root)
  (cond
    [(branch? root) ((let ([v (get-value root)] [l (get-left root)]) (if (leaf? l) v (get-min l))))]
    [(leaf? root) (error "get-min does not support call on Leaf")]
    [else (error "Invalid type, please pass \'Br")]))

; insert 同様に
; return は (tree のルート, 削除されたか否か) のペア
(define (delete root value)
  (car (delete? root value)))
(define (delete? root value)
  (cond
    [(leaf? root) (list root #f)]
    [(branch? root)
     (let ([v (get-value root)] [l (get-left root)] [r (get-right root)])
       (cond
         [(= value v)
          (cond
            [(and (leaf? l) (leaf? r)) (list (newleaf) #t)]
            [(leaf? l) (list r #t)]
            [(leaf? r) (list l #t)]
            [else ; l is branch && r is branch
             (let* ([m (get-min l)] [del-res (delete? l m)]) ; min value
               (list (newbranch m (car del-res) r) cadr del-res))])]
         [(< value v)
          (let ([l-res (delete? l value)]) (list (newbranch v (car l-res) r) (cadr l-res)))]
         [else ; v < value
          (let ([r-res (delete? r value)]) (list (newbranch v l (car r-res)) (cadr r-res)))]))]
    [else (error invalid-type-msg)]))

(define root0 (newtree 100))
(define root1 (insert root0 110))
(define root2 (insert root1 90))
(define root3 (insert root2 105))
(define root4 (insert root3 107))
(define root5 (insert root4 98))
