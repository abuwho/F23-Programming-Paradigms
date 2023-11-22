#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)

(defrel (parento x y)
  (conde
    [(== x 'Alice) (== y 'Bob)]
    [(== x 'Alice) (== y 'Charlie)]))

(defrel (siblingo x y)
  (fresh (p)
         (parento p x)
         (parento p y)))

; (run* (x y) (siblingo x y))

(defrel (direct-traino from to)
  (let [(ft (cons from to))]
    (matche ft
            [(saarbruecken . dudweiler)]
            [(forbach . saarbruecken)]
            [(freyming . forbach)]
            [(stAvold . freyming)]
            [(fahlquemont . stAvold)]
            [(metz . fahlquemont)]
            [(nancy . metz)])))

(defrel (traino from to)
  (conde
   [(direct-traino from to)]
   [(fresh (intermediate)
           (direct-traino from intermediate)
           (traino intermediate to))]))

; (run 1 (q) (traino 'metz 'freyming))
; (run* (q) (traino q 'freyming))
; (run* (q) (traino 'freyming q))
; (run* (x y) (traino x y))

(defrel (train-patho from to path)
  (conde
   [(direct-traino from to)
    (== path (list from to))]
   [(fresh (intermediate i-path)
           (direct-traino from intermediate)
           (train-patho intermediate to i-path)
           (== path (cons from i-path)))]))

; (run 1 (path) (train-patho 'metz 'freyming path))
; (run* (from path) (train-patho from 'freyming path))
; (run* (x y path) (train-patho x y path))
; (run* (x y)
;       (fresh (a b c)
;              (train-patho x y (list a b c))))

(defrel (membero x xs)
  (fresh (y ys)
         (== `(,y . ,ys) xs)
         (conde
          [(== x y)]
          [(membero x ys)])))

; (run* (q) (membero 2 '(1 3 2 4)))
; (run* (q) (membero 2 '(1 3 4)))
; (run* (q)
;       (membero q '(1 a 3 1 (1 2) 4))
;       (numbero q))
; (run 3 (q) (membero 'a q))
; (run 3 (x xs) (membero x xs))

(defrel (not-membero x xs)
  (conde
   [(== '() xs)]
   [(fresh (y ys)
           (== `(,y . ,ys) xs)
           (=/= x y)
           (not-membero x ys))]))

; (run* (q) (not-membero 2 '(1 3 2 4)))
; (run* (q) (not-membero 2 '(1 3 4)))
; (run* (q)
;       (not-membero q '(1 a 3 1 (1 2) 4))
;       (numbero q))
; (run 3 (q) (not-membero 'a q))
; (run 3 (x xs) (not-membero x xs))
      
(defrel (lengtho xs n)
  (matche xs
   [()
    (zeroo n)]
   [(,y . ,ys)
    (fresh (k)
           (<o n (build-num 100))
           (pluso (build-num 1) k n)
           (lengtho ys k))]))

; (build-num 100)
; (run* (n) (lengtho '(a b c) n))
; (run* (xs) (lengtho xs (build-num 3)))

(define (appendo xs ys xys)
  (matche xs
   [()
    (== ys xys)]
   [(,z . ,zs)
    (fresh (zys)
           (== (cons z zys) xys)
           (appendo zs ys zys))]))

; (run 1 (xys) (appendo '(a b c) '(1 2 3) xys))
; (run 1 (ys)
;      (appendo '(a b c) ys '(a b c 1 2 3)))
; (run 1 (xs)
;      (appendo xs '(1 2 3) '(a b c 1 2 3)))
; (run* (xs ys)
;      (appendo xs ys '(a b c 1 2 3)))
; (run* (xs ys)
;       (fresh (n)
;              (<o n (build-num 4))
;              (<o (build-num 1) n)
;              (lengtho xs n)
;              (appendo xs ys '(a b c 1 2 3))))

(defrel (reverso orig rev)
  (matche orig
    [()
     (== rev '())]
    [(,x . ,xs)
     (fresh (revxs)
            (appendo revxs (list x) rev)
            (reverso xs revxs))]))

; (run* (rev) (reverso '(a b c) rev))
; (run* (rev) (reverso rev '(c b a)))

(defrel (same-lengtho xs ys)
  (matche xs
   [() (== '() ys)]
   [(,a . ,as)
    (matche ys
     [(,b . ,bs)
      (same-lengtho as bs)])]))

(defrel (reverse-witho orig acc rev)
  (matche orig
   [()
    (== rev acc)]
   [(,x . ,xs)
    (reverse-witho xs (cons x acc) rev)]))

(defrel (better-reverso orig rev)
  (same-lengtho orig rev)
  (reverse-witho orig '() rev))

(run* (rev) (better-reverso '(a b c)  rev))
(run* (rev) (better-reverso rev '(c b a)))

; (run* (n) (lengtho '(a b c) n))
; (run* (xs) (lengtho xs (build-num 3)))

(defrel (binaryo lst)
  (fresh (t)
         (matche lst
                 [(0)]
                 [(1)]
                 [(0 . ,t) (binaryo t)]
                 [(1 . ,t) (binaryo t)])))

; (run 4 (n) (binaryo n))
; '((0) (1) (0 0) (1 0))