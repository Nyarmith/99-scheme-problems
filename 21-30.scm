;; 21 - Insert an element at a given position into a list
; I never know whether to make these input-robust or if it's more helpful to break if not used correctly
(define (insert-at o l n)
  (if (or (<= n 1) (null? l)) (cons o l)
   (cons (car l) (insert-at o (cdr l) (- n 1)))))

;; 22 - Create a list containing all integers within a given range
(define (range a b)
  (if (> a b) '()
   (cons a (range (+ a 1) b))))

;; 23 - Extract a given number of randomly selected elements from a list

;; helper fn to get kth element and move it to start of list
(define (get-k-partition l k ctr)
  (if (null? l) '()
  (if (< ctr k)
    (let ((fst (car l))
          (rst (get-k-partition (cdr l) k (+ ctr 1))))
    (cons (car rst) (cons fst (cdr rst))))
    l)))

(define (rnd-select l n)
  (if (or (= n 0) (null? l)) '()
   (let ((part (get-k-partition l (random (+ (length l) 1)) 1)))
    (cons (car part) (rnd-select (cdr part) (- n 1))))))

;; 24 - Lotto: Draw N different random numbers from the set 1..M
(define (lotto-select n M)
  (rnd-select (range 1 M) n))

;; 25 - Generate a random permutation of the elements of a list
(define (rnd-permu l)
  (rnd-select l (length l)))

;; 26 - Generate the combinations of K distinct objects chosen from the N elements of a list
(define (combination n l)
  (if (or (null? l) (<= n 0)) '()
  (if (= n 1) (map (lambda (e) (cons e '())) l)
    (let ((fst (car l))
         (combs (combination (- n 1) (cdr l))))
    (append (map (lambda (e) (cons fst e)) combs) (combination n (cdr l)))))))

;; 28 - Group the elements of a set into disjoint subsets
; strategy: iterate through two-combinations, treat them as partitions (so that the cdr of the list is the unchosen elements), then do the same with 3.
(define (cons-one-partition l la)
  (if (null? l) '()
  (let ((fst (car l))
         (snd (cdr l)))
  (cons (cons (cons fst '()) (cons (append la snd) '()))
          (cons-one-partition snd (cons fst la))))))

(define (combination-partition n l lo)
  (if (or (null? l) (<= n 0)) '()
  (if (= n 1)
    (let ((cop (cons-one-partition l '())))
     (map (lambda (e) (cons (car e) (cons (append lo (car (cdr e))) '()))) cop))
    (let ((fst (car l))
         (combs (combination-partition (- n 1) (cdr l) lo)))
    (append (map (lambda (e) (cons (cons fst (car e)) (cdr e))) combs) (combination-partition n (cdr l) (cons fst lo))) ))))

(define (group-helper l p)
  (if (or (null? p) (null? l)) '()
  (let ((group-parts (combination-partition (car p) l '())))
  (map (lambda (e)
    (let ((fst (car e))
          (snd (car (cdr e))))
    (if (null? snd) (cons (cons fst '()) '())
    (let ((finpart (group-helper snd (cdr p))))
     (map (lambda (o)  (cons fst (car o))) finpart ))))) group-parts))))

(define (group-unfold l lo)
  (if (null? l) lo
   (group-unfold (cdr l) (append lo (car l) ))))

(define (group l p)
  (group-unfold (group-helper l p) '()))

;; 27 - Group the elements of a set into disjoint subsets
(define (group3 l) (cons-one-partition l '(2 3 4)))

;; 29 - Sorting a list of lists according to length of sublists
; Sorting a list of lists according to length of sublists
(define (split-list-helper l lo n)
  (if (<= n 0) (cons  (reverse lo) (cons l '()))
  (split-list-helper (cdr l) (cons (car l) lo) (- n 1))))

(define (split-list l)
  (let ((hlf (floor (/ (length l) 2))))
  (split-list-helper l '() hlf)))

(define (l< a b)
  (let ((len-a (length a))
        (len-b (length b)))
  (if (< len-a len-b) #t
  (if (> len-a len-b) #f
  (if (= 0 len-a) #f
  (if (= 0 len-b) #t
  (let ((fst-a (car a)) (fst-b (car b)))
  (if (< fst-a fst-b) #t
  (if (> fst-a fst-b) #f
  (l< (cdr a) (cdr b)))))))))))

(define (merge a b out)
  (if (null? a) (reverse (append (reverse b) out))
  (if (null? b) (reverse (append (reverse a) out))
  (let ((fst-a (car a))
         (fst-b (car b)))
  (if (l< fst-a fst-b)
    (merge (cdr a) b (cons fst-a out))
    (merge a (cdr b) (cons fst-b out)))))))

(define (lsort l)
  (if (= 1 (length l)) l
  (let ((parts (split-list l)))
  (let ((a (lsort (car parts)))
        (b (lsort (car (cdr parts)))))
    (merge a b '())))))
  

;; 30 - Sort according to time frequencies
; strategy - frequency-encode everything into a set first, sort the set by the instance-number, the go through the set and return the list

(define (item-in-histlist a l li)
  (if (null? l) (cons (cons 1 (cons a '())) li)
  (let ((sublist (car (cdr (car l))))
        (listcount (car (car l))))
  (if (eq? a sublist) (append (cons (cons (+ listcount 1) (cons a '())) li) (cdr l))
    (item-in-histlist a (cdr l) (cons (car l) li))))))

(define (hist-process l lo)
  (if (null? l) lo
  (let ((newlist (item-in-histlist (car l) lo '())))
    (hist-process (cdr l)  newlist))))

(define (lh< a b)
  (let ((len-a (car a))
        (len-b (car b)))
  (if (< len-a len-b) #t #f)))

(define (merge-hist a b out)
  (if (null? a) (reverse (append (reverse b) out))
  (if (null? b) (reverse (append (reverse a) out))
  (let ((fst-a (car a))
         (fst-b (car b)))
  (if (lh< fst-a fst-b)
    (merge-hist (cdr a) b (cons fst-a out))
    (merge-hist a (cdr b) (cons fst-b out)))))))

(define (hist-sort l)
  (if (= 1 (length l)) l
  (let ((parts (split-list l)))
  (let ((a (hist-sort (car parts)))
        (b (hist-sort (car (cdr parts)))))
    (merge a b '())))))

(define (denest-hist l)
  (if (null? l) '()
   (cons (car (cdr (car l))) (denest-hist (cdr l) ))))

(define (l-hist-sort l)
  (denest-hist (hist-sort (hist-process l '()))))
