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

;; 27 - Group the elements of a set into disjoint subsets
; strategy: iterate through two-combinations, treat them as partitions (so that the cdr of the list is the unchosen elements), then do the same with 3.

;;(define (group l p)
