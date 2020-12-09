;; 11 - Modified run-length encoding, if an element has no duplicates, it's just in the list
(define (encode-modified li)
  (if (null? li)
    '()
    (let ((fst (car li))
          (rst (encode-modified (cdr li))))
      (if (null? rst)
        (cons fst rst)
        (let ((rstfst (car rst)))
          (if (list? rstfst)
            (if (eq? fst (car (cdr rstfst)))
              (cons (cons (+ 1 (car rstfst)) (cons fst '()) ) (cdr rst))
              (cons fst rst))
            (if (eq? fst rstfst)
              (cons (cons 2 (cons fst '()) ) (cdr rst))
              (cons fst rst))))))))

;; 12 - Decode modified run-length encoded list
(define (insert-copies e n l)
  (if (<= n 0) l (cons e (insert-copies e (- n 1) l))))

(define (decode-modified l)
  (if (null? l) '()
    (let ((fst (car l))
           (rst (decode-modified (cdr l))))
      (if (list? fst)
        (insert-copies (car (cdr fst)) (car fst) rst)
        (cons fst rst)))))

;; 13 - Run-length encoding of a list (direct solution)
;; I technicall did this in problem 11, TODO: Implement version that uses p9 and processes that

;; 14 - Duplicate the elemens of a list
(define (dupli l)
  (if (null? l)
    '()
    (let ((fst (car l)))
      (cons fst (cons fst (dupli (cdr l))))) ))

;; 15 - Replicate the elements of a list a given number of times
(define (repli l n)
  (if (null? l) '()
    (let ((fst (car l))
          (rst (repli (cdr l) n)))
      (insert-copies fst n rst))))

;; 16 - Drop every N'thelement from a list
(define (drop-helper l n c)
  (if (null? l) '()
    (if (eq? n c)
      (drop-helper (cdr l) n 1)
      (cons (car l) (drop-helper (cdr l) n (+ c 1))))))

(define (drop l n)
  (drop-helper l n 1))

;; 17 - Split a list into two parts, the length argument is given
; I think it's neat to do this by defining extensions of car and cdr
(define (ncar l n)
  (if (or (null? l) (<= n 1)) '()
   (cons (car l) (ncar (cdr l) (- n 1)))))

(define (ncdr l n)
  (if (or (null? l) (<= n 1)) l
   (ncdr (cdr l) (- n 1))))

(define (split l n)
  (cons (ncar l n) (cons (ncdr l n) '())))

;; 18 - Extract a slice from a list, indices I and K
; let's do this without those primitives
(define (slice l i k)
  (if (null? l) l
  (if (and (<= i 1) (>= k 1)) (cons (car l) (slice (cdr l) (- i 1) (- k 1)))
  (if (< k 1) '()
    (slice (cdr l) (- i 1) (- k 1))))))

;; 19 - Rotate the list N places to the left
; rotating is splitting, so let's do that
(define (rotate l n)
  (if (= n 0) l
  (if (> n 0)
     (let ((splt (split l n)))
       (append (car (cdr splt)) (car splt)))
     (let ((len (length l)))
     (let ((splt (split l (+ len (+ n 1)))))
       (append (car (cdr splt)) (car splt)))))))

;; 20 - Remove the K'th element from a list
; easy, let's just do this direct
(define (remove-at l k)
  (if (<= k 1) (cdr l)
   (cons (car l) (remove-at (cdr l) (- k 1)))))
