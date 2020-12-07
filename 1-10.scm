;; 1 - Return the last S-expression of a list
(define (my-last l)
  (if (null? (cdr l))
    (car l)
    (my-last (cdr l))))

; also consider
; (car (reverse l))

;; 2 - Get the last two S-expressions of a list
(define (my-but-last l)
  (if (eq? (cdr (cdr l)) '())
    l
    (my-but-last (cdr l))))

; also consider
; (define (slast l)
; (let ((ll (reverse l)))
;   (cons (car (cdr l)) (cons (car l) '()))))

;; 3 - Find the K-th element of a list
(define (element-at l k)
  (if (<= k 1)
    (car l)
    (element-at (cdr l) (- k 1))))

;; 4 - Find the number of elements of a list
(define (my-size l)
  (if (null? l)
     0
     (+ 1 (my-size (cdr l)))
   ))

;; 5 - Reverse a list
;; This is interesting because it's easy to make slow, also note reverse is usually built-in
(define (my-reverse-helper li lo)
  (if (null? (cdr li)) (cons (car li) lo)
    (my-reverse-helper (cdr li) (cons (car li) lo))))

(define (my-reverse l)
  (if (null? l) '()
    (my-reverse-helper l '())))

;; 6 - Find  out whether a list is a palindrome
(define (is-palindrome l)
  (equal? l (reverse l)))

;; 7 - Flatten a nested list structure
(define (flatten-helper li lo)
  (if (null? li) lo
    (let ((fst (car li)))
      (if (list? fst)
        (flatten-helper fst (flatten-helper (cdr li) lo))
        (cons fst (flatten-helper (cdr li) lo))))))

(define (flatten l) (flatten-helper l '()))

;; 8 - Eliminate consecutive duplicates of list elements
(define (compress l)
  (if (null? l) '())
  (if (null? (cdr l)) l
    (let ((fst (car l))
           (rest (compress (cdr l))))
      (if (eq? fst (car rest))
        rest
        (cons fst rest)))))

;; 9 - Pack consecutive duplicates of list elements into sublists
(define (pack-helper l s o)
  (if (null? l) (cons s o)
    (let ((fst (car l)))
      (if (eq? fst (car s))
        (pack-helper (cdr l) (cons fst s) o)
        (pack-helper (cdr l) (cons fst '()) (cons s o))))))

(define (pack l) (reverse (pack-helper (cdr l) (cons (car l) '()) '())))

;; 10 - Run-length encoding of a list, with previous result
(define (encode-helper c l o)
  (let ((curenc (cons (length c) (cons (car c) '()))))
    (if (null? l) (cons curenc o)
      (encode-helper (car l) (cdr l) (cons curenc o)))))

(define (length-encode l)
  (let ((packed (pack l)))
  (reverse (encode-helper (car packed) (cdr packed) '()))))

; could do problems 9 and 10 better in future, without reverses
