;; 31 - Determine whether a given integer number is prime

(define (is-prime-helper-basic x n)
  (if (= n 1) #t
  (if (= (modulo x n) 0) #f
  (is-prime-helper x (- n 1)))))

(define (is-prime x)
  (is-prime-helper-basic x (floor (sqrt x))))

;; 32 - Determine the reatest common divisor of two positive integer numbers
(define (my-gcd a b)
  (if (= b 0) a
  (my-gcd b (modulo a b))))

;; 33 - Determine whether two positive integer numbers are coprime
(define (coprime a b) (= (my-gcd a b) 1))

;; 34 - Calculate Euler's totient function phi(m)

(define (range a b)
  (if (= a b) (cons b '())
   (cons a (range (+ a 1) b))))

(define (eliminate-every l n c)
  (if (null? l) '()
  (if (= n c) (eliminate-every (cdr l) n 1)
   (cons (car l) (eliminate-every (cdr l) n (+ c 1))))))

(define (sieve l)
 (if (null? l) '()
 (let ((fst (car l)))
   (cons fst (sieve (eliminate-every (cdr l) fst 1) )))))

(define (gen-primes n)
  (sieve (range 2 n)))

; incorrect defn:
;(define (totient-phi x) (length (gen-primes (- x 1))))
; actual correct defn
(define (num-coprime x l)
  (if (null? l) 0
  (let ((cpn (num-coprime x (cdr l))))
  (if (coprime (car l) x)
   (+ 1 cpn)
   cpn
   ))))
(define (totient-phi x) (+ 1 (num-coprime x (range 2 (- x 1)))))

;; 35 - Determine the prime factors of a given positive integer
(define (test-factors x l)
 (if (= x 1) '()
 (let ((fst (car l)))
 (if (= (modulo x fst) 0)
   (cons fst (test-factors (/ x fst) l))
   (test-factors x (cdr l))))))

(define (prime-factors x)
  (test-factors x (gen-primes x)))

;; 36 - Determine the prime factors of a given positive integer (2)
; this time the output should be like ((3 2) (5 1) (7 1))
(define (rh-inc l n)
  (if (null? l) (cons (cons n (cons 1 '())) '())
  (let ((e (car (car l)))
         (m (car (cdr (car l)))))
  (if (= n e) (cons (cons n (cons (+ m 1) '())) (cdr l))
    (cons (car l) (rh-inc (cdr l) n))))))

(define (rh-compress l lo)
  (if (null? l) lo
    (rh-compress (cdr l) (rh-inc lo (car l)))))

(define (prime-factors-mult x)
  (rh-compress (prime-factors x) '()))

;; 37 - Calculate Euler's Totient Function phi(m) more efficiently
; oh, I see I misunderstood what the totient function is in problem 34


;; 38 - Compare the two methods of calc

;; 39 - A ranged list of prime numbers
(define (primes-in-range-helper b l)
 (if (null? l) l
 (if (< b (car l)) (primes-in-range-helper b (cdr l)) l)))


(define (primes-in-range a b) (primes-in-range-helper a (gen-primes b)))

;; 40 - Goldbach's conjecture

;somehow do a double loop here
(define (goldback-helper l n)
)

(define (goldbach x)
  (let ((x-primes (prime-factors x)))
  (goldbach-helper x-primes x)))
   
