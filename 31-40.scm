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

(define (cdr-n l n c)
  (if (null? l) '()
  (if (= n c) l
   (cdr-n (cdr l) n (+ c 1)))))

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

(define (totient-phi x) (length (gen-primes (- x 1))))

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
