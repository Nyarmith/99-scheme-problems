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

