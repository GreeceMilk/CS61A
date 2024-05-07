(define (over-or-under num1 num2) 
  (cond ((< num1 num2) -1)
        ((= num1 num2) 0)
        (else 1)))

(define (make-adder num)
  (lambda (inc) (+ num inc)))

(define (composed f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1) (lambda (x) (f x))
    (composed f (repeat f (- n 1)))))

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) (let ((s (min a b)) (m (max a b))) 
                      (if (= (modulo m s) 0)
                          s
                          (gcd s (modulo m s)))))