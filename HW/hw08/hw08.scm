(define (ascending? s) (if (or (null? s) (null? (cdr s))) true (and (<= (car s) (car (cdr s))) (ascending? (cdr s)))))
(expect (ascending? (list 1 2 3 3 4)) true)
(expect (ascending? (list 1 2 3 3 2)) false)

(define (my-filter pred s)
    (cond ((null? s) nil)
          ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
          (else (my-filter pred (cdr s)))))
(expect (my-filter zero? (list 1 2 3 0 5 6 0 2)) (0 0))

(define (interleave lst1 lst2) 
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          (else (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2)))))))
(expect (interleave (list 1 2 3) (list 4 5 6 7 8)) (1 4 2 5 3 6 7 8))

(define (no-repeats s)
  (if (null? s) s
    (cons (car s)
      (no-repeats (filter (lambda (x) (not (= (car s) x))) (cdr s)))))
)