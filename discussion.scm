(define (vir-fib n)
    (cond ((zero? n) 0)
          ((= n 1) 1)
          (else (+ (vir-fib (- n 1)) (vir-fib (- n 2)))))
)

(expect (vir-fib 10) 55)
(expect (vir-fib 1) 1)

(define with-list
    (list
        (list 'a 'b) 'c 'd (list nil) 'e
    )
)
(define with-quote
    '(
        (a b) c d () e
    )

)
(define with-cons (cons (cons 'a (cons 'b nil)) (cons 'c (cons 'd (cons nil (cons 'e nil))))))

(define (list-concat a b)
        (if (null? a) b (cons (car a) (list-concat (cdr a) b)))
)

(expect (list-concat '(1 2 3) '(2 3 4)) (1 2 3 2 3 4))
(expect (list-concat '(3) '(2 1 0)) (3 2 1 0))

(define (map-fn fn lst)
    (if (null? lst) nil (cons (fn (car lst)) (map-fn fn (cdr lst))))
)

(map-fn (lambda (x) (* x x)) '(1 2 3))
; expect (1 4 9)

(define (remove item lst)
   (filter (lambda (x) (not (= x item))) lst)
)

(expect (remove 3 nil) ())
(expect (remove 2 '(1 3 2)) (1 3))
(expect (remove 1 '(1 3 2)) (3 2))
(expect (remove 42 '(1 3 2)) (1 3 2))
(expect (remove 3 '(1 3 3 7)) (1 7))

(define (duplicate lst)
    (apply append (map (lambda (x) (cons x (cons x nil))) lst))
)

(expect (duplicate '(1 2 3)) (1 1 2 2 3 3))
(expect (duplicate '(1 1)) (1 1 1 1))

;; Disc 11
;; (+ 1 2 3 4)
;; (+ 1 (* 2 3))
;; (and (< 1 0) (/ 1 0))
;; call scheme_eval on <expr> to get the value, assign the value to a key in a dictionary
;; if the left most value evaluates to false, return the left most values. otherwise evaluate the rest of the expression. If there is one value left, return that value. If there is no value, return true
;; evaluate the first sub-expression using scheme_eval. if it is false, return the value, otherwise recursively evluate the rest of the sub-expressions. 
        
;; eval on (+ 1 2): eval on +, eval on 1, eval on 2: apply + to (1 2) to 3
;; eval on (+ 2 4 6 8):eval on +, eval on 2, eval on 4, eval on 6, eval on 8: apply + to (2 4 6 8) to 20
;; eval on (+ 2 (* 4 (- 6 8))): eval on +, eval on 2, eval on (* 4 (- 6 8)): eval on *, eval on 4, eval on (- 6 8): eval on -, eval on 6, eval on 8: apply - to (6 8) to -2: apply * to (4 -2) to -8: apply + to (2 -8) to -6
;; eval on (and 1 (+ 1 0) 0): eval on and, eval on 1, eval on (+ 1 0): eval on +, eval on 1, eval on 0, apply (+ 1 0) to 1: eval on 0, to 0
;; eval on (and (+ 1 0) (< 1 0) (/ 1 0)): eval on and, eval on (+ 1 0): eval on + 1 0, apply to 1, eval on (< 1 0), to false, to false
        
(define (reverse lst)
    (if (null? lst) nil (append (reverse (cdr lst)) (list (car lst))))
)
(expect (reverse (list 1 2 3 4)) (4 3 2 1))

; helper function
; returns the values of lst that are bigger than x
; e.g., (larger-values 3 '(1 2 3 4 5 1 2 3 4 5)) --> (4 5 4 5)
(define (larger-values x lst)
    (filter (lambda (n) (> n x)) lst))

(define (longest-increasing-subsequence lst)
    (if (null? lst)
        nil
        (begin
            (define first (car lst))
            (define rest (cdr lst))
            (define large-values-rest
                (larger-values first rest))
            (define with-first
                (cons first (longest-increasing-subsequence large-values-rest)))
            (define without-first
                (longest-increasing-subsequence rest))
            (if (> (length with-first) (length without-first)) 
                with-first
                without-first))))

(expect (longest-increasing-subsequence '()) ())
(expect (longest-increasing-subsequence '(1)) (1))
(expect (longest-increasing-subsequence '(1 2 3)) (1 2 3))
(expect (longest-increasing-subsequence '(1 9 2 3)) (1 2 3))
(expect (longest-increasing-subsequence '(1 9 8 7 6 5 4 3 2 3)) (1 2 3))
(expect (longest-increasing-subsequence '(1 9 8 7 2 3 6 5 4 5)) (1 2 3 4 5))
(expect (longest-increasing-subsequence '(1 2 3 4 9 3 4 1 10 5)) (1 2 3 4 9 10))

(define (cons-all first rests)
    (map (lambda (x) (cons first x)) rests)
)
(expect (cons-all 1 '((2 3) (2 4) (3 5))) ((1 2 3) (1 2 4) (1 3 5)))

;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
    (cond ((zero? total) '(()))
          ((< total 0) nil)
          ((null? denoms) nil)
          (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms)))))
)
(expect (list-change 10 '(25 10 5 1)) ((10) (5 5) (5 1 1 1 1 1) (1 1 1 1 1 1 1 1 1 1)))
(expect (list-change 5 '(4 3 2 1)) ((4 1) (3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1)))

;; Disc 12
;; 5
;; `(begin ,x ,y)
;; (list 'begin x y)
;; (cons 'begin (cons x (cons y nil)))
;; f
;; 2
;; x
;; Error / 2000
;; g
;; 4
;; 7 / Error
;; h
;; 7
;; if-else-5
;; 5
;; 2

;; The macro changes the behavior of division
;; expr will be evaluated first before we changed the behavior 
(define-macro (mystery expr)
    `(let ((/ (lambda (a b) (if (= b 0) 1 (/ a b))))) ,expr))

(define (letter-grade earned possible)
    (define grade (mystery (/ earned possible)))
    (cond
        ((>= grade 0.9) 'A)
        ((>= grade 0.8) 'B)
        ((>= grade 0.7) 'C)
        ((>= grade 0.6) 'D)
        (else 'F))
    )

; Tests
(expect (letter-grade 100 0) A)
(expect (letter-grade 95 100) A)
(expect (letter-grade 85 100) B)
(expect (letter-grade 75 100) C)
(expect (letter-grade 65 100) D)
(expect (letter-grade 55 100) F)

(define-macro (max expr1 expr2)
    ; `(if (> ,expr1 ,expr2) ,expr1 ,expr2)
    ; (cons `if (cons (cons `> (cons expr1 (cons expr2 nil))) (cons expr1 (cons expr2 nil))))
    (list `if (list `> expr1 expr2) expr1 expr2)
    )

; Test
(expect (max -3 (+ 1 2)) 3)
(expect (max 1 1) 1)

; (define-macro (multi-assign sym1 sym2 expr1 expr2)
;     `(begin (define ,sym1 ,expr1) (define ,sym2 ,expr2) undefined)
; )

; ; Tests
; (multi-assign x y 1 2)
; (expect (= x 1) #t)
; (expect (= y 2) #t)

(define-macro (multi-assign sym1 sym2 expr1 expr2)
    `(begin (define ,sym2 (list ,expr1 ,expr2))
            (define ,sym1 (car ,sym2))
            (define ,sym2 (car (cdr ,sym2)))
            undefined)
)

; Tests
(multi-assign x y 1 2)
(expect (= x 1) #t)
(expect (= y 2) #t)
(multi-assign x y y x)
(expect (= x 2) #t)
(expect (= y 1) #t)

; (define (replace-helper e o n)
; (if (null? e) nil (if (equal? (car e) o)
;     (cons n (replace-helper (cdr e) o n))
;     (cons (replace-helper (car e) o n) (replace-helper (cdr e) o n)))))
; (define-macro (replace expr old new)
; (replace-helper expr old new))

; ; Tests
; (expect (replace (define x 2) x y) y)
; (expect (= y 2) #t)
; (expect (replace (+ 1 2 (or 2 3)) 2 0) 1)