(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (map fn lst)
  (if (null? lst)
    nil
    (cons (fn (car lst)) (map fn (cdr lst)))
  )
)

(define (zip pairs)
  (if (null? (car pairs))
    nil
    (cons (map car pairs) (zip (map cdr pairs)))
  )
)

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
    (define (looper n s)
      (if (null? s)
        nil
        (cons (cons n (cons (car s) nil)) (looper (+ 1 n) (cdr s)))
      )
    )
    (looper 0 s)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (looper list1 list2)
    (cond
      ((null? list1) list2)
      ((null? list2) list1)
      (else (cons (car list1) (cons (car list2) (looper (cdr list1) (cdr list2)))))
    )
  )
  (if (eqv? comp <)
    (looper list1 list2)
    (looper list2 list1)
  )
)
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 17
          )
        )
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons (cons 'lambda 
                        (cons (car (zip (let-to-lambda values))) (let-to-lambda body))) 
                 (cons (car (cadr (zip (let-to-lambda values))))
                       (cdr (cadr (zip (let-to-lambda values))))
                 )
           )
           ; END PROBLEM 17
          )
        )
        (else
         ; BEGIN PROBLEM 17
         (map let-to-lambda expr)
         ; END PROBLEM 17
        )
  )
)

