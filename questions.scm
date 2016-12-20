(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
;;MAP 
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items))))
)
;;CONS-ALL

(define (cons-all first rests)
  (if (null? rests)
    nil
    (cons (cons first (car rests)) (cons-all first (cdr rests))))
)

;;ZIP
(define (zip pairs)
    (define (helper pairs)
      (if (null? pairs)
        ()
      (cons (car (car pairs)) (helper (cdr pairs)))))
    (define (helper-two pairs)
      (if (null? pairs)
        ()
      (cons (car (cdr (car pairs))) (helper-two (cdr pairs)))))
  (list (helper pairs) (helper-two pairs))
)

(define (length s)
  (if (null? s)
    0
    (+ 1 (length (cdr s)))
  )
)

(define (range start end)
  (if (<= end start)
    ()
    (cons start (range (+ start 1) end))
  )
)


;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  (define (enum s index)
    (if (null? s)
      ()
      (cons (list index (car s)) (enum (cdr s) (+ 1 index)))))
      ;(list (car range) (car s)))
  (enum s 0)



    ;(cons (cons (car (range 0 (length s))) (car s)) (enumerate (cdr s)))
)
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  
  
  (cond 
    ((< total 0) nil)
    ((null? denoms) nil)
    ((= total 0) '(()) )
    (else
    (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))))
 
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr) 
         ; BEGIN Question 20
        expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20f
           (cons form (cons params (map analyze body)))))
           ; END Question 20
           
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Questi on 20
           (append (list (list 'lambda (car (zip (map analyze values))) (car (map analyze body)))) (cadr (zip (map analyze values))))))
           ; END Question 20
           
        (else
         ; BEGIN Question 20
         (map analyze expr))))
         ; END Question 20
      
;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21

