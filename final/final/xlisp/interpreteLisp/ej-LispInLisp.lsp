; Problema : Lisp En Lisp.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676


(exec 'B '((A 2) (B 10)) )

;quote
(exec '(quote (car a)) )
(exec '(quote ((2 3) (4 5))) )


;and y or
(exec '(and (or t nil) t) )
(exec '(and (or t nil) (or nil nil)) )
(exec '(or (or t nil) (or nil nil )) )

;if
(exec '(if (or t nil) 1 2))

;cond
(exec '(cond (nil 99) ((and t t) 5) (t 2) ))
(exec '(cond ((and t t) 5) (nil 99)  (t 2) ))

;list
(exec '(list 2 3 4 5))
(exec '(list 1 a (quote (1 2))) '((a 10))); resultado: '(1 10 (1 2))

;list con proceso
(exec '(list (or t t) (and t nil))); resultado:  '(t nil)

;car
(exec '(car (quote (4 2 3))))
(exec '(car (quote ( (2 3) (4 5)))))
(exec '(car (car (quote((2 3 4))))) )

;lambda
(exec '((lambda (x) (* x 2)) 2)) ; resultado: 4 
(exec '((lambda (x y) (+ (* x 2) y)) 2 4)) ; resultado: 8
(exec '(lambda (x) (* x 2))) ; resultado: '(lambda (x) (* x 2))
(exec '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3))) ; resultado:  '((1 4 5) (2 4 5) (3 4 5))


;aritmeticas
(exec '(* (/ 12 4) 10 )) ; resultado: '30
(exec '(+ (* (/ 12 4) 10 ) 1000)) ; resultado: '1030

;funcionales
(exec 
    '(my_fun 1)
    '( (my_fun (lambda (x) (* x 2))) )
) ; resultado: '2

(exec
    '(mapcar 'numberp (quote (1 2 3 4)))
) ; resultado: '(t t t t))

(exec
    '(mapcar 'my_fun (quote (1 2 3 4)))
    '((my_fun (lambda (x) (* x 2))))
    
); resultado: '(2 4 6 8)

(exec
    '(mapcar 'my_fun (quote (a b c d)))
    '((my_fun (lambda (x) (* x 2)))(a 10)(b 20)(c 30)(d 40))
); resultado: '(20 40 60 80)

