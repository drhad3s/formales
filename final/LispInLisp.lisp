; Problema : Lisp En Lisp.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676


;------------- Funciones para manejo del ambiente ------------
;obtiene el valor del elemento en el ambiente, o nil si no está.
(defun get_From_Env (elem env)
    (if (or (null env) (null elem))
        nil
        (if (eq (caar env) elem)
            (cadar env)
            (get_From_Env elem (cdr env))
        )
    )
)

;obtiene t si el elemento está en el ambiente, o nil en caso contrario.
(defun is_In_Env (elem env)
    (if (or (null env) (null elem))
        nil
        (if (eq (caar env) elem)
            T
            (is_In_Env elem (cdr env))
        )
    )
)

;agrega o reemplaza el valor de un elemento en el ambiente
(defun replace_or_add (env param new_value)
    (if (null env)
        (list (list param new_value))
        (if (eq (caar env) param)
            (cons (list param new_value) (cdr env))
            (cons (car env) (replace_or_add (cdr env) param new_value))
        )
    )
)

;expande el ambiente
(defun expand_env (env params vals)
    (if (null params)
        env
        (expand_env (replace_or_add env (car params) (car vals)) (cdr params) (cdr vals))
    )
)

;------------- Fin Funciones para manejo del ambiente ------------

;------------- Funciones auxiliares para exec ------------

;evalua una expresion quote
;(quote expresion)
(defun exec_quote (code)
    (cadr code)
)

;evalua una expresion que es un atomo
;atomo
(defun exec_atom (the_atom env)
    (if (is_In_Env the_atom env)
        (get_From_Env the_atom env)
        the_atom
    )
)

;evalua una expresion or
;(or expr1 expr2)
(defun exec_or (code env)
    (if (exec (cadr code) env)
        t
        (exec (caddr code) env)
    )
)

;evalua una expresion and
;(and expr1 expr2)
(defun exec_and (code env)
    (if (exec (cadr code) env)
        (exec (caddr code) env)
        nil
    )
)

;evalua una expresion if
;(if expresion true-code false-code)
(defun exec_if (code env)
    (if (exec (cadr code) env)
        (exec (caddr code) env)
        (exec (cadddr code) env)
    )
)

;evalua una lista de expresion del cond con el sig formato
;((expr code) (expr code) )
(defun exec_cond_list (code env)
    (if (exec (caar code) env)
        (exec (cadar code) env)
        (exec_cond_list (cdr code) env)
    )
)

;evalula una expresion cond
;(cond (expr code) (expr code))
(defun exec_cond (code env)
    (exec_cond_list (cdr code) env)
)

;aplicacion de lambda
;(caddar code): codigo a ejecutar
;(cadar code): parametros de la fcn lambda
;(cdr code): valores que toman los parametros de la funcion lambda
;env: el ambiente actual
(defun apply_lambda (code env)
    (exec (caddar code) (expand_env env (cadar code) (cdr code)))
)

(defun esFuncion (code f)
    (equal (car code) f)
)

(defun apply_Code (code)
    (apply (car code) (cdr code))
)

;implementacion del mapcar
(defun exec_Mapcar (f l env)
    (if (null l)
        nil
        (cons (exec (list f (list 'quote (car l))) env) (exec_Mapcar  f (cdr l) env))
    )
)

;evalue la lista de argumentos de una funcion
;(param1 param2 ... paramn)
(defun eval_args_list (code env)
    (if (null code)
        nil
        (cons (exec (car code) env) (eval_args_list (cdr code) env))
    )
)

;evalue la lista de argumentos de una funcion
;(fun param1 param2 ... paramn)
(defun eval_args (code env)
    (cons (car code) (eval_args_list (cdr code) env))
)

(defun exec_fun (code env)
    (if (atom (car code))
        (cond
            ((esFuncion code 'nth) (nth (cadr code) (caddr code)))
            ((esFuncion code 'cons) (cons (cadr code) (caddr code)))
            ((esFuncion code 'append) (append (cadr code) (caddr code)))
            ((esFuncion code 'apply) (apply (cadr code) (caddr code)))
            ((esFuncion code 'mapcar) (exec_Mapcar (cadr code) (caddr code) env))
            ;buscar en el ambiente por si hay una funcion con el nombre 'car code'
            ((is_In_Env (car code) env) (exec (cons (get_From_Env (car code) env) (cdr code)) env))
            (t (apply_Code code)); caso contrario ejecuta con la función que venga de nombre, y los parametros siguiente.
        )
        (cond
            ;procesamos lambda
            ((esFuncion (car code) 'lambda) (apply_lambda code env))
            ;que queda?
            (t nil)
        )
    )
)

;evalua una expresion lisp
(defun exec (code &optional (env nil))
    (if (null code) nil
        (cond
            ((atom code) (exec_atom code env))
            ((esFuncion code 'quote) (exec_quote code))
            ((esFuncion code 'or) (exec_or code env))
            ((esFuncion code 'and) (exec_and code env))
            ((esFuncion code 'if) (exec_if code env))
            ((esFuncion code 'cond) (exec_cond code env))
            ((esFuncion code 'lambda) code )
            (t (exec_fun (eval_args code env) env))
        )
    )
)

'(--------- Tests ----------)
;testing function(cons (exec (list f (list 'quote (car l))) env) (exec_Mapcar  f (cdr l) env))
;=============================
(defun test (name actual expected)
    (if (equal actual expected)
        (list name 'passed)
        (list name 'fail '=> 'expected expected 'actual actual)
    )
)
;=============================

;numeros
(test 'numero (exec '2) '2)

;letras
(test 'letra (exec 'A) 'A)

;true false
(test 'tf1 (exec nil) nil)
(test 'tf2 (exec 'nil) nil)
(test 'tf3 (exec 't) t)

;variables de ambiente
(test 'amb1 (exec 'A '((A 2)) ) '2)
(test 'amb2 (exec 'B '((A 2) (B 10)) ) '10)

;quote
(test 'quote1 (exec '(quote A) ) 'A)
(test 'quote2 (exec '(quote 1) ) '1)
(test 'quote3 (exec '(quote (car a)) ) '(car a))
(test 'quote4 (exec '(quote ((2 3) (4 5))) ) '((2 3) (4 5)) )

;or
(test 'or1 (exec '(or t t) ) 't)
(test 'or2 (exec '(or t nil) ) 't)
(test 'or3 (exec '(or nil nil) ) 'nil)
(test 'or4 (exec '(or nil t) ) 't)

;and
(test 'and1 (exec '(and nil nil) ) 'nil)
(test 'and2 (exec '(and t nil) ) 'nil)
(test 'and3 (exec '(and nil t) ) 'nil)
(test 'and4 (exec '(and t t) ) 't)

;and y or
(test 'andor1 (exec '(and (or t nil) t) ) 't)
(test 'andor2 (exec '(and (or t nil) (or nil nil)) ) 'nil)
(test 'andor3 (exec '(or (or t nil) (or nil nil )) ) 't)

;if
(test 'if1 (exec '(if t 1 2)) '1)
(test 'if2 (exec '(if t 1 2)) '1)
(test 'if3 (exec '(if (or t nil) 1 2)) '1)

;cond
(test 'cond1 (exec '(cond (t 2) )) '2)
(test 'cond2 (exec '(cond (nil 5) (t 2) )) '2)
(test 'cond3 (exec '(cond ((and nil nil) 5) (t 2) )) '2)
(test 'cond4 (exec '(cond ((and nil nil) 5) (nil 99) (t 2) )) '2)
(test 'cond5 (exec '(cond (nil 99) ((and t t) 5) (t 2) )) '5)
(test 'cond6 (exec '(cond ((and t t) 5) (nil 99)  (t 2) )) '5)

;list
(test 'list1 (exec '(list 2 3 4)) '(2 3 4))
(test 'list2 (exec '(list 2 3 4 5)) '(2 3 4 5))
(test 'list3 (exec '(list t)) '(t))
(test 'list4 (exec '(list 1 a (quote (1 2))) '((a 10))) '(1 10 (1 2)))

;list con proceso
(test 'listproc1 (exec '(list (or t t))) '(t))
(test 'listproc2 (exec '(list (or t t) (and t nil))) '(t nil))
(test 'listproc3 (exec '(list (quote (2 3 4)))) '((2 3 4)))

;car
(test 'car1 (exec '(car (quote (2 3)))) '2)
(test 'car2 (exec '(car (quote (4 2 3)))) '4)
(test 'car3 (exec '(car (quote ( (2 3) (4 5))))) '(2 3))

;cdr
(test 'cdr1 (exec '(cdr (quote (4 2 3)))) '(2 3))

;caar
(test 'caar1 (exec '(caar (quote ((4 2 3))))) '4)

;cdar
(test 'cdar1 (exec '(cdar (quote ((4 2 3))))) '(2 3))

;car + ambiente
(test 'car-amb1 (exec '(car (list a 2 3)) '((a 100)) ) '100)

;cdr + ambiente
(test 'cdr-amb1 (exec '(cdr (list a b c)) '((a 100) (b 99) (c 98)) ) '(99 98))

;not
(test 'not1 (exec '(not t)) nil)
(test 'not2 (exec '(not nil)) t)
(test 'not3 (exec '(not a) '((a nil))) t)

;lambda
(test 'lambda1 (exec '((lambda (x) (* x 2)) 2)) '4)
(test 'lambda2 (exec '((lambda (x y) (+ (* x 2) y)) 2 4)) '8)
(test 'lambda3 (exec '(lambda (x) (* x 2))) '(lambda (x) (* x 2)))
(test 'lambda4 (exec '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3))) '((1 4 5) (2 4 5) (3 4 5)))

;expandir ambiente
(test 'exp_amb1 (replace_or_add nil 'a '1) '((a 1)))
(test 'exp_amb2 (replace_or_add '((a 2)) 'a '1) '((a 1)))
(test 'exp_amb3 (replace_or_add '((b 10) (a 2)) 'a '1) '((b 10) (a 1)))
(test 'exp_amb4 (expand_env nil '(x y) '(10 20)) '((x 10) (y 20)))
(test 'exp_amb5 (expand_env '((x 50)) '(x y) '(10 20)) '((x 10) (y 20)))

;recursion
(test 'rec1 (exec '(car (car (quote((2 3 4))))) ) '2)

;aritmeticas
(test 'aritm1 (exec '(+ 2 3) ) '5)
(test 'aritm2 (exec '(+ 2 3 4) ) '9)
(test 'aritm3 (exec '(- 3 4 5) ) '-6)
(test 'aritm4 (exec '(* 3 4) ) '12)
(test 'aritm5 (exec '(/ 12 4) ) '3)
(test 'aritm6 (exec '(* (/ 12 4) 10 )) '30)
(test 'aritm7 (exec '(+ (* (/ 12 4) 10 ) 1000)) '1030)

;atom
(test 'atom1 (exec '(atom 2)) t)
(test 'atom2 (exec '(atom nil)) t)
(test 'atom3 (exec '(atom (quote (2 3 4)))) nil)

;listp
(test 'listp1 (exec '(listp 2)) nil)
(test 'listp2 (exec '(listp nil)) t)
(test 'listp3 (exec '(listp (quote (2 3)))) t)

;numberp
(test 'numberp1 (exec '(numberp (quote (2 3)))) nil)
(test 'numberp2 (exec '(numberp 2)) t)
(test 'numberp3 (exec '(numberp nil)) nil)

;null
(test 'null1 (exec '(null nil)) t)
(test 'null2 (exec '(null 3)) nil)

;nth
(test 'nth1 (exec '(nth 0 (quote (0 1)))) '0)
(test 'nth2 (exec '(nth 1 (quote (0 1)))) '1)
(test 'nth3 (exec '(nth 4 (quote (0 1 2 3 4)))) '4)

;cons
(test 'cons1 (exec '(cons 4 (quote (0 1 2 3 4)))) '(4 0 1 2 3 4))
(test 'cons2 (exec '(cons 4 (quote (4)))) '(4 4))
(test 'conscdr1 (exec '(cons x (cdr y)) '((x a)(y (b c)))) '(a c))
(test 'conscdr2 (exec '(cons 'a (quote (1 2)))) '(a 1 2))

;append
(test 'append1 (exec '(append (quote (4)) (quote (4)))) '(4 4))
(test 'append2 (exec '(append (quote (4 5 6)) (quote (4 5 6)))) '(4 5 6 4 5 6))

;length
(test 'length1 (exec '(length (quote (4 5 6)))) '3)

;apply
(test 'apply1 (exec '(apply '+ (quote (4 5 6)))) '15)
(test 'apply2 (exec '(apply '* (quote (4 5)))) '20)

;mapcar
(test 'mapcar1 (exec '(mapcar 'numberp (quote (4)))) '(t))
(test 'mapcar2 (exec '(mapcar 'numberp (quote (4 5 6 nil)))) '(t t t nil))
(test 'mapcar3 (exec '(mapcar 'car (quote ( (2 3) (4 5 ))) )) '(2 4))

;reverse
(test 'reverse (exec '(reverse (quote (4 5 6 7)))) '(7 6 5 4))

;===========
;funcionales
;===========

(test 'fun1 (exec
    '(my_fun 1)
    '( (my_fun (lambda (x) (* x 2))) )
    )
    '2
)

(test 'fun2 (exec
    '(mapcar 'numberp (quote (1 2 3 4)))
    )
    '(t t t t)
)

(test 'fun3 (exec
    '(mapcar 'my_fun (quote (1 2 3 4)))
    '((my_fun (lambda (x) (* x 2))))
    )
    '(2 4 6 8)
)

(test 'fun4 (exec
    '(mapcar 'my_fun (quote (a b c d)))
    '((my_fun (lambda (x) (* x 2)))(a 10)(b 20)(c 30)(d 40))
    )
    '(20 40 60 80)
)

