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

