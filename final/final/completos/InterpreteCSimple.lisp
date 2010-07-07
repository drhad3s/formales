; Problema : Interprete de C.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

;------ Validaciones -------
(defun esVariable (var mem)
    (pertenece_ListaPares var mem)
)

(defun esAsignacion (expr memoria)
    (if (esVariable (car expr) memoria)
        t
        (and (or (equal (car expr) '++) (equal (car expr) '--)) 
            (esVariable (cadr expr) memoria)
        )
    )
)

(defun esFuncion (prg f)
    (equal (caar prg) f)
)

(defun isValidType (type) 
    (cond 
        ((eq type 'int) T)
        ((eq type 'long) T)
        ((eq type 'float) T)
        ((eq type 'double) T)
        (T nil)
    )
)

(defun isVarDef (prg)
    (if (>= (length (car prg)) 2)
        (isValidType (caar prg) )
        nil
    )
)

;----- Fin Validaciones -------

;-------- Operaciones de memoria ---------
(defun buscar (elemento listaPares)
    (if (null listaPares)
        nil
        (if (equal elemento (caar listaPares))
            (cadar listaPares)
            (buscar elemento (cdr listaPares))
        )
    )
)

(defun pertenece_ListaPares (elemento listaPares)
    (if (null listaPares)
        nil
        (if (equal elemento (caar listaPares) )
            t
            (pertenece_ListaPares elemento (cdr listaPares))
        )
    )
)

(defun modificar_Valor_Var_En_Mem (nombreElemento NuevoValor listaPares)
    (if (null listaPares)
        nil
        (if (equal nombreElemento (caar listaPares) )
            (cons (list nombreElemento NuevoValor ) (cdr listaPares) )
            (cons (car listaPares ) (modificar_Valor_Var_En_Mem nombreElemento NuevoValor (cdr listaPares)))
        )
    )
)

(defun addPairKeyValue (key value mem)
    (cons (list key value ) mem )
)

(defun agregar_Var_Con_Asignacion (var valor mem)
    (addPairKeyValue var (evaluar valor mem) mem)
)

(defun agregar_Var_A_Mem (var mem)
    (if (null var) 
        mem
        (if (equal (length var) 1)
            (addPairKeyValue (car var) 0 mem )
            (if (equal (nth 1 var) '= )
                (agregar_Var_Con_Asignacion (car var) (caddr var) (agregar_Var_A_Mem (cdddr var) mem))
                (addPairKeyValue (car var) 0 (agregar_Var_A_Mem (cdr var) mem))
            )
        )
    )
)

;-------- Fin Operaciones de memoria ---------

;----- Evaluar -------

(defun esOperadorLisp (el)
    (cond 
        ((equal el '+) t)
        ((equal el '*) t)
        ((equal el '-) t)
        ((equal el '/) t)
        ((equal el '<) t)
        ((equal el '>) t)
        ((equal el '<=) t)
        ((equal el '>=) t)
        ((equal el 'and) t)
        ((equal el 'or) t)
        ((equal el 'eq) t)
        ( t nil)
    )
)

(defun prioridad_op (op)
    (cond
        ((equal op '+) 2)
        ((equal op '*) 3)
        ((equal op '-) 2)
        ((equal op '/) 3)
        ((equal op '<) 1)
        ((equal op '>) 1)
        ((equal op '<=) 1)
        ((equal op '>=) 1)
        ((equal op 'eq) 1)
        ( t 0)
    )
)

(defun componer_expresion (ops vars)
    (if (null ops) 
        vars
        (componer_expresion (cdr ops) (cons (list (car ops) (cadr vars) (car vars)) (cddr vars) ))
    )
)

(defun inf_a_pref (expr &optional (ops nil) (vars nil))
    (if (null expr) 
        (componer_expresion ops vars)
        (if (esOperadorLisp (car expr))
            (if (> (length ops) 0)
                (if (< (prioridad_op (car expr)) (prioridad_op (car ops)))
                    (inf_a_pref (cdr expr) (cons (car expr)(cdr ops)) 
                                (cons (list (car ops) (cadr vars) (car vars)) (cddr vars) )
                    )
                    (inf_a_pref (cdr expr) (cons (car expr) ops) vars)
                )
                (inf_a_pref (cdr expr) (cons (car expr) ops) vars)
            )
            (if (atom (car expr))
                (inf_a_pref (cdr expr) ops (cons (car expr) vars))
                (inf_a_pref (cdr expr) ops (cons (car (inf_a_pref (car expr))) vars))
            )
        )    
    )
)

(defun filtrar_nil_t (resultado)
    (cond
        ((equal resultado nil) 0)
        ((equal resultado t) 1)
        (t resultado)
    )
)

(defun operar_lista (lista)
    (if (null lista)
        nil
        (if (atom (car lista))
            (eval lista)
            (operar_lista (car lista))
        )
    )
)

(defun operar (lista)
    (if (eq (length lista) 1)
        (if (atom (car lista))
            (car lista); tiene que ser variable o cte.
            (filtrar_nil_t (operar_lista (inf_a_pref lista)))
        )
        (filtrar_nil_t (operar_lista (inf_a_pref lista)))
    )
)

(defun esOperadorEnC (op)
    (cond 
        ((equal op '+) t)
        ((equal op '*) t)
        ((equal op '-) t)
        ((equal op '/) t)
        ((equal op '<) t)
        ((equal op '>) t)
        ((equal op '<=) t)
        ((equal op '>=) t)
        ((equal op '&&) t)
        ((equal op '||) t)
        ((equal op '==) t)
        ( t nil)
    )
)

(defun traducirOp (op)
    (cond
        ((equal op '&&) 'and)
        ((equal op '||) 'or)
        ((equal op '==) 'eq)
        ( t op)
    )
)

(defun getVarValue (varName mem)
    (buscar varName mem)
)

;Arma una expreción algebraica en base de la sentencia, donde figuran variables y otros, que las busca en memoria.
(defun armar_Expresion (sentencia mem)
    (if (null sentencia)
        nil
        (cond
            ;Si es parentesis, hay que evaluar lo de adentro y dejar la misma estructura
            ( (not (atom (car sentencia))) (cons (list (armar_Expresion (car sentencia) mem)) (armar_Expresion (cdr sentencia) mem)) )
            ;Si es variable, hay que buscar en memoria el valor
            ( (esVariable (car sentencia) mem) (cons (getVarValue (car sentencia) mem) (armar_Expresion (cdr sentencia) mem)) )
            ;Si es un operador traducirlo a los de lisp
            ( (esOperadorEnC (car sentencia)) (cons (traducirOp (car sentencia)) (armar_Expresion (cdr sentencia) mem)) )
            ;Caso contrario, es constante( u operacion, que para esta instancia resulta como si fuese constante).
            ( T (cons (car sentencia) (armar_Expresion (cdr sentencia) mem)) )
        )
    )
)

(defun evaluar (sentencia mem)
    ;(filtrar_nil_t (evaluar_lisp prg mem))
    (if (atom sentencia)
        sentencia
        (operar (armar_Expresion sentencia mem))
    )
    
)

;------- Fin Evaluar -------

;------ Funciones auxiliares para ejecutar ------
(defun asignacion (expr memoria)
    (if (esVariable (car expr) memoria) 
        (cond
            ( (equal (nth 1 expr) '=) (modificar_Valor_Var_En_Mem (car expr) (evaluar (cddr expr) memoria) memoria))
            ( (equal (nth 1 expr) '++) (asignacion (list (car expr) '= (car expr) '+ 1 ) memoria))
            ( (equal (nth 1 expr) '--) (asignacion (list (car expr) '= (car expr) '- 1 ) memoria))
            ( t (asignacion (list (car l) '= (car l) (nth 1 expr) (nth 3 expr)) memoria))
        )
        (asignacion (reverse expr) memoria)
    )
)

(defun procesar_if (prg mem entrada salida)
    (if (not (equal (evaluar (cadar prg) mem) 0)) 
        (ejecutar (append (list (nth 2 (car prg))) (cdr prg)) mem entrada salida)
        (if (equal (length (car prg)) 5) 
            (ejecutar (append (list (nth 4 (car prg))) (cdr prg)) mem entrada salida)
            (ejecutar (cdr prg) mem entrada salida)
        )
    )
)

(defun procesar_while (prg mem entrada salida)
    (if (not (equal (evaluar (nth 1 (car prg)) mem) 0))
        (ejecutar (append (cddar prg) prg) mem entrada salida)
        (ejecutar (cdr prg) mem entrada salida)
    )
)

(defun procesar_repeat_aux (prg mem entrada salida)
    (if (equal (evaluar (nth 1 (car prg)) mem) 0)
        (ejecutar (append (cddar prg) prg) mem entrada salida)
        (ejecutar (cdr prg) mem entrada salida)
    )
)

(defun procesar_repeat (prg mem entrada salida)
    (if (eq (nth 2 (car prg)) 'until)
        (ejecutar 
            (append 
                (append (cadar prg) 
                    (list (cons 'repeat_aux (cons (nth 3 (car prg)) (cadar prg)) ))
                )
                (cdr prg)
            )
            mem entrada salida)
        'syntax_error
    )
)

(defun procesar_for (prg mem entrada salida)
    (ejecutar   
        (append
            (list (car (nth 1 (car prg))) 
                (cons 'while 
                    ( cons (cadr (nth 1 (car prg))) 
                        (append (cddar prg) (list (caddr (nth 1 (car prg))) ) )
                    )
                ) 
            )
            (cdr prg)
        )
        mem entrada salida
    )
)


(defun procesar_scanf (prg mem entrada salida)
    (ejecutar (cdr prg) (modificar_Valor_Var_En_Mem (cadar prg) (car entrada) mem) (cdr entrada) salida)
)

(defun procesar_printf (prg mem entrada salida)
    (ejecutar (cdr prg) mem entrada (append salida (list (evaluar (cdar prg) mem))) )
)

(defun procesar_varDef (prg mem entrada salida)
    (if (pertenece_ListaPares(cadar prg) mem)
        'Error
        (ejecutar (cdr prg) (agregar_Var_A_Mem (cdar prg) mem) entrada salida)
    )
)

;------ Fin Funciones auxiliares para ejecutar ------

(defun ejecutar (prg mem &optional (entrada nil) (salida nil))
    (if (null prg) 
        salida
        (cond
            ( (esFuncion prg 'scanf) (procesar_scanf prg mem entrada salida) )
            ( (esFuncion prg 'printf) (procesar_printf prg mem entrada salida) )
            ( (isVarDef prg) (procesar_varDef prg mem entrada salida) )
            ( (esAsignacion (car prg) mem) (ejecutar (cdr prg) (asignacion (car prg) mem) entrada salida) )
            ( (esFuncion prg 'if) (procesar_if prg mem entrada salida) )
            ( (esFuncion prg 'while) (procesar_while prg mem entrada salida) )
            ( (esFuncion prg 'repeat_aux) (procesar_repeat_aux prg mem entrada salida) )
            ( (esFuncion prg 'repeat) (procesar_repeat prg mem entrada salida) )
            ( (esFuncion prg 'for) (procesar_for prg mem entrada salida) ) 
            ( t (list 'syntax_error (car prg)) )    
        )
    )
)

(defun run (prg &optional (entrada nil) (mem nil) )
    (if (null prg)
        nil
        (cond
            ( (isVarDef prg) (run (cdr prg) entrada (agregar_Var_A_Mem (cdar prg) mem)) )
            ( (eq (caar prg) 'main) (ejecutar (cadar prg) mem entrada) )
            ( t 'Error )
        )
    )
)

'(----- Load utils for tests -----)

(defun test (name actual expected)
    (if (equal actual expected)
        (list name 'passed)
        (list name 'fail '=> 'expected expected 'actual actual)
    )
)

'(----- Inicio tests -----)
(test 'Buscar_En_Memoria_Esta_Solo
    (buscar 'a '((a 2)))
    '2
)

(test 'Buscar_En_Memoria_Esta_Con_Otro
    (buscar 'a '( (b 3) (a 2)))
    '2
)

(test 'Buscar_En_Memoria_No_Esta
    (buscar 'c '( (b 3) (a 2)))
    nil
)

(test 'Pertenece_ListaPares_Esta_Solo
    (pertenece_ListaPares 'a '((a 2)))
    t
)

(test 'Pertenece_ListaPares_Esta_Con_Otro
    (pertenece_ListaPares 'a '( (b 3) (a 2)))
    t
)

(test 'Pertenece_ListaPares_No_Esta
    (pertenece_ListaPares 'c '( (b 3) (a 2)))
    nil
)

(test 'Modificar_Esta_Solo
    (modificar_Valor_Var_En_Mem 'a 5 '( (a 2)))
    '( (a 5))
)

(test 'Modificar_Esta_Con_Otro
    (modificar_Valor_Var_En_Mem 'a 5 '( (b 3) (a 2)))
    '( (b 3) (a 5))
)

(test 'Modificar_No_Esta
    (modificar_Valor_Var_En_Mem 'c 2 '( (b 3) (a 2)))
    '( (b 3) (a 2))
)

(test 'Agregar_No_Esta
    (agregar_Var_A_Mem '(c = 2) '( (b 3) (a 2)))
    '( (c 2) (b 3) (a 2))
)

(test 'Agregar_Esta
    (agregar_Var_A_Mem '(a = 4) '( (b 3) (a 2)))
    '( (a 4) (b 3) (a 2))
)

(test 'var_def_ok 
    (isVarDef '((int i)) )
    t
)

(test 'var_def_no_name 
    (isVarDef '((int)) )
    nil
)

(test 'var_def_no_type 
    (isVarDef '((i)) )
    nil
)

(test 'var_def_wrong_type 
    (isVarDef '((integer i)) )
    nil
)

'(----- Fin tests -----)

'(----- Ejecución completa -----)
'(La salida debe ser "hola" "cond_true" 21 A B 12)

(run '(
        (int n j = 10 i k z)
        (main(
            (i = 1)
            (z = 10)
            (repeat (
                    (scanf z)
                    (printf z)
                )
                until (z == 10)
            )
            (repeat (
                    (z -- )
                    (printf z)
                )
                until (z < 0)
            )
            (while (i <= 10)
                (i ++)
            )
            (for ( (int w = 0) (w <= 10) ( w ++) )
                (i ++)
            )
            (-- i)
            (i --)
            (printf "hola")
            (if (1 && 2) (printf "cond_true") else (printf "cond_false"))
            (printf (i + 1))
            (scanf n)
            (printf n)
            (scanf n)
            (printf n)
            (k = ((1 + 2) * ( 3 + 1)))
            (printf k)
            )
        )
    ) ;codigo
    '(1 2 3 10 A B) ;entrada
)


