;---------- Manejo de funciones y variables en memoria ---------

(defun buscar (elemento listaPares)
    (if (null listaPares)
        nil
        (if (equal elemento (caar listaPares) )
            (cadar listaPares)
            (buscar elemento (cdr listaPares))
        )
    )
)

(defun modificar (nombreElemento NuevoValor listaPares)
    (if (null listaPares)
        nil
        (if (equal nombreElemento (caar listaPares) )
            (cons (list nombreElemento NuevoValor ) (cdr listaPares) )
            (cons (car listaPares ) (modificar nombreElemento NuevoValor (cdr listaPares)))
        )
    )
)

(defun esVariable (nombreVar mem)
    (not (null (buscar nombreVar (car mem))))
)

(defun esFun (nombreFun mem)
    (not (null (buscar nombreFun (cadr mem))))
)

(defun esAsignacion (sentencia mem)
    (if (esVariable (car sentencia) mem)
        (or (eq (cadr sentencia) '= )
            (or (eq (cadr sentencia) '++ )
                (eq (cadr sentencia) '-- )
            )
        )
        (and (esVariable (cadr sentencia) mem)
            (or (eq (nth 0 sentencia) '++ )
                (eq (nth 0 sentencia) '-- )
            )
        )
    )
)

(defun getFun (fnName mem)
    (cdr (buscar fnName (cadr mem)))
)

(defun agregarFunAMem (sentencia mem)
    (if (null (buscar (cadr sentencia) (cadr mem)) )
        (list (car mem) (cons (list ( cadr sentencia ) sentencia) (cadr mem) ))
        'Error
    )
)

(defun getVarValue (varName mem)
    (cdr (buscar varName (car mem)))
)

(defun agregarVarAMem (sentencia mem)
    (if (null (buscar (cadr sentencia) (car mem)) )
        (cons (cons (list ( cadr sentencia ) (cdr sentencia) ) (car mem) )  (cdr mem) ) )
        'Error
    )
)

(defun modifVarMem (sentencia mem)
    (if (not (null (buscar (car sentencia) (car mem)) ))
        (cons (modificar (car sentencia) (cadr sentencia) (car mem)) (cdr mem) )
        'Error
    )
)

(defun isFunctionDef (sentencia)
     (if (= (length sentencia) 4)
        (isValidType (car sentencia) )
        nil
    )
)

(defun isMain (sentencia)
    (eq (cadr sentencia) 'main)
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

(defun isVarDef (sentencia)
    (if (>= (length sentencia) 2)
        (isValidType (car sentencia) )
        nil
    )
)

;---------- Fin Manejo de funciones y variables en memoria ---------

;---------- evaluacion de sentencias ---------
(defun armarExpresion (sentencia mem ent sal) ;TODO tiene que devolver: expresión,ent,sal
    (if (null sentencia)
        nil
        (cond 
            ((esVariable (car sentencia) mem) (cons (getVarValue (car sentencia) mem) (armarExpresion (cdr sentencia) mem ent sal) ))
            ((esFun (car sentencia) mem) (cons (ejecutar (getFun (car sentencia) mem) ent mem ) (armarExpresion (cdr sentencia) mem ent sal) )) ;TODO sacar a una nueva funcion para poder llamar a armar expresion con los nuevos ent y sal
            (T (cons (car sentencia) (armarExpresion (cdr sentencia) mem ent sal) ))
        )
    )
)

(defun convertirALisp (expresion)
    ;aca hay que hacer lo del labo.
)

(defun operar (lista)
    ;operar elemento por elemento de la lista con apply.. son funciones lisp
)

(defun evaluar_Aux (listaParams) ; listaParams -> expresión,ent,sal
    (cons (operar (convertirALisp (car listaParams) ) ) (cdr listaParams) ) 
)

(defun evaluar (sentencia mem ent sal)
    (evaluarAux (armarExpresion sentencia mem ent sal ))  
)

(defun asignacion_Aux (nombreVar listaParams mem) ; listaParams -> nuevoValor, ent y sal
    (cons (modifVarMem (list nombreVar (car listaParams)) mem) (caddr listaParams))
)

(defun asignacion (sentencia mem ent sal)
    (if (esVariable (car sentencia) mem)
        (cond
            ((eq (cadr sentencia) '= )  (asignacionAux (car sentencia) (evaluar (nth 2 sentencia) mem ent sal) mem) )
            ((eq (cadr sentencia) '++ ) (asignacion (list (car sentencia) '= (car sentencia) '+1 ) mem ent sal))
            ((eq (cadr sentencia) '-- ) (asignacion (list (car sentencia) '= (car sentencia) '-1 ) mem ent sal))
            (t (asignacion (list (car sentencia) '= (car sentencia) (cadr sentencia) (nth 3 sentencia) ) mem ent sal))
        )
        (asignacion (reverse sentencia) mem ent sal )
    )
)
;----------fin de evaluacion de sentencias ---------

;----------Funciones------------
(defun ejecutar_scanf (prg ent mem sal)
    (ejecutar (cdr prg) (modifMem (cadar prg) (car ent) mem) (cdr ent)  sal)
)

(defun ejecutar_printf (prg ent mem sal)
    (ejecutar (cdr prg) mem (cdr ent) (append sal (list (evaluar (cadar prg) mem ent sal)) ));TODO ver lo que devuelve evaluar aca
)

(defun ejecutar_if (prg ent mem sal)
    (if (not (eq (eval (cadar prg) mem) 0 ))
        (ejecutar (append (nth 2 (car prg)) (cdr prg)) mem ent sal)
        (if (eq (length (car prg)) 5)
            (ejecutar (append (nth 4 (car prg)) (cdr prg)) mem ent sal)
            (ejecutar (cdr prg) mem ent  sal)
        )
    )
)

(defun ejecutar_while (prg ent mem sal)
    (if (eq (eval (cadar prg) mem) 0)
        (ejecutar (cdr prg) mem ent sal)
        (ejecutar (append (nth 2 (car prg)) prg) mem ent  sal)
    )
)

(defun ejecutar_for (prg ent mem sal)
    ;(if (eq (eval (cadar prg) mem) 0)
    ;    (ejecutar (cdr prg) ent mem sal)
    ;    (ejecutar (append (nth 2 (car prg)) prg) ent mem sal)
    ;)
)

(defun ejecutar_Asignacion_Aux (prg listaParams)
    (ejecutar (cdr prg) (car listaParams) (cadr listaParams) (caddr listaParams) ) 
)

(defun ejecutar_Asignacion (prg mem ent sal)
    (ejecutar_Asignacion_Aux prg (asignacion (car prg) mem ent sal))
)

(defun ejecutar_Fun_Aux (prg listaParams mem)
    (ejecutar (cdr prg) mem (car listaParams) (cadr listaParams) ) 
)

(defun ejecutar_Fun (prg ent mem sal)
    (ejecutar_Fun_Aux prg (ejecutar (getFun (car sentencia) mem) mem ent sal ) mem);TODO falta evaluar parametros
)

(defun ejecutar (prg mem ent sal )
    (if (null prg)
        (list ent sal '0)
        (cond
            ((eq (caar prg) 'scanf) (ejecutar_scanf prg ent mem sal))
            ((eq (caar prg) 'printf) (ejecutar_printf prg ent mem sal))
            ((eq (caar prg) 'if) (ejecutar_if prg ent mem sal))
            ((eq (caar prg) 'while) (ejecutar_while prg ent mem sal))
            ((eq (caar prg) 'for) (ejecutar_for prg ent mem sal))
            ((eq (caar prg) 'return) (list ent sal (cadar prg) ) ;TODO ver que tengo que agregar una evaluación del parametro
            ((esAsignacion (car prg) mem) (ejecutar_Asignacion prg ent mem sal))
            ((esFun (car prg) mem) (ejecutar_Fun prg ent mem sal))
            (t (list 'syntax_error  (car prg) ))
        )
    )
)
;---------- Fin Funciones------------

(defun agregarVarAMemMainAux ( prg mem salidaEvaluada )
    (run (cdr prg) (cadr salidaEvaluada) (agregarVarAMem (car salidaEvaluada) mem) (caddr salidaEvaluada))
)

(defun agregarVarAMemMain ( prg mem ent sal )
    (agregarVarAMemMainAux prg mem (evaluar (car prg) mem ent sal) )
)

;---------- Main cicle -----------
(defun run (prg &optional (mem nil) (ent nil) (sal nil))
    (if (null prg)
        nil
        (if (isFunctionDef (car prg))
            (if (isMain (car prg))
                (cadr (ejecutar (cdr prg) mem ent sal));TODO falta evaluar parametros
                (run (cdr prg) (agregarFunAMem (car prg) mem) ent sal)
            )
            (if (isVarDef (car prg))
                (agregarVarAMemMain prg mem ent sal)
                'Error
            )
        )
    )
)

; ---------- test ------------
(setq testPrg 
    '((int i j = 10)
     (int k)
        (void main (argv argc) (
            (scanf k)
            (i = 1)
            (while (i < k) 
                (print i)
                (i ++)
            )
            (print j)
            )
        )
    )
)
