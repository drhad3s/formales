(defun asignacion (L mem)

)
(defun esVariable (A mem)
    (not (null (buscar A mem)))
)

(defun esAsignacion (l mem)
    (if (esVariable (car l) mem)
        T
        (if (or (eq (car l) '++) (eq (car l) '-- ))
            T
            nil
    ) 
)

(defun ejec (prg ent mem &optional (sal nil) )
    (if (null prg)
        sal
        (cond  
            ((eq (caar prg) 'scanf) (ejec (cdr prg) (cdr ent) (modifMem (cadar prg) (car ent) mem) sal))
            ((eq (caar prg) 'printf) (ejec (cdr prg) (cdr ent) mem (cons (eval (cadar prg) mem) sal )) ) ; esto imprime alrevez la salida
            ((eq (caar prg) 'if) (if (not (eq (eval (cadar prg) mem) 0 ))
                                    (ejec (append (nth 2 (car prg)) (cdr prg)) ent mem sal)
                                    (if (eq (length (car prg)) 5)
                                        (ejec (append (nth 4 (car prg)) (cdr prg)) ent mem sal)
                                        (ejec (cdr prg) ent mem sal)
                                    )
                                 ))
            ((eq (caar prg) 'while) (if (eq (eval (cadar prg) mem) 0)
                                        (ejec (cdr prg) ent mem sal)
                                        (ejec (append (nth 2 (car prg)) prg) ent mem sal)
                                    ))
        )
    )
)

(defun run (prg ent &optional (mem nil) )
    (if (null prg)
        nil
        (if (eq (caar prg) 'int)
            (run (cdr prg) ent (agregarAMem (cdar prg) mem))
            (if (eq (caar prg) 'main)
                (ejec (cadar prg) ent mem)
                'Error
            )
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;---------- Manejo de funciones y variables en memoria ---------

(defun buscar (elemento listaPares)
    (if (null listaPares)
        nil
        (if (equal elemento (caar listaPares) )
            (cdar listaPares)
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
            ((esFun (car sentencia) mem) (cons (ejec (getFun (car sentencia) mem) ent mem ) (armarExpresion (cdr sentencia) mem ent sal) )) ;TODO sacar a una nueva funcion para poder llamar a armar expresion con los nuevos ent y sal
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

(defun evaluarAux (listaParams) ; listaParams -> expresión,ent,sal
    (cons (operar (convertirALisp (car listaParams) ) ) (cdr listaParams) ) 
)

(defun evaluar (sentencia mem ent sal)
    (evaluarAux (armarExpresion sentencia mem ent sal ))  
)

(defun asignacionAux (nombreVar listaParams mem) ; listaParams -> nuevoValor, ent y sal
    (cons (modifVarMem (list nombreVar (car listaParams)) mem) (caddr listaParams))
)

(defun asignacion (sentencia mem ent sal)
    (if (esVariable (car sentencia) mem)
        (if (eq (nth 1 sentencia) '= ) 
            (asignacionAux (car sentencia) (evaluar (nth 2 sentencia) mem ent sal) mem)
            (if (eq (nth 1 sentencia) '++ )
                (asignacion (list (car sentencia) '= (car sentencia) '+1 ) mem ent sal)
                (if (eq (nth 1 sentencia) '-- )
                    (asignacion (list (car sentencia) '= (car sentencia) '-1 ) mem ent sal)
                    (asignacion (list (car sentencia) '= (car sentencia) (nth 1 sentencia) (nth 3 sentencia) ) mem ent sal)
                )
            )
        )
        (asignacion (reverse sentencia) mem ent sal )
    )
)
;----------fin de evaluacion de sentencias ---------

;----------Funciones------------
(defun scanf (prg ent mem sal)
    (ejec (cdr prg) (modifMem (cadar prg) (car ent) mem) (cdr ent)  sal)
)

(defun printf (prg ent mem sal)
    (ejec (cdr prg) mem (cdr ent) (append sal (list (evaluar (cadar prg) mem ent sal)) ));TODO ver lo que devuelve evaluar aca
)

(defun ifSentencia (prg ent mem sal)
    (if (not (eq (eval (cadar prg) mem) 0 ))
        (ejec (append (nth 2 (car prg)) (cdr prg)) mem ent sal)
        (if (eq (length (car prg)) 5)
            (ejec (append (nth 4 (car prg)) (cdr prg)) mem ent sal)
            (ejec (cdr prg) mem ent  sal)
        )
    )
)

(defun while (prg ent mem sal)
    (if (eq (eval (cadar prg) mem) 0)
        (ejec (cdr prg) mem ent sal)
        (ejec (append (nth 2 (car prg)) prg) mem ent  sal)
    )
)

(defun for (prg ent mem sal)
    ;(if (eq (eval (cadar prg) mem) 0)
    ;    (ejec (cdr prg) ent mem sal)
    ;    (ejec (append (nth 2 (car prg)) prg) ent mem sal)
    ;)
)

(defun ejecAsignacionAux (prg listaParams)
    (ejec (cdr prg) (car listaParams) (cadr listaParams) (caddr listaParams) ) 
)

(defun ejecAsignacion (prg mem ent sal)
    (ejecAsignacionAux prg (asignacion (car prg) mem ent sal))
)

(defun ejecFunAux (prg listaParams mem)
    (ejec (cdr prg) mem (car listaParams) (cadr listaParams) ) 
)

(defun ejecFun (prg ent mem sal)
    (ejecFunAux prg (ejec (getFun (car sentencia) mem) mem ent sal ) mem)
)

(defun ejec (prg mem ent sal )
    (if (null prg)
        (list ent sal)
        (cond  
            ((eq (caar prg) 'scanf) (scanf prg ent mem sal))
            ((eq (caar prg) 'printf) (printf prg ent mem sal))
            ((eq (caar prg) 'if) (ifSentencia prg ent mem sal))
            ((eq (caar prg) 'while) (while prg ent mem sal))
            ((eq (caar prg) 'for) (for prg ent mem sal))
            ((esAsignacion (car prg) mem) (ejecAsignacion prg ent mem sal))
            ((esFun (car prg) mem) (ejecFun (getFun (car sentencia) mem) ent mem sal))
            (T 'Error)
        )
    )
)
;---------- Fin Funciones------------

;---------- Main cicle -----------
(defun run (prg ent &optional (mem nil) (sal nil))
    (if (null prg)
        nil
        (if (isFunctionDef (car prg))
            (if (isMain (car prg))
                (cadr (ejec (cdr prg) mem ent sal))
                (run (cdr prg) ent (agregarFunAMem (car prg) mem))
            )
            (if (isVarDef (car prg))
                (run (cdr prg) ent (agregarVarAMem (evaluar (car prg) mem ent sal) mem)) ;TODO ver lo que devuelve evaluar aca
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
