(defun asignacion (sentencia mem)
    (if (esVariable (car sentencia) mem)
        (cond
            ((eq (cadr sentencia) '= )  (asignacionAux (car sentencia) (evaluar (nth 2 sentencia) mem ) mem) )
            ((eq (cadr sentencia) '++ ) (asignacion (list (car sentencia) '= (car sentencia) '+1 ) mem))
            ((eq (cadr sentencia) '-- ) (asignacion (list (car sentencia) '= (car sentencia) '-1 ) mem))
            (t (asignacion (list (car sentencia) '= (car sentencia) (cadr sentencia) (nth 3 sentencia) ) mem))
        )
        (asignacion (reverse sentencia) mem)
    )
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
