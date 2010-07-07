; Problema : Interprete de C.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

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

