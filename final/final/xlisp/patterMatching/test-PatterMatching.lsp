; Problema : Patter matching.
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
(test 'No_Vars_Pero_Igual
    (PatternMat '(Hola mundo) '(Hola mundo) nil)
    nil
)

(test 'No_Vars_No_matchea
    (PatternMat '(Pepe mundo) '(Hola mundo) nil)
    'NoMatchea
)

(test 'Var_Simple
    (PatternMat '(A) '( Hola) '(A) )
    '((A HOLA))
)

(test 'Var_Simple_Mezclado_Palabras
    (PatternMat '(A mundo) '( Hola mundo) '(A) )
    '((A HOLA))
)

(test 'Var_Doble_Mezclado_Palabras_Repetido
    (PatternMat '(A mundo A) '( Hola mundo Hola) '(A))
    '((A HOLA))
)

(test 'Var_Doble_Mezclado_Palabras_Repetido_No_matchea
    (PatternMat '(A mundo A) '( Hola mundo Chau) '(A) )
    'NoMatchea
)

(test 'Var_Doble_Distinto_Mezclado_Palabras
    (PatternMat '(A mundo B) '( Hola mundo Chau) '(A B) )
    '((B CHAU) (A HOLA))
)

(test 'Var_Triple_Con_Parentesis_Distinto_Mezclado_Palabras
    (PatternMat '((B y C) A ) '((hola y chau) mundo) '(A B C))
    '((A MUNDO) (C CHAU) (B HOLA))
)

'(----- Fin tests -----)

