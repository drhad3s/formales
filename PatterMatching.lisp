; Problema : Patter matching.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

(defun verif (P asoc E L)
    (if (eq asoc 'NohayAsoc)
        (cons (list P E) L)
        (if (equal asoc E)
            L
            'NoMatchea
        )
    )
)

(defun buscar (V L)
    (if (null L) 
        'NohayAsoc
        (if (eq V (caar L))
            (cadar L)
            (buscar v (cdr L))
        )
    )
)

(defun pertenece (A L)
    (if (null L)
        nil
        ( if (eq A (car L))
            T
            (pertenece A (cdr L))
        )
    )
)

(defun esvar (P Vars)
    (if (atom P)
        (pertenece P Vars)
        nil
    )
)

(defun PatternMat (P E Vars &optional(L nil))
    (if (eq L 'NoMatchea)
        'NoMatchea
        (if (esvar P Vars)
            (verif P (buscar P L) E L)
            (if (atom P)
                (if (eq P E)
                    L
                    'NoMatchea
                )
                (PatternMat (cdr P) (cdr E) Vars (PatternMat (car P) (car E) Vars L ))
            )
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

;Ejemplo de uso
 (PatternMat '((B y C) A ) '((hola y chau) mundo) '(A B C))

