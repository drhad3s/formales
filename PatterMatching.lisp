(defun verif (P asoc E L)
    (if (eq asoc 'NohayAsoc)
        (append (List P E) L)
        (if (equal asoc E)
            L
            'NoMatchea
        )
    )
)

(defun buscar (V L)
    (if (null L) 
        'NohayAsoc
        (if (eq V (car L))
            (cadr L)
            (buscar v (cddr L))
        )
    )   
)

(defun esvar (P)
    (if (atom P)
        nil
        T
    )
)

(defun PatternMat (P E &optional(L nil))
    (if (eq L 'NoMatchea)
        'NoMatchea
        (if (esvar p)
            (verif P (buscar P L) E L)
            (if (atom P)
                (if (eq P E) 
                    L
                    'NoMatchea
                )
                (PatternMat (cdr P) (cdr E) (PatternMat (car P) (car E) L ))
            )
        )
    )
)

(PatternMat '(Hola mundo) '(Hola mundo) )
(PatternMat '(Pepe mundo) '(Hola mundo) )
(PatternMat '(Pepe mundo) '( (A) mundo) )
