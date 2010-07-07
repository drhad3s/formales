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

