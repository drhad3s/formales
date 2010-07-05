; Problema : N reinas, solución planteada por la profesora, con algunas mejoras minimas.(Llega a 15 reinas. Apartir de las 14, funciona mas lento)
; Explicación : Esta solución está basada en mantener un tablero con las posibles reinas en él, y descartar posiciones según las reinas que se vayan eligiendo. Aparte se guardan un historial de las reinas para poder volver para atras.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

(defun eliminarColumna (elemento filaTab)
    (if (null filaTab)
        nil
        (if (or (or (eq (apply '+ elemento) (apply '+ (car filaTab)))
                    (eq (apply '- elemento) (apply '- (car filaTab)))
                (= (cadar filaTab) (cadr elemento) )) )
            (eliminarColumna elemento (cdr filaTab))
            (cons (car filaTab) (eliminarColumna elemento (cdr filaTab)) )
        )
    )
)

(defun eliminaPorFila (elemento tab )
    (if (null tab)
        nil
        (cons (eliminarColumna elemento (car tab) ) 
               (eliminaPorFila elemento (cdr tab))
        )
    )
)

(defun eliminanulos (lista)
    (if (null lista)
        nil
        (if (null (car lista))
            (eliminanulos (cdr lista))
            (cons (car lista) (eliminanulos (cdr lista)))
        )
    )
)

(defun elimtodoslospares(listapos tab)
    (if (null listapos) 
        (eliminanulos tab)
        (elimtodoslospares (cdr listapos) (eliminaPorFila (car listapos) (cdr tab)))
    )
)

(defun buscarLong2oMas (L)
    (if (null L)
        nil
        ( if (>= (length (car L)) 2)
            L
            (buscarLong2oMas (cdr L))
        )
    )
)

(defun crearFila (maxCol &optional (fila '1) (minCol '1))
    (if (> minCol maxCol)
        nil
        (cons (list fila minCol) (crearFila maxCol fila (+ minCol '1) ))
    )
)

(defun tablero (n &optional (fila '1) (columna '1))
    (if (> fila n)
        nil
        (cons (crearFila N fila) (tablero N (+ fila '1) (+ columna '1 ) ) )
    )
)

(defun reinasaux (n posreinas)        
    (reinas n  (cons (cdar posreinas) (cdr posreinas)) (elimtodoslospares (cons (cadar posreinas)(mapcar 'car (cdr posreinas))) (tablero n)))
)

(defun reinas (n &optional (posreinas (list(car (tablero n)))) (tab (elimtodoslospares (list(caar posreinas)) (tablero n)))  )
    (if (eq (length posreinas) n)
        (reverse (mapcar 'car posreinas))
        (if (< (+ (length posreinas) (length tab)) n) ; No tengo posiblidad de llegar a las N reinas.
            (reinasaux n  (buscarlong2omas posreinas))    
            (if (null tab) 
                (reinasaux n (buscarlong2omas posreinas))  
                (reinas n (cons (car tab) posreinas) (elimtodoslospares (list (caar tab)) tab))
            )
        )
    )
)

(reinas 15)
