(defun eliminapardiagonal (par lista)
    (if (null lista) nil
        (if (or (eq (apply '+ par) (apply '+ (car lista))) 
                (eq (apply '- par) (apply '- (car lista)))
            )
        (eliminapardiagonal par (cdr lista))
        (cons (car lista) (eliminapardiagonal par (cdr lista)))
        )
    )
)  

(defun eliminadiagonales( par tab)
    (if (null tab) 
        nil
        (cons (eliminapardiagonal par (car tab)) (eliminadiagonales par (cdr tab)))
    )
)

(defun eliminafila (fil tab)
    (if (null tab)
        nil
        (if (eq fil (caaar tab)) 
            (cdr tab)
            (cons (car tab) (eliminafila fil (cdr tab)))
        )
    )
)

(defun eliminarpar (col lista)
    (if (null lista) 
    nil
        (if (eq col (cadar lista))
            (eliminarpar col (cdr lista))
            (cons (car lista) (eliminarpar col (cdr lista)))
        )
    )
)

(defun eliminacolumna (col tab)
    (if (null tab)
        nil
        (cons (eliminarpar col (car tab)) (eliminacolumna col (cdr tab)))
    )
)
	
(defun eliminanulos (l)
    (if (null l) 
        nil
        (if (null (car l))
            (eliminanulos (cdr l))
            l
        )
    )
)

(defun elimtodoslospares(listapos tab)
    (if (null listapos) 
        (eliminanulos tab)
        (elimtodoslospares (cdr listapos) 
            (eliminadiagonales (car listapos) 
                (eliminacolumna (cadar listapos) (eliminafila (caar listapos) tab)))
        )
    )
)

(defun buscarlong2omas (posreinas)
    (if (null (car posreinas)) 
        (buscarlong2omas (cdr posreinas))
        (if (null (cdar posreinas)) 
            (buscarlong2omas (cdr posreinas)) 
            posreinas
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
        (if (< (+ (length posreinas) (length tab)) n)  ;me pase
            (reinasaux n  (buscarlong2omas posreinas))    
            (if (null tab) 
                (reinasaux n (buscarlong2omas posreinas))  
                (reinas n (cons (car tab) posreinas) (elimtodoslospares (list (caar tab)) tab))
            )
        )
    )
)

(reinas 15)
