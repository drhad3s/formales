; Problema : N reinas, solución propia.(Llega a 17 reinas. Apartir de las 14, funciona mas lento)
; Explicación : Esta solución está basada en validar cada reina individualmente contra el resto, esto hace que solo se mantenga en memoria la solución actual, ni siquiera se mantiene el tablero, solo la fila actual de donde se quiere sacar la nueva reina.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

(defun validarReinaColFilDiag (reina nuevaReina)
    (if (eq (car reina) (car nuevaReina))
        nil
        (if (eq (cadr reina) (cadr nuevaReina))
            nil
            (if (or (eq (apply '+ reina) (apply '+ nuevaReina))
                    (eq (apply '- reina) (apply '- nuevaReina)))
                nil
                T
            )
        )
    )
)

;Valida la nueva reina contra las ya existentes.
(defun validarReinas (reinas nuevaReina n)
    (if (null reinas)
        T
        (if (validarReinaColFilDiag (car reinas) nuevaReina )
            (validarReinas (cdr reinas) nuevaReina n) 
            nil
        )
    )
)

(defun crearFila (maxCol &optional (fila '1) (minCol '1))
    (if (> minCol maxCol)
        nil
        (cons (list fila minCol) (crearFila maxCol fila (+ minCol '1) ))
    )
)

;Busca un elemento para el cual se puede ir al siguiente, osea no sea la ultima columna
(defun buscarLong2oMas (L N)
    (if (null L)
        nil
        ( if (>= (cadar L) N )
            (buscarLong2oMas (cdr L) N)
            L
        )
    )
)

;intenta agregar una reina de la lista de posibles reinas.
(defun agregarReina (reinas nuevasReinas n)
    (if (null nuevasReinas)
        (cons nil reinas)
        (if (validarReinas reinas (car nuevasReinas) n)
            (cons (car nuevasReinas) reinas)
            (agregarReina reinas (cdr nuevasReinas) n)
        )
    )
)

;intenta agregar reinas hasta que tenga que volver para atras.
(defun ReinasAux (N posreinas )
    (If (null (car posreinas))
        posreinas
        (if (eq (length posreinas) N) 
            posreinas
            (if (< (length posreinas) (caar posreinas))
                (cons nil posreinas)
                (ReinasAux N (agregarReina posreinas (crearFila N (+(length posreinas) 1) ) N ) )
            )
        )
    )
)

;crea el entorno para volver a agregar una reina cuando se descarta la elegida
(defun reinasGoingBack (n posreinas)
    (agregarReina (cdr posreinas)  (crearFila N (caar posreinas) (+ (cadar posreinas) 1) ) n )
)

;Verifica si tiene que volver atras agregando las reinas, y descartar la ultima elegida.
(defun checkIfHaveToGoBack (N posreinas )
    (If (null (car posreinas)) ;tengo que volver atras
        (checkIfHaveToGoBack N (reinasGoingBack N  (buscarLong2oMas (cdr posreinas) N)))
        (if (eq (length posreinas) N) ; Termine
            posreinas
            (if (< (length posreinas) (caar posreinas)) ; No tengo posiblidad de llegar a las N reinas.
                (checkIfHaveToGoBack N (reinasGoingBack N  (buscarLong2oMas (cdr posreinas) N)))
                posreinas
            )
        )
    )
)

;Devuelve las reinas pero en orden inverso.
(defun ReverseReinas (N &optional (posreinas '((1 1)) ) )
    (if (eq (length posreinas) N )  
        posreinas
        (ReverseReinas N (checkifhaveTogoBack N (ReinasAux N posreinas)))
    )
)

;Main function
(defun Reinas (N )
    (print (reverse (ReverseReinas N )))
)

;Ejemplo de uso
(reinas 17)
