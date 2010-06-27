; Problema : N reinas, solución propia.
; Explicación : Esta solución está basada en validar cada reina individualmente contra el resto, esto hace que solo se mantenga en memoria la solución actual, ni siquiera se mantiene el tablero, solo la fila actual de donde se quiere sacar la nueva reina.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

(defun validarReinaColFil (reina nuevaReina)
    (if (eq (car reina) (car nuevaReina))
        nil
        (if (eq (cadr reina) (cadr nuevaReina))
            nil
            T
        )
    )
)

(defun validarReinasColFil (reinas nuevaReina)
    (if (null reinas)
        nil
        (cons (validarReinaColFil (car reinas) nuevaReina ) (validarReinasColFil (cdr reinas) nuevaReina) )
    )
)

(defun ListaValida (L)
    (if (null L)
        T
        (if (car L) 
            ( ListaValida ( cdr L))
            nil
        )
    )
)

(defun estaAdentro (nodo N)
    (if (and (> (car nodo) 0) (<= (car nodo) N) )
        (if (and (> (cadr nodo) 0) (<= (cadr nodo) N))
            T
            nil
        )
        nil
    )
)

(defun generarDiagIzq (diag N) 
    (if (estaAdentro (car diag) N)
        (generarDiagIzq (cons (list (- (caar diag) '1) (- (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiagDer (diag n)
    (if (estaAdentro (car diag) N)
        (generarDiagDer (cons (list (- (caar diag) '1) (+ (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiagonales (nuevaReina n ) 
    (append (generarDiagIzq (list nuevaReina) n) 
        (generarDiagDer (list nuevaReina) n)
    )
)

(defun pertenece (elemento lista)
    (if (null lista)
        nil
        (if (equal elemento (car lista) )
            T
            (pertenece elemento (cdr lista))
        )
    )
)

(defun algunElementoPertenece (elementos lista)
    (if (null elementos)
        T
        (if (pertenece (car elementos) lista)
            nil
            (algunElementoPertenece (cdr elementos) lista )
        )
    )
)

(defun validarDiag (reinas nuevaReina n)
    (algunElementoPertenece reinas (generarDiagonales nuevaReina n ) )
)

(defun validarReinas (reinas nuevaReina n)
    (if (null reinas)
        T
        (if (ListaValida (validarReinasColFil reinas NuevaReina ))
            (validarDiag reinas nuevaReina n)
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

(defun buscarLong2oMas (L N)
    (if (null L)
        nil
        ( if (>= (cadar L) N )
            (buscarLong2oMas (cdr L) N)
            L
        )
    )
)

(defun agregarReina (reinas nuevasReinas n)
    (if (null nuevasReinas)
        (cons nil reinas)
        (if (validarReinas reinas (car nuevasReinas) n)
            (cons (car nuevasReinas) reinas)
            (agregarReina reinas (cdr nuevasReinas) n)
        )
    )
)

(defun ReinasAux (N posreinas )
    (If (null (car posreinas))
        posreinas
        (if (eq (length posreinas) N) 
            posreinas
	        (if (< (length posreinas) (caar posreinas))  ;me pase
                (cons nil posreinas)
                (ReinasAux N (agregarReina posreinas (crearFila N (+(length posreinas) 1) ) N ) )
            )
        )
    )
)

(defun reinasGoingBack (n posreinas)
    (agregarReina (cdr posreinas)  (crearFila N (caar posreinas) (+ (cadar posreinas) 1) ) n )
)

(defun checkIfHaveToGoBack (N posreinas )
    (If (null (car posreinas))
        (checkIfHaveToGoBack N (reinasGoingBack N  (buscarLong2oMas (cdr posreinas) N)))
        (if (eq (length posreinas) N) 
            posreinas
            (if (< (length posreinas) (caar posreinas))
                (checkIfHaveToGoBack N (reinasGoingBack N  (buscarLong2oMas (cdr posreinas) N)))
                posreinas
            )
        )
    )
)

(defun ReverseReinas (N &optional (posreinas (list (car (crearFila N))) ) )
    (if (eq (length posreinas) N )  
        posreinas
        (ReverseReinas N (checkifhaveTogoBack N (ReinasAux N posreinas)))
    )
)

(defun Reinas (N )
    (print (reverse (ReverseReinas N )))
)

;(trace validarReinaColFil)
;(trace agregarReina)
;(trace reinas)
;(trace reinasaux)
;(trace checkIfHaveToGoBack)
;(trace buscarLong2oMas)
;(reinas 4)
;(reinas 5)
;(reinas 6)
;(reinas 7)
;(reinas 8)
;(reinas 9)
;(reinas 10)
(reinas 13)
;(reinas 14) ; empieza a tomarse mas tiempo
;(reinas 17) ; maximo antes de overflow
;(reinas 20)
