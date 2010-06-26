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

(defun generarDiag1 (diag N) 
    (if (estaAdentro (car diag) N)
        (generarDiag1 (cons (list (- (caar diag) '1) (- (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiag2 (diag n)
    (if (estaAdentro (car diag) N)
        (generarDiag2 (cons (list (- (caar diag) '1) (+ (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiag3 (diag n)
    (if (estaAdentro (car diag) N)
        (generarDiag3 (cons (list (+ (caar diag) '1) (+ (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiag4 (diag n)
    (if (estaAdentro (car diag) N)
        (generarDiag4 (cons (list (+ (caar diag) '1) (- (cadar diag) '1) ) diag) N)
        (cdr diag)
    )
)

(defun generarDiagonales (nuevaReina n ) 
    (append (generarDiag1 (list nuevaReina) n) 
        (append (generarDiag2 (list nuevaReina) n)
            (append (generarDiag3 (list nuevaReina) n)
                (generarDiag4 (list nuevaReina) n)
            )
        )
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

;(defun reinasaux (n posreinas)
;    (reinas n (agregarReina (cdr posreinas)  (crearFila N (caar posreinas) (+(cadar posreinas) 1) ) n ) )
;)

;(defun Reinas (N &optional (posreinas (list (car (crearFila N))) ) )
;    (If (null (car posreinas))
;        (reinasaux N  (buscarLong2oMas (cdr posreinas) N))
;        (if (eq (length posreinas) N) 
;            (reverse posreinas)
;	        (if (< (length posreinas) (caar posreinas))  ;me pase
;                (reinasaux N (buscarLong2oMas posreinas) N)
;                (Reinas N (agregarReina posreinas (crearFila N (+(length posreinas) 1) ) N ) )
;            )
;        )
;    )
;)

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
    (reverse (ReverseReinas N ))
)

;(trace validarReinaColFil)
;(trace agregarReina)
;(trace reinas)
;(trace reinasaux)
;(trace checkIfHaveToGoBack)
;(trace buscarLong2oMas)
;(buscarLong2oMas '(((1 4)) ((2 3) (2 4))) 4)
;(reinas 4)
;(reinas 5)
;(reinas 6)
;(reinas 7)
;(reinas 8)
;(reinas 9)
;(reinas 10)
(reinas 13)
;(reinas 14) ; empieza a tomarse mas tiempo
(reinas 17) ; maximo antes de overflow
;(reinas 18)
;(reinas 20)
