; Problema : GPS.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

(defun pertenece (A L)
  (if (null L)
    nil
    ( if (eq A (car L))
      T
      (pertenece A (cdr L))
    )
  )
)

(defun diferencia (a b)
  (if (null a)
    nil
    (if (null b)
      a
      (if (pertenece (car a) b)
        (diferencia (cdr a) b)
        (cons (car a) (diferencia (cdr a) b))
      )
    )
  )
)

(defun vecinos (actual L)
  (if (null L)
    nil
    ( if (eq actual (caar L))
      (cadar L)
      (vecinos actual (cdr L))
    )
  )
)

(defun distribuir (cam vec)
  (if (null vec)
    nil
    (cons (cons (car vec) cam) (distribuir cam (cdr vec)))
  )
)

; obtiene todos los caminos posibles de i a f en el grafo
(defun GPS (i f grafo &optional(caminos (list( list i))) )
  (if (null caminos)
    nil
    (if (eq (caar caminos) f)
      (cons (car caminos) (GPS i f grafo (cdr caminos) ) )
      (GPS i f grafo (append (distribuir(car caminos)
                (diferencia (vecinos (caar caminos) grafo) (car caminos))
               )
              (cdr caminos)
           )
      )
    )
  )
)

(defun obtenerMinimo (L)
  (if (eq (length L) 1)
    (car L)
    (if (null (car L) )
      (obtenerMinimo (cdr L) )
      (if (< (length (car L)) (length (cadr L)) )
        (obtenerMinimo (cons (car L) (cddr L)) )
        (obtenerMinimo (cdr L) )
      )
    )
  )
)

(defun obtenerMaximo (L)
  (if (eq (length L) 1)
    (car L)
    (if (null (car L) )
      (obtenerMaximo (cdr L) )
      (if (< (length (car L)) (length (cadr L)) )
        (obtenerMaximo (cdr L) )
        (obtenerMaximo (cons (car L) (cddr L)) )
      )
    )
  )
)

(defun caminoMinimo (i f grafo)
  ( obtenerMinimo (GPS i f grafo) )
)

(defun caminoMaximo (i f grafo)
  ( obtenerMaximo (GPS i f grafo) )
)

'(----- Codificador / Decodificador -----)

(defun traductor ( c diccionario)
  (if (null diccionario)
    nil
    ( if (eq c (caar diccionario))
      (cadar diccionario)
      (traductor c (cdr diccionario))
    )
  )
)

(defun traductorList ( L diccionario)
  (if (null L)
    nil
    ( cons (traductor (car L) diccionario ) (traductorList (cdr L) diccionario ) )
  )
)

(defun compararPorCalle (calle otroTerm)
  (or (equal calle (car otroTerm) ) 
    (equal calle (cadr otroTerm) )
  )
)

(defun comparar (term otroTerm)
  (and (compararPorCalle (car term) otroTerm) (compararPorCalle (cadr term) otroTerm) )
)

(defun codificador ( term diccionario)
  (if (null diccionario)
    nil
    ( if (comparar term (cadar diccionario))
      (caar diccionario)
      (codificador term (cdr diccionario))
    )
  )
)

; obtiene cual es el comun entre 2 esquinas, para saber por cual calle va.
(defun obtenerComun (term otroTerm)
  (if (null otroTerm)
    (car term)
    (if (compararPorCalle (car term) otroTerm)
      (car term)
      (if (compararPorCalle (cadr term) otroTerm)
        (cadr term)
        nil
      )
    )
  )
)

; obtiene el comun de los 2 primeros del recorrido
(defun obtenerComunPrimeros (recorrido )
  (if (null recorrido)
    nil
    (if (= (length recorrido) 1)
      (caar recorrido)
      (obtenerComun (car recorrido) (cadr recorrido))
    )
  )
)


(defun crearNuevoActual (recorrido)
  (list (obtenerComunPrimeros recorrido) 1)
)

(defun aumentarContadorCuadras (actual)
  (list (car actual) (+ (cadr actual) 1))
)

(defun agregarAlFinal (lista item)
  (if (null lista)
    (list item)
    (append lista (list item))
  )
)

; comprime el recorrido generando pares calle por la que va, cantidad de cuadras.
(defun comprimirRecorrido (recorrido &optional (result nil) 
                    ( actual (list (obtenerComunPrimeros recorrido) 0)) )
  (if (null recorrido)
    (agregarAlFinal result actual)
    (if (equal (car actual) (obtenerComunPrimeros recorrido) ) ; en la proxima sigue en la misma calle
      (comprimirRecorrido (cdr recorrido) result (aumentarContadorCuadras actual) )
      (comprimirRecorrido (cdr recorrido) (agregarAlFinal result actual) (crearNuevoActual recorrido ))
    )
  )
)

(defun armarDescripcion (actual)
  (list 'luego 'gire 'en (car actual) 'y 'avance (cadr actual) 'cuadras.)
)

; Genera la descripción del circuito basado en el recorrido comprimido
(defun describir (rec &optional (desc nil))
  (if (null rec)
    (append desc '(hasta llegar a destino.))
    (if (null desc)
      (describir (cdr rec) (list 'Tome (caar rec) 'y 'avance (cadar rec) 'cuadras.) )
      (if (eq (cadar rec) 1)
        (describir (cdr rec) (append desc (list 'Doble 'en (caar rec) 'y 'avance 'una 'cuadra.) ) )
        (describir (cdr rec) (append desc (armarDescripcion (car rec)) ) )
      )
    )
  )
)

(defun traducirRecorrido (rec diccionario)
  (describir (comprimirRecorrido (traductorList rec diccionario)) )
)

