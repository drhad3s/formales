; Problema : GPS.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

'(----- Load utils for tests -----)

(setq grafoTest '((a(b c)) (b(a e d)) (c(a d e)) (d(b c e)) (e(e b d)) ) )

(setq diccionarioTest '(
                        (a (PaseoColon Independencia))
                        (b (PaseoColon Chile))
                        (f (Independencia Balcarse))
                        (g (Independencia Defensa))
                        (h (Defensa Chile))
                        (k (Defensa Balcarse ))
                        (l (Belgrano Balcarse) )
                   ) 
)

(defun test (name a e)
    (if (equal a e)
        (list name 'passed)
        (list name 'fail '=> 'expected e 'actual a)
    )
)

'(----- Inicio tests -----)

(test 'diferenciaNull 
    (diferencia '(a b) nil) 
    '(a b)
)

(test 'diferenciaOneElement 
    (diferencia '(a b c) '(b)) 
    '(a c)
)

(test 'diferenciaMoreElements 
    (diferencia '(a b c) '(a c)) 
    '(b)
)

'(---------)

(test 'obtenerMinimoEqualsAndNil 
    (obtenerMinimo '((a) (b) nil (c)) ) 
    '(c)
)

(test 'obtenerMinimoLast 
    (obtenerMinimo '((a b c) (a b)) ) 
    '(a b)
)

(test 'obtenerMinimoFirst 
    (obtenerMinimo '((a b) (a b c)) ) 
    '(a b)
) 

(test 'obtenerMinimoMiddle 
    (obtenerMinimo '((a b c) (a b) (a b c d)) ) 
    '(a b)
)

'(---------)

(test 'obtenerMaximoEqualsAndNil 
    (obtenerMaximo '((a) (b) nil (c)) )
    '(a)
)

(test 'obtenerMaximoFirst 
    (obtenerMaximo '((a b c) (a b)) )
    '(a b c)
)

(test 'obtenerMaximoLast 
    (obtenerMaximo '((a b) (a b c)) )
    '(a b c)
)

(test 'obtenerMaximoMiddle 
    (obtenerMaximo '((a b) (a b c) (a)) )
    '(a b c)
)

'(---------)
(test 'vecinosNull 
    (vecinos 'a nil)
    'nil
)

(test 'vecinosFirst 
    (vecinos 'a '((a(b c)) (b(a e d)) )) 
    '(b c)
)

(test 'vecinosMiddle 
    (vecinos 'c '((a(b c)) (c(a d e)) (b(e b d)) ))
    '(a d e)
)

(test 'vecinosLast 
    (vecinos 'b '((a(b c)) (c(a d e)) (b(e b d)) ))
    '(e b d)
)

'(---------)

(test 'GPS 
    (GPS 'a 'e grafoTest)
    '((E B A) (E C D B A) (E D B A) (E B D C A) (E D C A) (E C A))
)

(test 'GPSFinalNotExist 
    (GPS 'a 'w grafoTest)
    'nil
)

(test 'GPSInitialNotExist 
    (GPS 'w 'a grafoTest)
    'nil
)

'(---------)

(test 'caminoMaximo 
    (caminoMaximo 'a 'e grafoTest)
    '(E C D B A)
)

(test 'caminoMinimo 
    (caminoMinimo 'a 'e grafoTest)
    '(E C A)
)

'(---------)

(test 'compararEsta 
    (comparar '(PaseoColon Chile) '(PaseoColon Chile) )
    'T
)

(test 'compararEstaInvertido 
    (comparar '(Chile PaseoColon) '(PaseoColon Chile) )
    'T
)

(test 'compararNoEsta 
    (comparar '(No esta) '(PaseoColon Chile) )
    'nil
)

'(---------)

(test 'codificadorEsta 
    (codificador '(PaseoColon Chile) diccionarioTest )
    'b
)

(test 'codificadorEstaInvertido 
    (codificador '(Chile PaseoColon) diccionarioTest )
    'b
)

(test 'codificadorNoEsta 
    (codificador '(No esta) diccionarioTest )
    'nil
)

'(---------)

(test 'traductorEsta
    (traductor 'b diccionarioTest )
    '(PaseoColon Chile)
)

(test 'traductorNoEsta 
    (traductor 'w diccionarioTest )
    'nil
)

'(----- Fin tests -----)


