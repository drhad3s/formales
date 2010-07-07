; Problema : Interprete de C.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676

'(----- Ejecución completa -----)
'(La salida debe ser 1 2 3 10 9 8 7 6 5 4 3 2 1 0 -1 "hola" "cond_true" 21 A B 12)

(run '(
        (int n j = 10 i k z)
        (main(
            (i = 1)
            (z = 10)
            (repeat (
                    (scanf z)
                    (printf z)
                )
                until (z == 10)
            )
            (repeat (
                    (z -- )
                    (printf z)
                )
                until (z < 0)
            )
            (while (i <= 10)
                (i ++)
            )
            (for ( (int w = 0) (w <= 10) ( w ++) )
                (i ++)
            )
            (-- i)
            (i --)
            (printf "hola")
            (if (1 && 2) (printf "cond_true") else (printf "cond_false"))
            (printf (i + 1))
            (scanf n)
            (printf n)
            (scanf n)
            (printf n)
            (k = ((1 + 2) * ( 3 + 1)))
            (printf k)
            )
        )
    ) ;codigo
    '(1 2 3 10 A B) ;entrada
)


