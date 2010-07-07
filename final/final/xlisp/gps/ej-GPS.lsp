; Problema : GPS.
; Lenguajes Formales - Primer Cuatrimestre 2010
; Alumno : Bello Camilletti, Nicolás.
; Padrón : 86676


(setq grafoMain '(
        (a(b f)) (b(a c )) (c(b d)) (d(c e)) (e(d l))
         (f(g))                              (l(k))
         (g(h))  (h(i))    (i(c j)) (j(d k)) (k())
             ) 
)

(setq diccionario '(
        (a (PaseoColon Independencia))
        (b (PaseoColon Chile))
        (c (PaseoColon Mexico ))
        (d (PaseoColon Venezuela))
        (e (PaseoColon Belgrano))
        (f (Independencia Balcarse))
        (g (Independencia Defensa))
        (h (Defensa Chile))
        (i (Defensa Mexico))
        (j (Defensa Venezuela))
        (k (Defensa Balcarse ))
        (l (Belgrano Balcarse) )
                   ) 
)

(traducirRecorrido 
    (caminoMaximo 
        (codificador '(PaseoColon Independencia) diccionario) 
        (codificador '(PaseoColon Belgrano) diccionario)
        grafoMain
    )
    diccionario
)



