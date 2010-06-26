(defun ListaArgEval (expr amb)
	(if (null expr)
		nil
		(if (atom expr)
			expr
			(cons (evalTLC (car expr) amb) ( ListaArgEval ( cdr expr) amb ) )
		)
	)
)

(defun ExtenderAmb (LP LA amb) 
    (if (null LP)
        amb
        (append (list (car LP) (car LA)) (ExtenderAmb ( cdr LP)(cdr LA) amb ) )
    )        
)

(defun aplicar (FN params amb)
    (if (atom FN)
        ( cond 
            ((eq FN 'car) (caar params))
            ((eq FN 'list ) params )
            ((eq FN 'cdr ) (cdar params) )
            ((eq FN 'cons ) (cons (car params) (cadr params)))
        )
        (evalTLC ( nth 2 FN) (ExtenderAmb (nth 1 FN) params amb ))
    )
)

(defun buscar (expr amb)
    (if (null amb)
		expr
		(if (atom amb)
			(if ( eq expr amb) 
				amb
				nil
			)
			(if (eq (car amb) expr)
				(cadr amb)
				(buscar expr (cddr amb))
			)
		)
	)
)

(defun evalTLC (expr amb)
	(if (null expr)
		nil
		(if (atom expr)
			(if (numberp expr)
				expr
				(buscar expr amb)
			)
			( cond 
				(( eq (car expr) 'quote) 
					(nth 1 expr)
				)
				(( eq (car expr) 'and) 
					( if (evalTLC (nth 1 expr) amb )
						(evalTLC (nth 2 expr) amb)
						nil
					)
				)
				(( eq (car expr) 'or) 
					( if (evalTLC (nth 1 expr) amb )
						T
						(evalTLC (nth 2 expr) amb)
					)
				)
				(( eq (car expr) 'if) 
					( if (evalTLC (nth 1 expr) amb )
						(evalTLC (nth 2 expr) amb)
						(evalTLC (nth 3 expr) amb)
					)
				)
				(( eq (car expr) 'cons)
					(cons (nth 1 expr) (nth 2 expr))
				)
                (( eq (car expr) 'lambda) 
                    expr
				)
				( T
					(aplicar    (evalTLC (car expr) amb) 
                                (ListaArgEval (cdr expr) amb )
                                (mapcar (lambda(x) (evalTLC x amb)) (cdr expr) ) 
                    )
				)
			)
		)
	)
)

(evalTLC NIL NIL )
(evalTLC '3 NIL )
(evalTLC '(if T 1 2) NIL )
(evalTLC '(sum 2 3) '((sum (lambda (a b) (+ a b)))) )
(evalTLC '(cdr (a b c)) NIL)
(evalTLC '(car (a b c)) NIL)
(evalTLC '(car ( cdr (a b c))) NIL )

