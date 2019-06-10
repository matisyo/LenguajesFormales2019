(defun get-value (x lista)
	(cond
		((null lista) x)
		((eq (car lista) x) (cadr lista))
		(T (get-value x (cddr lista)) )
	)
)
(defun trasponer (m)
		(if (null (car m)) nil
			(append (list (mapcar #'car  m )) 	(trasponer (mapcar #'cdr  m ) ))		
		)
 )
(defun ampliar (vars elems args) 
	
	(append 	
	(reduce #'append
	(trasponer (list vars elems)))
	args)
)

(defun micond (lista args)
	(if (null lista) nil
		(if (eq (evaluar (caar lista) args) T)
			(evaluar (cadar lista) args)
			(micond (cdr lista) args)
		)
	)
)
(defun aplicar (fn lae args)
	(if (atom fn)
		(cond
			;Un solo parametro

			((eq fn 'car)     (car     (car lae) ))
			((eq fn 'cadr)     (cadr     (car lae) ))
			((eq fn 'cdr)     (cdr     (car lae) ))
			((eq fn 'not)     (not     (car lae) ))
			((eq fn 'atom)    (atom    (car lae) ))
			((eq fn 'symbolp) (symbolp (car lae) ))
			((eq fn 'numberp) (numberp (car lae) ))
			((eq fn 'null)    (null    (car lae) ))
			((eq fn 'listp)   (listp   (car lae) ))
			((eq fn 'length)  (length  (car lae) ))
			;Dos parametros
			((eq fn 'cons) (cons (car lae) (cadr lae)))
			((eq fn 'eq)   (eq   (car lae) (cadr lae)))
			((eq fn '*)    (*    (car lae) (cadr lae)))
			((eq fn '+)    (+    (car lae) (cadr lae)))
			((eq fn '-)    (-    (car lae) (cadr lae)))
			((eq fn '/)    (/    (car lae) (cadr lae)))
			((eq fn '<)    (<    (car lae) (cadr lae)))
			((eq fn '>)    (>    (car lae) (cadr lae)))
			((eq fn 'nth)  (nth  (car lae) (cadr lae)))

			;Otras funciones
			((eq fn 'LIST)  lae ) 
			((eq fn 'append)  (reduce #'append lae ) )

			((eq fn 'mapcar) 
				(if (eq (length lae) 2)
					(mapcar (lambda (x) (aplicar (car lae) (list x) args)) (cadr lae))
					(mapcar (lambda (x) (aplicar (car lae) x args)) (trasponer (cdr lae)))
				)
			)
			(T (aplicar (get-value fn args) lae args))
		)		
		(evaluar (caddr fn) (ampliar (cadr fn) lae args))
	)
)
(defun while-cycle (p f e args)
	(if
		(evaluar 
			(append (list p)  (list 'e) )  
			(append (list 'e e) args)
		)
		(while-cycle p f 
			(evaluar 
				(append (list f)  (list 'e) )  
				(append (list 'e e) args)
			)
			args
		)
		e
	)
	
	)

(defun evaluar (fn args)
	(if (atom fn) (if (null fn) 
					   nil
					   (if (numberp fn) fn (get-value fn args))
				   )
		(cond 
			((eq (car fn) 'QUOTE) (cadr fn))
			((eq (car fn) 'and) (if (null (evaluar (cadr fn) args)) nil (evaluar (caddr fn) args)))
			((eq (car fn) 'or) (if (eq T (evaluar (cadr fn) args)) T (evaluar (caddr fn) args)))
			((eq (car fn) 'if) (if (null (evaluar (cadr fn) args)) (evaluar (cadddr fn) args) (evaluar (caddr fn) args)))
			((eq (car fn) 'lambda) fn)
			((eq (car fn) 'cond) (micond (cdr fn ) args ) )
			((eq (car fn) 'while) (while-cycle 									
									(cadr fn)
									(caddr fn)
									(evaluar (cadddr fn) args)
									args))
	
			
			
			(T (aplicar (car fn) (mapcar (lambda (x) (evaluar x args)) (cdr fn)) args))
		)
	)
)

(print (list 'resultado
(evaluar '(
		while 
		(lambda (x) (NoCero (car x)) ) 
		(lambda (x) (list (Restar1 (car x)) (* (car x) (cadr x)) ) )
		(car '( (5 1) 8 7) )
		)
		'(NoCero (lambda(x)(not(eq x 0))) 
		Restar1 (lambda(n)(- n 1) ) )
 )
)
)