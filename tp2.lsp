(defun get-value (x lista)
	(cond
		((null lista) x)
		((eq (car lista) x) (cadr lista))
		(T (get-value x (cddr lista)) )
	)
)
(defun ampliar-entorno (variables valores args)
		 (append
		 	(reduce #'append (trasponer 
		 		(list variables (mapcar (lambda (x) (evaluar x args)) valores) )
		 	)) 
		 args)
)
(defun trasponer (m)
		(if (null (car m))
		nil
		(append 
			(list (mapcar #'car  m )) 
			(trasponer (mapcar #'cdr  m ) ))		
 		)
 )
(defun mapfun (fn lista args)
	(if (null lista) nil
		(append (list (evaluar (list fn (car lista)) args )) (mapfun fn (cdr lista) args) )
	)
)
(defun mapfunnadico (fn lista args)
	(if (null lista) nil
		(append (list (evaluar (append (list fn) (car lista)) args )) (mapfunnadico fn (cdr lista) args) )
	)
)
(defun micond (lista args)
	(if (null lista) nil
		(if (eq (evaluar (caar lista) args) T)
			(evaluar (cadar lista) args)
			(micond (cdr lista) args)
		)
	)
)

(defun evaluar (fn args)
	(print (list 'evaluar fn))
	(if (atom fn) (if (null fn) nil (if (numberp fn) fn (get-value fn args))  )		
		(cond
		((eq (car fn) 'QUOTE) (cadr fn))
		((eq (car fn) 'AND) (if (evaluar (cadr fn) args) 
								(if (evaluar (caddr fn) args) T nil ) 
								nil							
							))
		((eq (car fn) 'OR) (if (evaluar (cadr fn) args) T	
								(if (evaluar (caddr fn) args) T nil )
							))
	
		((eq (car fn) 'LIST) (mapcar #'(lambda (x) (evaluar x args)) (cdr fn)) )
		((eq (car fn) 'CAR) (car (evaluar (cadr fn) args)) )
		((eq (car fn) 'CDR) (cdr (evaluar (cadr fn) args)) )
		((eq (car fn) 'CONS) (cons (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) 'if) (if (evaluar (cadr fn) args)  (evaluar (caddr fn) args)  (evaluar (cadddr fn) args))) 
		((eq (car fn) 'eq) (eq (evaluar (cadr fn) args) (evaluar (caddr fn) args)))
		((eq (car fn) '*) (* (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '-) (- (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '+) (+ (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '/) (/ (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '<) (< (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )	
		((eq (car fn) '>) (> (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )	
		((eq (car fn) 'not) (not (evaluar (cadr fn) args) ) )	
		((eq (car fn) 'append) (reduce #'append (mapcar (lambda (x)  (evaluar x args) ) (cdr fn)  ) ))	
		((eq (car fn) 'nth) (nth (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )		
		((eq (car fn) 'cond) (micond (cdr fn ) args ) )


		((eq (car fn) 'lambda) fn)	


		((and (listp (car fn)) (eq (caar fn) 'lambda) )
			(evaluar (caddar fn) (ampliar-entorno (cadar fn) (cdr fn) args) ))
		((and (listp fn) (reduce (lambda (x y) (and x y)) (mapcar #'numberp fn )) ) fn)
		((eq (car fn) 'mapcar) 
			(if  (eq (length (cddr fn)) 1 )
				(mapfun (evaluar (cadr fn) args) ( evaluar (caddr fn) args) args)
				(mapfunnadico (evaluar (cadr fn) args) (trasponer (mapcar (lambda (x) (evaluar x args)) (cddr fn) )) args)
			)
		)
			

		(T (evaluar (cons (get-value (car fn) args) (cdr fn)) args)	)

		)
	)
)


(print (evaluar '(mapcar 'union '((a v e)(s e a)) '((m a s)(m e n o s)))
 '(union (lambda(x y)
 (if (null x) y
 (if (pertenece (car x)y) (union (cdr x)y)
 (cons (car x)(union (cdr x)y))
)
 )
 )
 pertenece (lambda (a li)
 (if (null li) nil
 (if (eq a (car li)) t
 (pertenece a (cdr li))
 )
 )
 )
 )
))
