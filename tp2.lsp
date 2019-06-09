(defun get-value (x lista)
	(cond
		((null lista) nil)
		((eq (car lista) x) (cadr lista))
		(T (get-value x (cddr lista)) )
	)
)
(defun crear-entorno (l1 l2)
	(if (null l1) nil
	(append (list (car l1)) (crear-entorno l2 (cdr l1)) )
	)
)
(defun trasponer (m)
		(if (null (car m))
		nil
		(append 
			(list (mapcar #'car  m )) 
			(trasponer (mapcar #'cdr  m ) ))		
 		)
 )
(defun mapfun (fn lista)
	(print fn)
	(print (car lista))
	(if (null lista) nil
		(append (list (evaluar (list fn (car lista)) nil )) (mapfun fn (cdr lista)) )
	)
)
(defun mapfunnier (fn lista)
	(print fn)
	(print (car lista))
	(if (null lista) nil
		(append (list (evaluar (list fn (car lista)) nil )) (mapfun fn (cdr lista)) )
	)
)

(defun evaluar (fn args)

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
		((eq (car fn) '*) (* (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '-) (- (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '+) (+ (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '/) (/ (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )
		((eq (car fn) '<) (< (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )	
		((eq (car fn) '>) (> (evaluar (cadr fn) args) (evaluar (caddr fn) args)) )	


		((and (listp (car fn)) (eq (caar fn) 'lambda) )   
			(evaluar  (caddar fn) (crear-entorno  (cadar fn) (cdr fn) ) ))

		((eq (car fn) 'mapcar) 
			(if  (eq (length (cddr fn)) 1 )
				(mapfun (evaluar (cadr fn) args) ( evaluar (caddr fn) args) )
				(
					mapfun (evaluar (cadr fn) args) (trasponer (mapcar (lambda (x) (evaluar x args)) (cddr fn) ))
				)
			)
		)
			;(print (evaluar (cddr fn) args))
			
			
			;print ( evaluar (caddr fn) nil) 
			
			

		(T  fn
			;(aplicar 
			;(evaluar (car fn) args) 
			;(mapcar #'(lambda (x) (evaluar x args)) (cdr fn))
			;args
			;)
		)

		)
	)
)


(print (evaluar '(mapcar (lambda (x y z) (+ x  (+ y z)) ) '(1 2 3) '(1 2 3) '(1 2 3)) nil ) )

(print (evaluar '((lambda (x y) (+ x y)) 2 4) nil))
;(print(evaluar '(cons a b) '(a 100 b (1 2 3)) ))

; PRUEBAS
;(print (evaluar '2 nil))
;(print (evaluar 'nil nil))
;(print (evaluar 't nil))
;(print (evaluar 'A '(A 2)))
;(print (evaluar 'B '(A 2 B 10)))
;(print (evaluar '(quote A) nil))
;(print (evaluar '(quote 1) nil))
;(print (evaluar '(quote (car a)) nil))
;(print (evaluar '(quote ((2 3) (4 5))) nil))
;(print (evaluar '(and t t) nil ))
;(print (evaluar '(and t nil) nil ))
;(print (evaluar '(or t t) nil ))
;(print (evaluar '(or t nil) nil ))
;(print (evaluar '(or nil nil) nil ))
;(print (evaluar '(and (or t nil) t) nil))
;(print (evaluar '(and (or t nil) (or nil nil)) nil))
;(print (evaluar '(or (or t nil) (or nil nil )) nil))
;(print (evaluar '(list a 2 3) '(a 100)))
;(print (evaluar '(car (list a 2 3)) '(a 100)))
;(print (evaluar '(cdr (list a b c)) '(a 100 b 99 c 98)))
;(print (evaluar '((lambda (x) (* x 2)) 2) nil))
;(print (evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil))
;(print (evaluar '(lambda (x) (* x 2)) nil))

;(print (evaluar '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3)) nil))
;(print (evaluar '(mapcar 'car (quote ( (2 3) (4 5 )))) nil))

; (print (evaluar '(fact 5) '(fact (lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))))))
; (print (evaluar '(mapcar 'fact (quote ( 2 3 4 5 )))
; 				'(fact (lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))))))
; (print (evaluar '(not A) '(A T)))
; (print (evaluar '(nth 2 A) '(A (1 2 3 4))))
; (print (evaluar '(append (quote ((1 2) (3 4) (5 6)))) nil))
; (print (evaluar '(atom 1) nil))
; (print (evaluar '(atom (quote (1 2 3))) nil))
; (print (evaluar '(symbolp (quote A)) nil))
; (print (evaluar '(symbolp 1) nil))
; (print (evaluar '(numberp 1) nil))
; (print (evaluar '(listp (quote (1 2 3))) nil))
; (print (evaluar '(null (quote (1 2 3))) nil))
; (print (evaluar '(null nil) nil))
; (print (evaluar '(length (quote (1 2 3))) nil))
; (print (evaluar '(reduce (lambda (x y) (if (> x y) x y)) (quote (4 5 1 2 3))) nil))
; (print (evaluar '(reduce 'append (quote ((1 2) (3 4) (5 6)))) nil))
; (print (evaluar '(cond ((eq 1 3) nil) ((eq 1 2) nil) ((> 0 1) T)) nil))
; (print (evaluar '(cond ((eq 1 3) nil) ((eq 2 2) 2) ((> 2 1) T)) nil))
; (print (evaluar '(mapcar 'cons '(a b c) '(1 2 3)) nil))
; (print (evaluar '(mapcar 'list '(a b c) '(1 2 3) '(4 5 6)) nil))
;(print (evaluar '(mapcar 'suma '(1 2 3) '(4 5 6)) '(suma (lambda (x y) (+ x y)))))