(setq grafo '(
	(a(b f)) 
	(b(a c ))
	(c(b d ))
	(d(c n e)) 
	(e(d)) 
	(f(g ))
	(g(h)) 
	(h(i l)) 
	(i(m j)) 
	(j( k)) 
	(k(o))
	(l (b f)) 
	(m (l c)) 
	(n ( j m)) 
	(o(e n))
	)
)
(setq diccionario 
	'(
	(a (PaseoColon Independencia))
	(b (PaseoColon Chile))
	(c (PaseoColon Mexico ))
	(d (PaseoColon Venezuela))
	(e (PaseoColon Belgrano))
	(f (Independencia Balcarce))
	(g (Independencia Defensa))
	(h (Defensa Chile))
	(i (Defensa Mexico))
	(j (Defensa Venezuela))
	(k (Defensa Belgrano ))
	(l (Balcarce Chile ))
	(m (Balcarce Mexico))
	(n (Balcarce Venezuela))
	(o (Balcarce Belgrano))
	)
)
(defun isout (x x2)
	(not (reduce (lambda (a b) (or a b))
	(mapcar (lambda (tmp) (eq tmp x2)) x )))	
	)

(defun get-node (x x2)
	(equal x (cadr x2))
	)
(defun get-childs (x x2)
	(equal x (car x2))
	)

(defun delete-cond (condition x lista)
	(reduce 
	 	(lambda (lista-acum itero)
	 	
	 		(if  (funcall condition x itero) 
	 			(cons itero lista-acum) 
	 			lista-acum ))
	 	(append '(nil) lista) )
	)
	
(defun get-dict (condition x dict)
	(first (delete-cond condition x dict)))

(defun dfs (actual recorrido grafo hasta)
	(let (
		(hijos (cadr ( get-dict 'get-childs actual grafo)))
		)
	(cond ((equal hijos nil) nil)
		  ((eq actual hasta) (print (cons actual recorrido)))
		  (T 	(mapcar 
				(lambda (x) (dfs x (cons actual recorrido) grafo hasta ))
				(if (equal nil recorrido) hijos (delete-cond 'isout recorrido hijos)	)))
		))
	)


(defun dfs-legacy (actual recorrido grafo hasta)
	(let (
		(hijos (cadr ( get-dict 'get-childs actual grafo)))
		)
	(if (null hijos) nil
	(if (eq actual hasta)
		(print (cons actual recorrido))
		(mapcar 
			(lambda (x) (dfs x (cons actual recorrido) grafo hasta ))
			(if (equal nil recorrido) hijos (delete-cond 'isout recorrido hijos)	)))
	))
	)

(defun ultimo (lista)
	(cond ( (null (cdr lista)) (car lista)          )
		  (  T                   (ultimo (cdr lista)) )
		)
)
(defun revez (lista laux)
	(cond ( (null lista) laux                                         )
		  ( T                 (revez (cdr lista) (cons (car lista) laux) ) )
	)
)
(defun GPS (i f grafo dicc &optional (tray (list(list i))))
	(let (
		(desde (first (get-dict 'get-node i diccionario )))
		(hasta (first (get-dict 'get-node f diccionario )))
		)
	(print desde)
	(print hasta)
	(dfs desde nil grafo hasta)
	)
)

(defun join (a b) 
	(
	reduce (lambda (x y) (cons x y) )
	append nil  
	(mapcar (lambda (z) (includes z b)) a)

	)
)
	;)

(defun includes(a l2)
	(cond
		( (null l2) nil )
		( (eq a (car l2))  a )
		( T         (includes a (cdr l2) ) )
		)
)

;(print (ultimo '(1 2 3 4 5)))
;(print (revez '(1 2 3 4 5) '() ))

;(print (includes 1 '(2 1 4) ))

;(print (join '(1 2 3) '(2 1 4) ))
;(GPS '(PaseoColon Independencia) '(Defensa Belgrano) grafo diccionario)

