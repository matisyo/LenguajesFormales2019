(setq grafo '(
	(a (b f)) 
	(b (a c ))
	(c (b d ))
	(d (c n e)) 
	(e (d)) 
	(f (g ))
	(g (h)) 
	(h (i l)) 
	(i ( m j)) 
	(j ( k)) 
	(k (o))
	(l (b f)) 
	(m (l c)) 
	(n ( j m)) 
	(o (e n))
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
;----- Generar Recorridos

(defun dfs (nodo recorrido destino grafo n)
	(cond
		( (equal nodo destino) (list (append (list n) recorrido (list nodo))))
		( (null (in nodo recorrido)) 
			(reduce (lambda (x y) (append  x y ) )
			(mapcar 
				(lambda (z) (dfs z (append  recorrido (list nodo)) destino grafo (+ 1 n) ))
				(get-value nodo grafo)
			)
		    )
		)
		(T nil)
	)
)

(defun keep-best (m)
	( let
		 (
		( minimo (car (reduce (lambda (x y) (if (< (car x)  (car y) ) x y) )  m )))
		)
		 
		(reduce
			(lambda (x y) (append x y))
			(mapcar (lambda (x) (if (equal (car x) minimo) (list (cdr x)) nil ) ) m )
		)
	)
)


;----- Mostrar Recorrido
(defun in (a l)
	(if (null l)
	nil
	(reduce
		(lambda (x y) (append x y) )
		(mapcar (lambda (x) (if (equal a x) (list a) nil) ) l)
		)
	)
)

(defun join (l1 l2)
	(if (null l1) 
		nil
		(append (in (car l1) l2) (join (cdr l1) l2))
	)
)
(defun get-key (letra diccionario)
(car
(reduce
		(lambda (x y) (append x y ) )	
		(mapcar 
			(lambda (linea) 
				(if (equal (cadr linea)  letra)
					(list (car linea))
					nil
				)
			)
			diccionario
		)
	)
)
)

(defun get-value (letra diccionario)
	(reduce
		(lambda (x y) (append x y ) )	
		(mapcar 
			(lambda (linea) 
				(if (equal (car linea)  letra)
					(cadr linea)
					nil
				)
			)
			diccionario
		)
	)
)

(defun make_path (camino diccionario)
	(cond
		((null (cdr camino)) nil)
		(T 				
			(append 
			(join
			(get-value (car camino) diccionario) 
			(get-value (cadr camino) diccionario) 
			)
			(make_path (cdr camino) diccionario)
			) 						
		)

	)
)

(defun describe_path (lista n)
	(cond	
		( (null (cdr lista)) (format t "RECORRER ~D CUADRAS POR ~A HASTA LLEGAR A DESTINO~%~%" n (car lista)))
		( (equal (car lista) (cadr lista)) (describe_path (cdr lista) (+ 1 n)) )  
		(T 
			(format t "RECORRER ~D CUADRAS POR ~A Y DOBLAR EN ~A~%" n (car lista) (cadr lista) )
			(describe_path (cdr lista) 1)
			)
	)
)

;;----- Main
(defun GPS (desde hasta grafo diccionario)
	( let (
		(i (get-key desde diccionario) )
		(f (get-key hasta diccionario) )
		)	
	(mapcar (lambda (x) (describe_path (make_path x diccionario) 1) )
	 		(keep-best 
	 			(dfs i nil f grafo 0)
	 		)
	)
	)
)

(print (GPS '(PaseoColon Independencia) '(Defensa Belgrano) grafo diccionario))
