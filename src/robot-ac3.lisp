;;; Funciones usadas por las restricciones en el problema del robot de planificación
;;; de los apuntes de la asignatura.
(defun distintasAB (tabla)
	(let 
		(
			(vA (gethash 'A tabla))
			(vB (gethash 'B tabla))
		)
		(not (= vA vB))
	)
)

(defun distintasBC (tabla)
	(let 
		(
			(vB (gethash 'B tabla))
			(vC (gethash 'C tabla))
		)
		(not (= vB vC))
	)
)

(defun distintasBD (tabla)
	(let 
		(
			(vB (gethash 'B tabla))
			(vD (gethash 'D tabla))
		)
		(not (= vB vD))
	)
)

(defun igualesAD (tabla)
	(let 
		(
			(vA (gethash 'A tabla))
			(vD (gethash 'D tabla))
		)
		(= vA vD)
	)
)
	
(defun menor-queCD (tabla)
	(let 
		(
			(vC (gethash 'C tabla))
			(vD (gethash 'D tabla))
		)
		(< vC vD)
	)
)

(defun menor-queEA (tabla)
	(let 
		(
			(vE (gethash 'E tabla))
			(vA (gethash 'A tabla))
		)
		(< vE vA)
	)
)

(defun menor-queEB (tabla)
	(let 
		(
			(vE (gethash 'E tabla))
			(vB	(gethash 'B tabla))
		)
		(< vE vB)
	)
)

(defun menor-queEC (tabla)
	(let 
		(
			(vE (gethash 'E tabla))
			(vC (gethash 'C tabla))
		)
		(< vE vC)
	)
)

(defun menor-queED (tabla)
	(let 
		(
			(vE (gethash 'E tabla))
			(vD (gethash 'D tabla))
		)
		(< vE vD)
	)
)	

;;; Función que inicializa las variables y restricciones del PSR de los apuntes
;;; de la asignatura: ROBOT con PLANIFICACIÓN.
(defun robot ()
	(setf *variables* (make-hash-table))
	(setf (gethash 'A *variables*) (make-psr-var :nombre 'A :dominio (vector 1 2 3 4)))
	(setf (gethash 'B *variables*) (make-psr-var :nombre 'B :dominio (vector 1 2 4)))
	(setf (gethash 'C *variables*) (make-psr-var :nombre 'C :dominio (vector 1 3 4)))
	(setf (gethash 'D *variables*) (make-psr-var :nombre 'D :dominio (vector 1 2 3 4)))
	(setf (gethash 'E *variables*) (make-psr-var :nombre 'E :dominio (vector 1 2 3 4)))
	
	(setf *restricciones* (list
	                        (make-psr-restr :variables '(A B) :funcion 'distintasAB)
							(make-psr-restr :variables '(B C) :funcion 'distintasBC)
							(make-psr-restr :variables '(B D) :funcion 'distintasBD)
							(make-psr-restr :variables '(A D) :funcion 'igualesAD)
							(make-psr-restr :variables '(C D) :funcion 'menor-queCD)
							(make-psr-restr :variables '(E A) :funcion 'menor-queEA)
							(make-psr-restr :variables '(E B) :funcion 'menor-queEB)
							(make-psr-restr :variables '(E C) :funcion 'menor-queEC)
							(make-psr-restr :variables '(E D) :funcion 'menor-queED)
						   ))
						   
	(format t "~%~%Ejemplo de juguete en los apuntes de la asignatura: ROBOT con PLANIFICACIÓN~%~%")
	(format t "Variables y sus dominios:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(ac-3 *variables*)
	(format t "Después de ejecutado el algoritmo AC3:~%")
	(maphash #'(lambda (k v) (format t "~a = ~a~%" k v)) *variables*)
	(format t "~%")
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
)
