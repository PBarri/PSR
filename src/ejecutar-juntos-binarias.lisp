(defun inicializa ()
	(load "globales.fas" :verbose nil)
	(load "mates.fas" :verbose nil)
	(load "ac3.fas" :verbose nil)
	(load "ac2001.fas" :verbose nil)
	(load "random-2.fas" :verbose nil)
)

(defun ejecutar-juntos-binarias(nvariables ndominio nrestricciones npares)
	(when (not (genera-psr-2 nvariables ndominio nrestricciones npares))
		(return)
	)
	
	(setf *variables-copia* (make-hash-table))
	(maphash #'(lambda (k v) (setf (gethash k *variables-copia*) (make-psr-var :nombre k :dominio (make-array ndominio :initial-contents (loop for i from 1 to ndominio collect i))))) *variables*)
	
	(format t "~%~%Ejemplo aleatorio para AC3/AC2001~%~%")	
	(format t "Variables y sus dominios:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Restricciones:~%")
	(format t "~a~%" *restricciones*)
	(format t "~%")
	
	(format t "Pares prohibidos:~%")
	;(format t "~a~%" *pares-prohibidos*)
	(format t "~%")
		
	(ac-3 *variables*)
	(format t "Después de ejecutado el algoritmo AC3:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "Tiempo de ejecucion: ~F segundos.~%~%" (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
	
	(ac-2001 *variables-copia*)
	(format t "Después de ejecutado el algoritmo AC2001:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables-copia*)
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "Tiempo de ejecucion: ~F segundos.~%" (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
)
