(defun inicializa ()
	(load "globales.fas" :verbose nil)
	(load "mates.fas" :verbose nil)
	(load "ac2001.fas" :verbose nil)
	(load "random-2.fas" :verbose nil)
	(load "robot-ac2001.fas" :verbose nil)
)

(defun ejecutar-ac2001-binarias(nvariables ndominio nrestricciones npares)
	(when (not (genera-psr-2 nvariables ndominio nrestricciones npares))
		(return)
	)
	
	(format t "~%~%Ejemplo aleatorio de AC2001~%~%")	
	(format t "Variables y sus dominios:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Restricciones:~%")
	(format t "~a~%" *restricciones*)
	(format t "~%")
	
	(format t "Pares prohibidos:~%")
	(format t "~a~%" *pares-prohibidos*)
	(format t "~%")
		
	(ac-2001 *variables*)
	(format t "Después de ejecutado el algoritmo AC2001:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "~%Tiempo de ejecucion: ~F segundos." (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
)
