(defun inicializa ()
	(load "globales.fas" :verbose nil)
	(load "mates.fas" :verbose nil)
	(load "gac2001.fas" :verbose nil)
	(load "random-n.fas" :verbose nil)
)

(defun ejecutar-ac2001-multiples(nvariables ndominio nrestricciones ntuplas naridad)
	(when (not (genera-psr-n nvariables ndominio nrestricciones ntuplas naridad))
		(return)
	)
	
	(format t "~%~%Ejemplo aleatorio de AC2001 con aridad ~a~%~%" naridad)	
	(format t "Variables y sus dominos:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Restricciones:~%")
	(format t "~a~%" *restricciones*)
	(format t "~%")
		
	(gac-2001 *variables*)
	(format t "Después de ejecutado el algoritmo AC2001:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "~%Tiempo de ejecucion: ~F segundos." (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
)
