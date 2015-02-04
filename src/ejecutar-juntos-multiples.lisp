(defun inicializa ()
	(load "globales.fas" :verbose nil)
	(load "mates.fas" :verbose nil)
	(load "ac3.fas" :verbose nil)
	(load "gac2001.fas" :verbose nil)
	(load "random-n.fas" :verbose nil)
)

(defun ejecutar-juntos-multiples(nvariables ndominio nrestricciones npares naridad)
	(when (not (genera-psr-n nvariables ndominio nrestricciones npares naridad))
		(return)
	)
	
	(setf *variables-copia* (make-hash-table))
	(maphash #'(lambda (k v) (setf (gethash k *variables-copia*) (make-psr-var :nombre k :dominio (make-array ndominio :initial-contents (loop for i from 1 to ndominio collect i))))) *variables*)
	
	(format t "~%~%Ejemplo aleatorio para GAC3/GAC2001 con aridad ~a~%~%" naridad)	
	(format t "Variables y sus dominios:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	(format t "~%")
	
	(format t "Restricciones:~%")
	(format t "~a~%" *restricciones*)
	(format t "~%")
	
	(format t "Tuplas prohibidas:~%")
	(format t "~a~%" *tuplas-prohibidas*)
	(format t "~%")
		
	(ac-3 *variables*)
	(format t "Después de ejecutado el algoritmo GAC3:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables*)
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "Tiempo de ejecucion: ~F segundos.~%~%" (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
	
	(gac-2001 *variables-copia*)
	(format t "Después de ejecutado el algoritmo GAC2001:~%")
	(maphash #'(lambda (k v) (format t "~a~%" v)) *variables-copia*)
	
	(format t "Arcos procesados: ~a~&" *contador-arcos*)
	(format t "Tiempo de ejecucion: ~F segundos.~%" (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
)
