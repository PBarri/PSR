(defun inicializa ()
	(load "globales.fas" :verbose nil)
	(load "mates.fas" :verbose nil)
	(load "ac3.fas" :verbose nil)
	(load "random-2.fas" :verbose nil)
)

(defun ac3-estadisticas(nvariables ndominio nrestricciones npares)
	(when (not (genera-psr-2 nvariables ndominio nrestricciones npares))
		(return)
	)
	
	(ac-3 *variables*)
	(format t "~a ~a ~a ~a ~a ~F~&" nvariables ndominio nrestricciones npares *contador-arcos* (/ (- *tiempo1* *tiempo0*) internal-time-units-per-second))
)

