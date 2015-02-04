;;; Funci�n que comprueba, dada un asignaci�n de valores en TABLA, si NO es una asignaci�n prohibida.
;;; TABLA es una tabla de dispersi�n que asocia cada variable (de tipo entero) con su valor asignado.
;;; Par�metros: TABLA con la asignaci�n de valores
;;; Devuelve: T si se cumple que dicha asignaci�n es v�lida, NIL en caso contrario
;;; Pseudoc�digo:

;;; FUNCION COMPRUEBA-TUPLAS-PROHIBIDAS(TABLA)
;;; 1. Hacer VARIABLES igual a la lista ordenada de todas las variables de la asignaci�n
;;;    Hacer VALORES igual a la lista de todos los valores asignados a esas variables
;;; 2. Devolver la comprobaci�n de que el par (VARIABLES,VALORES) NO est� entre los pares
;;;    prohibidos de esas variables 
 
(defun comprueba-tuplas-prohibidas (tabla)
	(let 
		(
			(alist nil)
		)
		(maphash (lambda (k v) (push (cons k v) alist)) tabla)
		(setq alist (sort alist #'< :key #'car))
		(let
			(
				(variables (loop for par in alist collect (car par)))
				(valores (loop for par in alist collect (cdr par)))
			)
			(not (member valores (gethash variables *tuplas-prohibidas*) :test #'equal))
		)
	)
)

;;; Funci�n que devuelve las tuplas de n�meros formadas por las combinaciones de NVARIABLES tomadas de NARIDAD en NARIDAD
;;; en forma de restricciones.
;;; Par�metros: NVARIABLES es el n�mero de variables del PSR
;;;             NARIDAD es el n�mero de variables que participan en cada restricci�n
;;; Devuelve: Modifica *RESTRICCIONES* de forma que contiene todas las posibles restricciones binarias del PSR con NVARIABLES.
;;; Por ejemplo: Si NVARIABLE es 4 y NARIDAD es 3 se generan todas las posibles restricciones de 4 variables y aridad 3:
;;; * (1 2 3)
;;; * (1 2 4)
;;; * (1 3 4)
;;; * (2 3 4)
;;; Pseudoc�digo:

;;; FUNCION CREA-TODAS-RESTRICCIONES(NVARIABLES NARIDAD)
;;; 1. Hacer RESULTADO igual a la lista vac�a
;;; 2. Para cada LISTA-VARIABLES de todas las posibles combinaciones
;;;    2.1. A�adir a RESULTADO la restricci�n creada con la lista de variables LISTA-VARIABLES
;;; 3. Hacer *RESTRICCIONES* igual a RESULTADO
;;; NOTA: Se usan funciones auxiliares sacadas de:
;;; https://github.com/jaejin/99-lisp/blob/master/ninety-nine-lisp_P26.lisp

(defun n-combination (n) 
	(if (> n 0) 
		(let 
			(
				(child (n-combination (1- n)))
			) 
			(append child (list (list n)) (mapcar (lambda (x) (append x (list n))) child))
		) 
		'((0)) 
	)
) 

(defun index-append-combination (comb lst) 
	(loop for c in comb collect  
		(loop for i in c append (nth i lst))
	)
) 

(defun combination (n lists) 
	(mapcan (lambda (x) (if (= (length x) n) (list x))) 
		(index-append-combination (n-combination (1- (length lists))) 
			(mapcar (lambda (x) (list x)) lists) 
	    ) 
	)
) 

(defun crea-todas-restricciones (nvariables naridad)
	(let 
		(
			(resultado '())
		)
		(loop for lista-variables in (combination naridad (loop for i from 1 to nvariables collect i)) do
			(setq resultado (cons (make-psr-restr :variables lista-variables :funcion 'comprueba-tuplas-prohibidas) resultado))
		)
		(setf *restricciones* resultado)
	)
)

;;; Funci�n que genera un PSR aleatorio con restricciones m�ltiples.
;;; Par�metros: NVARIABLES es el n�mero de variables del PSR
;;;             NDOMINIO es el tama�o del dominio de todas las variables
;;;             NRESTRICCIONES es el n�mero de restricciones del problema
;;;             NTUPLAS-PROHIBIDAS es el n�mero de pares prohibidos de valores para todas la restricciones
;;;             NARIDAD es la aridad de las restricciones
;;; Importante: Si hay NVARIABLES, el n�mero m�ximo de restricciones que puede haber se calcula mediante combinatoria b�sica como
;;; las combinaciones de NVARIABLES tomadas de naridad en naridad: C(nvariables,naridad)
;;; Igualmente si el dominio es NDOMINIO el n�mero de posibles tuplas prohibidas es VR(ndominio,naridad) como m�ximo.
;;; La �ltima comprobaci�n controla que la aridad no sea mayor que el n�mero de variables.
;;; Devuelve: Modifica *RESTRICCIONES*, *VARIABLES* y *TUPLAS-PROHIBIDAS* de forma 
;;; que contengan un PSR aleatorio con restricciones de aridad M�LTIPLE.
;;; Pseudoc�digo:

;;; FUNCION GENERA-PSR-N(NVARIABLES NDOMINIO NRESTRICCIONES NPARES-PROHIBIDOS NARIDAD)
;;; 1. Hacer *VARIABLES* igual a la tabla de dispersi�n vac�a
;;; 2. Hacer *TUPLAS-PROHIBIDAS* igual a la tabla de dispersi�n vac�a
;;; 3. Hacer DOMINIO igual a una lista con los valores 1 ... NDOMINIO
;;; 4. Para cada VARIABLE desde 1 hasta NVARIABLES hacer
;;;    4.1. Crear variable con nombre VARIABLE y dominio DOMINIO
;;; 5. Hacer *RESTRICCIONES* igual a (CREAR-TODAS-RESTRICCIONES)
;;; 6. Mientras TAMA�O(*RESTRICCIONES*) > NRESTRICCIONES hacer
;;;    6.1. Eliminar aleatoriamente una restricci�n de *RESTRICCIONES*  
;;; 7. Para cada RESTRICCION en *RESTRICCIONES* hacer
;;;    7.1. Hacer LISTA-TUPLAS-PROHIBIDAS igual a la lista con todas las posibles tuplas
;;;    7.2. Mientras TAMA�O(LISTA-TUPLAS-PROHIBIDAS) > NTUPLAS-PROHIBIDAS hacer
;;;         7.2.2. eliminar aleatoriamente un elemento de la lista LISTA-TUPLAS-PROHIBIDAS
;;;    7.3. Crear par (RESTRICCION,LISTA-TUPLAS-PROHIBIDAS) en *TUPLAS-PROHIBIDAS*

(defun genera-psr-n (nvariables ndominio nrestricciones ntuplas-prohibidas naridad)
	(let
		(
			(resultado nil)
		)
		(if (or (> naridad nvariables)
			    (> nrestricciones (C nvariables naridad))
		        (> ntuplas-prohibidas (VR ndominio naridad)))
			(format t "Error en el paso de argumentos para la creaci�n del PSR~%")
			(progn 
				(setq resultado T)
				(setf *variables* (make-hash-table))
				(setf *tuplas-prohibidas* (make-hash-table :test #'equal))
				(let 
					(
						(dominio (loop for i from 1 to ndominio collect i))
					)
					(loop for variable from 1 to nvariables do
						(setf (gethash variable *variables*) (make-psr-var :nombre variable :dominio (make-array ndominio :initial-contents (loop for i from 1 to ndominio collect i))))
					)
					(crea-todas-restricciones nvariables naridad)
					(loop while (> (length *restricciones*) nrestricciones) do
						(let
							(
								(position (1+ (random (length *restricciones*))))
							)
							(setf *restricciones* (append (subseq *restricciones* 0 (1- position)) (nthcdr position *restricciones*)))
						)
					)
					(loop for restriccion in *restricciones* do
						(let 
							(
								(lista-pares-prohibidos (genera-todas-tuplas ndominio naridad '() '()))
							)
							(loop while (> (length lista-pares-prohibidos) ntuplas-prohibidas) do
								(let
									(
										(position (1+ (random (length lista-pares-prohibidos))))
									)
									(setf lista-pares-prohibidos (append (subseq lista-pares-prohibidos 0 (1- position)) (nthcdr position lista-pares-prohibidos)))
								)
							)
							(setf (gethash (psr-restr-variables restriccion) *tuplas-prohibidas*) lista-pares-prohibidos)
						)
					)
				)
			)
		)
		resultado
	)
)	

;;; Funci�n que genera todas las tuplas de NARIDAD componentes, cada una de ellas con un rango de n�meros
;;; naturales que va desde 1 hasta NDOMINIO
;;; Par�metros: NDOMINIO es el tama�o del dominio de todas las variables
;;;             NARIDAD es la aridad de las restricciones
;;;             LISTA donde se van almacenando todas las tuplas de valores
;;;             TUPLA lista donde se va almacenando la tupla actual
;;; Pseudoc�digo:

;;; FUNCION GENERA-PSR-N(NVARIABLES NDOMINIO NRESTRICCIONES NPARES-PROHIBIDOS NARIDAD)
;;; 1. SI NARIDAD = 0
;;;    1.1 A�adir TUPLA a LISTA
;;;    1.2 Devolver LISTA
;;; 2. SINO
;;;    2.1 Desde I = 1 hasta NDOMINIO
;;;    2.2 Hacer LISTA igual a GENERA-TODAS-TUPLAS(NDOMINIO NARIDAD-1 LISTA (CONS I TUPLA))
;;;    2.3 Devolver LISTA

(defun genera-todas-tuplas(ndominio naridad lista tupla)
	(if (= naridad 0)
		(progn 
			(setq lista (cons tupla lista))
			lista
		)
		(progn
			(loop for i from 1 to ndominio do
				(setq lista (genera-todas-tuplas ndominio (1- naridad) lista (cons i tupla)))
			)
			lista
		)
	)
)