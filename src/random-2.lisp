;;; Función que comprueba, dada un asignación de valores en TABLA, si NO es una asignación PROHIBIDA.
;;; TABLA es una tabla de dispersión que asocia cada variable (de tipo entero) con su valor asignado.
;;; Parámetros: TABLA con la asignación de valores
;;; Devuelve: T si se cumple que dicha asignación es válida, NIL en caso contrario
;;; Pseudocódigo:

;;; FUNCION COMPRUEBA-PARES-PROHIBIDOS(TABLA)
;;; 1. Hacer VARIABLES igual a la lista ordenada de todas las variables de la asignación
;;;    Hacer VALORES igual a la lista de todos los valores asignados a esas variables
;;; 2. Devolver la comprobación de que el par (VARIABLES,VALORES) NO está entre los pares
;;;    prohibidos de esas variables 
 
(defun comprueba-pares-prohibidos (tabla)
	(let 
		(
			(alist nil)
		)
		;; maphash itera sobre tabla
		(maphash (lambda (k v) (push (cons k v) alist)) tabla)
		;; < es el orden ascendente #' significa que lo que hay a la derecha es el simbolo de una funcion
		(setq alist (sort alist #'< :key #'car))
		(let
			(
				(variables (loop for par in alist collect (car par)))
				(valores (loop for par in alist collect (cdr par)))
			)
			(not (member valores (gethash variables *pares-prohibidos*) :test #'equal))
		)
	)
)

;;; Función que devuelve las tuplas de números formadas por las combinaciones de NVARIABLES tomadas de 2 en 2
;;; en forma de restricciones.
;;; Parámetros: NVARIABLES es el número de variables del PSR
;;; Devuelve: Modifica *RESTRICCIONES* de forma que contiene todas las posibles restricciones BINARIAS del PSR con NVARIABLES.
;;; Por ejemplo: Si NVARIABLE es 4 se generan todas las posibles restricciones binarias de 4 variables:
;;; * (1 2)
;;; * (1 3)
;;; * (1 4)
;;; * (2 3)
;;; * (2 4)
;;; * (3 4)
;;; Pseudocódigo:

;;; FUNCION CREA-TODAS-RESTRICCIONES(NVARIABLES)
;;; 1. Hacer RESULTADO igual a la lista vacía
;;; 2. Para cada I desde 1 hasta NVARIABLES hacer
;;;    2.1. Para cada J desde I+1 hasta NVARIABLE hacer
;;;         2.1.1. Añadir a RESULTADO la restricción creada con la lista de variables (I J)
;;; 3. Hacer *RESTRICCIONES* igual a RESULTADO  
	
(defun crea-todas-restricciones (nvariables)
	(let 
		(
			(resultado '())
		)
		(loop for i from 1 to nvariables do
			(loop for j from (1+ i) to nvariables do
				(setq resultado (cons (make-psr-restr :variables (list i j) :funcion 'comprueba-pares-prohibidos) resultado))
			)
		)
		(setf *restricciones* resultado)
	)
)

;;; Función que genera un PSR aleatorio con restricciones binarias.
;;; Parámetros: NVARIABLES es el número de variables del PSR
;;;             NDOMINIO es el tamaño del dominio de todas las variables
;;;             NRESTRICCIONES es el número de restricciones del problema
;;;             NPARES-PROHIBIDOS es el número de pares prohibidos de valores para todas la restricciones
;;; Importante: Si hay NVARIABLES, el número máximo de restricciones que puede haber se calcula mediante combinatoria básica como
;;; las combinaciones de NVARIABLES tomadas de dos en dos: C(nvariables,2)
;;; Igualmente si el dominio es NDOMINIO el número de posibles pares prohibidos es VR(ndominio,2) como máximo.
;;; Se hacen en la función las comprobaciones necesarias para verificar esto dando el mensaje oportuno por lo que 
;;; el valor devuelto por la funciónm indica si se han podido verificar estas condiciones.
;;; Devuelve: Modifica *RESTRICCIONES*, *VARIABLES* y *PARES-PROHIBIDOS* de forma 
;;; que contengan un PSR aleatorio con restricciones BINARIAS
;;; Pseudocódigo:

;;; FUNCION GENERA-PSR-2(NVARIABLES NDOMINIO NRESTRICCIONES NPARES-PROHIBIDOS)
;;; 1. Hacer *VARIABLES* igual a la tabla de dispersión vacía
;;; 2. Hacer *PARES-PROHIBIDOS* igual a la tabla de dispersión vacía
;;; 3. Hacer DOMINIO igual a una lista con los valores 1 ... NDOMINIO
;;; 4. Para cada VARIABLE desde 1 hasta NVARIABLES hacer
;;;    4.1. Crear variable con nombre VARIABLE y dominio DOMINIO
;;; 5. Hacer *RESTRICCIONES* igual a (CREAR-TODAS-RESTRICCIONES)
;;; 6. Mientras TAMAÑO(*RESTRICCIONES*) > NRESTRICCIONES hacer
;;;    6.1. Eliminar aleatoriamente una restricción de *RESTRICCIONES*  
;;; 7. Para cada RESTRICCION en *RESTRICCIONES* hacer
;;;    7.1. Hacer LISTA-PARES-PROHIBIDOS igual a la lista con todos los posibles pares
;;;    7.2. Mientras TAMAÑO(LISTA-PARES-PROHIBIDOS) > NPARES-PROHIBIDOS hacer
;;;         7.2.1. Eliminar aleatoriamente un par de LISTA-PARES-PROHIBIDOS
;;;    7.3. Crear par (RESTRICCION,LISTA-PARES-PROHIBIDOS) en *PARES-PROHIBIDOS*

(defun genera-psr-2 (nvariables ndominio nrestricciones npares-prohibidos)
	(let
		(
			(resultado nil)
		)
		(if (or (> nrestricciones (C nvariables 2))
		        (> npares-prohibidos (VR ndominio 2)))
			(format t "Error en el paso de argumentos para la creación del PSR~%")
			(progn 
				(setq resultado T)
				(setf *variables* (make-hash-table))
				(setf *pares-prohibidos* (make-hash-table :test #'equal))
				(loop for variable from 1 to nvariables do
					(setf (gethash variable *variables*) (make-psr-var :nombre variable :dominio (make-array ndominio :initial-contents (loop for i from 1 to ndominio collect i))))
				)
				(crea-todas-restricciones nvariables)
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
							(lista-pares-prohibidos (genera-todas-tuplas ndominio 2 '() '()))
						)
						(loop while (> (length lista-pares-prohibidos) npares-prohibidos) do
							(let
								(
									(position (1+ (random (length lista-pares-prohibidos))))
								)
								(setf lista-pares-prohibidos (append (subseq lista-pares-prohibidos 0 (1- position)) (nthcdr position lista-pares-prohibidos)))
							)
						)
						(setf (gethash (psr-restr-variables restriccion) *pares-prohibidos*) lista-pares-prohibidos)
					)
				)
			)
		)
		resultado
	)
)	

;;; Función que genera todas las tuplas de NARIDAD componentes, cada una de ellas con un rango de números
;;; naturales que va desde 1 hasta NDOMINIO
;;; Parámetros: NDOMINIO es el tamaño del dominio de todas las variables
;;;             NARIDAD es la aridad de las restricciones
;;;             LISTA donde se van almacenando todas las tuplas de valores
;;;             TUPLA lista donde se va almacenando la tupla actual
;;; Pseudocódigo:

;;; FUNCION GENERA-PSR-N(NVARIABLES NDOMINIO NRESTRICCIONES NPARES-PROHIBIDOS NARIDAD)
;;; 1. SI NARIDAD = 0
;;;    1.1 Añadir TUPLA a LISTA
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