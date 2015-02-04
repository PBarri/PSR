;;; Función que crea todos los arcos de un PSR a partir de la lista de 
;;; restricciones. 
;;; Parámetros: Ninguno.
;;; Devuelve: Lista con todos los arcos creados.
;;; Pseudocódigo:

;;; FUNCION ARCOS2001()
;;; 1. Hacer RESULTADO igual a vacío
;;; 2. Para cada RESTRICCION es *RESTRICCIONES*
;;;    2.1 Para cada VARIABLE en PSR-RESTR-VARIABLES(RESTRICCION)
;;;        Añadir a RESULTADO el arco formado por VARIABLE + RESTRICCION
;;; 3. Devolver RESULTADO

(defun arcos2001 ()
	(let
		(
			(resultado '())
		)
		(loop for restriccion in *restricciones* do
			(loop for variable in (psr-restr-variables restriccion) do
				(push (make-arco :variable variable :restriccion restriccion) resultado)
			)
		)
		resultado
	)
)

;;; Función que, dada una variable y una restricción binaria, devuelve la otra variable
;;; que participa en la restricción. 
;;; Parámetros: VARIABLE: Una de las variables de la RESTRICCION.
;;;             RESTRICCION: La restricción sobre las que estamos operando
;;; Devuelve: La otra variable de la RESTRICCION que no es VARIABLE.
;;; Pseudocódigo:

;;; FUNCION GET-OTRA-VARIABLE(VARIABLE RESTRICCION)
;;; 1. Hacer v1 igual a la primera variable de PSR-RESTR-VARIABLES(RESTRICCION)
;;;    Hacer v2 igual a la segunda variable de PSR-RESTR-VARIABLES(RESTRICCION)
;;; 2. Si VARIABLE = v1
;;;    2.1 Devolver v2
;;;    Si no
;;;    2.2 Devolver v1

(defun get-otra-variable (variable restriccion)
	(let
		(
			(v1 (car (psr-restr-variables restriccion)))
			(v2 (cadr (psr-restr-variables restriccion)))
		)
		(if (equal variable v1)
			v2
			v1
		)
	)
)

;;; Función que devuelve el índice siguiente con elemento distinto de NIL 
;;; a partir del índice i (sin incluir) en un array ordenado de valores
;;; Parámetros: I: Índice a partir del cuál queremos calcular el sucesor.
;;;             DOMINIO: Lista ordenada ascendentemente.
;;; Devuelve: El valor dentro de dominio siguiente a partir de V. 
;;;           Si no existe ese índice devuelve NIL. 
;;;           Si I es NIL debe devolver el primer elemento no NIL del array 
;;; Pseudocódigo:

;;; FUNCION SUCC(I DOMINIO)
;;; 1. Hacer FIN igual a tamaño del array dominio - 1
;;;    Hacer COMIENZO igual a v si v es ditinto de NIL, 0 en otro caso.
;;;    Hacer RESULTADOS igual a NIL
;;; 2. Desde I = COMIENZO hasta FIN
;;;	   2.1 Si DOMINIO[I] es distinto de NIL
;;;        Devolver I
;;; 3. Devolver NIL

(defun succ (v dominio)
	(let 
		(
			(fin (1- (array-dimension dominio 0)))
			(comienzo (if v (1+ v) 0))
			(resultado nil)
		)
		(loop for i from comienzo to fin do
			(when (aref dominio i)
				(setq resultado i)
				(return)
			)
		)
		resultado
	)
)

;;; Función que actualiza el dominio de una variable dependiendo de una restricción (versión AC2001)
;;; Parámetros: 
;;;    * VARIABLE: El nombre de la variable a calcular su nuevo dominio.
;;;    * RESTRICCION: La restriccion sobre la cuál calcular el nuevo dominio.
;;;    * VARIABLES: Lista de variables del PSR   
;;; Devuelve: La lista con los valores del nuevo dominio de la variable
;;; Pseudocódigo:

;;; FUNCION ACTUALIZA-DOMINIO(VARIABLE,RESTRICCION,VARIABLES)
;;; 1. Hacer DOMINIO-ACTUAL igual al dominio de VARIABLE en VARIABLES
;;;    Hacer NUEVO-DOMINIO igual al dominio de VARIABLE en VARIABLES
;;;    Hacer OTRA-VARIABLE igual a GET-OTRA-VARIABLE(VARIABLE, RESTRICCION)
;;; 2. Para cada valor A en DOMINIO-ACTUAL hacer
;;;    2.1 Hacer B el soporte de OTRA-VARIABLE en (VARIABLE, A)
;;;        Hacer DOMINIO-OTRA-VARIABLE igual al dominio de OTRA-VARIABLE en VARIABLES
;;;    2.2 Si B no está en DOMINIO-OTRA-VARIABLE
;;;        2.2.1 Hacer B igual al SUCC(B, DOMINIO-OTRA-VARIABLE)
;;;        2.2.2 MIENTRAS B distinto de NIL Y NO VALOR-CONSISTENTE2001(VARIABLE, OTRA-VARIABLE A B RESTRICCION)
;;;              2.2.2.1 Hacer B igual al SUCC(B, DOMINIO-OTRA-VARIABLE)
;;;        2.2.3 SI B distinto de NIL
;;;				 2.2.3.1 Hacer B último soporte de OTRA-VARIABLE en (VARIABLE, A)
;;;              SI NO
;;;              2.2.3.2 Quitar A de NUEVO-DOMINIO
;;; 3. Devolver NUEVO-DOMINIO

(defun actualiza-dominio2001 (variable restriccion variables)
	(let*
		(
			(dominio-actual (psr-var-dominio (gethash variable variables)))
			(limite (1- (array-dimension dominio-actual 0)))
			(nuevo-dominio (psr-var-dominio (gethash variable variables)))
			(otra-variable (get-otra-variable variable restriccion))
		)
		(setf *delete* nil)
		(loop for i from 0 to limite do
			(let*
				(
					(a (aref dominio-actual i))
					(indiceb (gethash (list variable otra-variable a) *ultimos-soportes*))
					(dominio-otra-variable (psr-var-dominio (gethash otra-variable variables)))
				)
				(when a
					(when (or (not indiceb) (not (aref dominio-otra-variable indiceb)))
						(setq indiceb (succ indiceb dominio-otra-variable))
						(loop while (and indiceb (not (valor-consistente2001 variable otra-variable a (aref dominio-otra-variable indiceb) restriccion))) do
							(setq indiceb (succ indiceb dominio-otra-variable))
						)
						(if indiceb
							(setf (gethash (list variable otra-variable a) *ultimos-soportes*) indiceb)
							(progn
								(setf (aref nuevo-dominio i) nil)
								(setf *delete* t)
							)
						)
					)
				)	
			)
		)
		nuevo-dominio
	)
)

;;; Función que calcula si una pareja de asignaciones (variable, valor)
;;; es un par de valores permitidos para cierta restricción BINARIA.
;;; Parámetros: VI: El nombre de la primera variable.
;;;             VJ: El nombre de la segunda variable.
;;;	            A: El valor que recibe la variable VI.
;;;             B: El valor que recibe la variable VJ.
;;;             RESTRICCION: La restricción cuyo par de valores hay que comprobar. 
;;; Devuelve: T si el par de valores es válido.
;;;           NIL en caso contrario.
;;; Pseudocódigo:

;;; FUNCION VALOR-CONSISTENTE2001(VI VJ A B RESTRICCION)
;;; 1. Hacer TABLA la tabla de dispersión vacía
;;; 2. Añadir a TABLA el par (vi,a) 
;;;	3. Añadir a TABLA el par (vj,b) 
;;; 4. Devolver PSR-RESTR-FUNCION(RESTRICCION, TABLA)

(defun valor-consistente2001 (vi vj a b restriccion)
	(let
		(
			(tabla (make-hash-table))
		)
		(setf *contador-arcos* (1+ *contador-arcos*))
		(setf (gethash vi tabla) a)
		(setf (gethash vj tabla) b)
		(funcall (psr-restr-funcion restriccion) tabla)
	)
)

;;; FUNCION AC-2001(VARIABLES)
;;; 1. Hacer RED igual a ARCOS()
;;; 2. Mientras RED no sea vacío
;;;    2.1 Hacer *ULTIMOS-SOPORTES* igual la tabla de dispersión vacía
;;;    2.2 Hacer ACTUAL el primero de RED y RED el resto de RED
;;;    2.3 Hacer VARIABLE igual a ARCO-VARIABLE(ACTUAL) y
;;;        RESTRICCION igual ARCO-RESTRICCION(ACTUAL)
;;;    2.4 Hacer DOMINIO-ACTUAL igual al dominio
;;;        de VARIABLE en VARIABLES
;;;    2.5 Hacer DOMINIO-NUEVO igual a
;;;        ACTUALIZA-DOMINIO(VARIABLE,RESTRICCION,VARIABLES)
;;;    2.6 Si DOMINIO-NUEVO y DOMINIO-ACTUAL son distintos, entonces
;;;           2.6.1 Actualizar (destructivamente) el dominio de ACTUAL
;;;                 en VARIABLES con NUEVO-DOMINIO
;;;           2.6.2 Incluir en RED todos los arcos de ARCOS() en los
;;;                 que aparezca VARIABLE y ésta NO sea la variable
;;;                 distinguida del arco (*)
;;; 3. Devolver VARIABLES

(defun ac-2001 (variables)
	(setf *contador-arcos* 0)
	(setf *tiempo0* (get-internal-real-time))
	(setf *ultimos-soportes* (make-hash-table :test #'equal))
	(let*
		(
			(red (arcos2001))
			(tabla-auxiliar (variables-arcos2001 red variables))
		)
		(loop while red do
			(let*
				(
					(actual (car red))
					(variable (arco-variable actual))
					(restriccion (arco-restriccion actual))
					(dominio-actual (psr-var-dominio (gethash variable variables)))
					(nuevo-dominio (actualiza-dominio2001 variable restriccion variables))
				)
				(setq red (cdr red))
				(when *delete*
					(setf (psr-var-dominio (gethash variable variables)) nuevo-dominio)
					(setq red (append red (gethash variable tabla-auxiliar)))
				)
			)
		)
		(setf *tiempo1* (get-internal-real-time))
		variables
	)
)

;;; Función que construye una tabla de dispersión donde aparecen como claves
;;; las variables del PSR y como valores la lista de arcos de esa variable en las
;;; que aparece como variable NO distinguida.
;;; Parámetros: 
;;;    * ARCOS: La red de arcos original como lista.
;;;    * VARIABLES-ORIGINALES: La tabla de dispersión con todas las variables originales
;;; Devuelve: Una tabla de dispersión con los pares:
;;;           (variable, lista_de_arcos_a_los_que_afecta)
;;; Pseudocódigo:

;;; FUNCION VARIABLES-ARCOS2001(ARCOS VARIABLES-ORIGINALES)
;;; 1. Hacer RESULTADO igual a MAPHASH()
;;;    Hacer VARIABLES igual a la lista de las claves de VARIABLES-ORIGINALES
;;; 2. Para cada VARIABLE en VARIABLES hacer
;;;    2.1 Para cada ARCO en ARCOS hacer
;;;        2.1.1 Hacer VARIABLE-DISTINGUIDA igual a (ARCO-VARIABLE ARCO)
;;;              Hacer VARIABLES-TODAS igual a las variables de la restricción del ARCO
;;;              HACER VALORES igual a la lista de arcos que ya tiene asignados VARIABLE
;;;        2.1.2 SI NO es igual VARIABLE que VARIABLE-DISTINGUIDA AND VARIABLE pertenece a VARIABLES todas
;;;              2.1.2.1 Añadir a VALORES el arco ARCO
;;; 3 Devolver RESULTADO

(defun variables-arcos2001 (arcos variables-originales)
	(let 
		(
			(resultado (make-hash-table))
			(variables '())
		)
		(maphash #'(lambda (k v) (setf variables (cons k variables))) variables-originales)
		(loop for variable in variables do
			(loop for arco in arcos do
				(let
					(
						(variable-distinguida (arco-variable arco))
						(variables-todas (psr-restr-variables (arco-restriccion arco)))
						(valores (gethash variable resultado))
					)
					(when (and (not (eq variable variable-distinguida))
						       (member variable variables-todas))
						  (if valores
							(setf (gethash variable resultado) (cons arco valores))
							(setf (gethash variable resultado) (list arco))
						  )
					)
				)
			)
		)
		resultado
	)
)


