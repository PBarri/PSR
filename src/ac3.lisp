;;; Funci�n que crea todos los arcos de un PSR a partir de la lista de 
;;; restricciones. 
;;; Par�metros: Ninguno.
;;; Devuelve: Lista con todos los arcos creados.
;;; Pseudoc�digo:

;;; FUNCION ARCOS()
;;; 1. Hacer RESULTADO igual a vac�o
;;; 2. Para cada RESTRICCION es *RESTRICCIONES*
;;;    2.1 Para cada VARIABLE en PSR-RESTR-VARIABLES(RESTRICCION)
;;;        A�adir a RESULTADO el arco formado por VARIABLE + RESTRICCION
;;; 3. Devolver RESULTADO

(defun arcos ()
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

;;; Funci�n que actualiza el dominio de una variable dependiendo de una restricci�n
;;; Par�metros: 
;;;    * VARIABLE: El nombre de la variable a calcular su nuevo dominio.
;;;    * RESTRICCION: La restriccion sobre la cu�l calcular el nuevo dominio.
;;;    * VARIABLES: Lista de variables del PSR   
;;; Devuelve: La lista con los valores del nuevo dominio de la variable
;;; Pseudoc�digo:

;;; FUNCION ACTUALIZA-DOMINIO(VARIABLE,RESTRICCION,VARIABLES)
;;; 1. Hacer DOMINIO-ACTUAL igual al dominio de VARIABLE en VARIABLES
;;;    Hacer NUEVO-DOMINIO igual a vac�o
;;; 2. Para cada VALOR en DOMINIO-ACTUAL hacer
;;;    2.1 Si para ese VALOR de VARIABLE existe al menos una asignaci�n a las
;;;        restantes variables de RESTRICCION que satisfaga RESTRICCION,
;;;        incluir VALOR en NUEVO-DOMINIO
;;; 3. Devolver NUEVO-DOMINIO

(defun actualiza-dominio (variable restriccion variables)
	(let*
		(
			(dominio-actual (psr-var-dominio (gethash variable variables)))
			(nuevo-dominio (make-array (array-dimension dominio-actual 0) :initial-element nil))
			(limite (1- (array-dimension dominio-actual 0)))
		)
		(setf *delete* nil)
		(loop for i from 0 to limite do
			(let
				(
					(valor (aref dominio-actual i))
				)
				(when valor
					(if (valor-consistente valor variable restriccion variables (psr-restr-variables restriccion) (make-hash-table))
						(setf (aref nuevo-dominio i) valor)
						(setf *delete* t)
					)
				)
			)
		)
		nuevo-dominio
	)
)

;;; Funci�n AC3 implementada como est� en los apuntes de la asignatura.
;;; Curiosamente ya sirve para restricciones m�ltiples.
;;; Par�metros: 
;;;    * VARIABLES: Las variables a tratar en formato tabla de dispersi�n.
;;;                 Ve�se la explicaci�n en la definici�n de la variable global 
;;;                 *variables* en el fichero "globales.lisp"
;;; Devuelve: Las variables con los dominios modificados para que sean arco-consistentes.
;;; Pseudoc�digo:

;;; FUNCION AC-3(VARIABLES)
;;; 1. Hacer RED igual a ARCOS()
;;; 2. Mientras RED no sea vac�o
;;;    2.1 Hacer ACTUAL el primero de RED y RED el resto de RED
;;;    2.2 Hacer VARIABLE igual a ARCO-VARIABLE(ACTUAL) y
;;;        RESTRICCION igual ARCO-RESTRICCION(ACTUAL)
;;;    2.3 Hacer DOMINIO-ACTUAL igual al dominio
;;;        de VARIABLE en VARIABLES
;;;    2.4 Hacer DOMINIO-NUEVO igual a
;;;        ACTUALIZA-DOMINIO(VARIABLE,RESTRICCION,VARIABLES)
;;;    2.5 Si DOMINIO-NUEVO y DOMINIO-ACTUAL son distintos, entonces
;;;           2.5.1 Actualizar (destructivamente) el dominio de ACTUAL
;;;                 en VARIABLES con NUEVO-DOMINIO
;;;           2.5.2 Incluir en RED todos los arcos de ARCOS() en los
;;;                 que aparezca VARIABLE y �sta NO sea la variable
;;;                 distinguida del arco (*)
;;; 3. Devolver VARIABLES

(defun ac-3 (variables)
	(setf *contador-arcos* 0)
	(setf *tiempo0* (get-internal-real-time))
	(let*
		(
			(red (arcos))
			(tabla-auxiliar (variables-arcos red variables))
		)
		(loop while red do
			(let*
				(
					(actual (car red))
					(variable (arco-variable actual))
					(restriccion (arco-restriccion actual))
					(dominio-actual (psr-var-dominio (gethash variable variables)))
					(nuevo-dominio (actualiza-dominio variable restriccion variables))
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

;;; Funci�n que construye una tabla de dispersi�n donde aparecen como claves
;;; las variables del PSR y como valores la lista de arcos de esa variable en las
;;; que aparece como variable NO distinguida.
;;; Par�metros: 
;;;    * ARCOS: La red de arcos original como lista.
;;;    * VARIABLES-ORIGINALES: La tabla de dispersi�n con todas las variables originales
;;; Devuelve: Una tabla de dispersi�n con los pares:
;;;           (variable, lista_de_arcos_a_los_que_afecta)
;;; Pseudoc�digo:

;;; FUNCION VARIABLES-ARCOS(ARCOS VARIABLES-ORIGINALES)
;;; 1. Hacer RESULTADO igual a MAPHASH()
;;;    Hacer VARIABLES igual a la lista de las claves de VARIABLES-ORIGINALES
;;; 2. Para cada VARIABLE en VARIABLES hacer
;;;    2.1 Para cada ARCO en ARCOS hacer
;;;        2.1.1 Hacer VARIABLE-DISTINGUIDA igual a (ARCO-VARIABLE ARCO)
;;;              Hacer VARIABLES-TODAS igual a las variables de la restricci�n del ARCO
;;;              HACER VALORES igual a la lista de arcos que ya tiene asignados VARIABLE
;;;        2.1.2 SI NO es igual VARIABLE que VARIABLE-DISTINGUIDA AND VARIABLE pertenece a VARIABLES todas
;;;              2.1.2.1 A�adir a VALORES el arco ARCO
;;; 3 Devolver RESULTADO

(defun variables-arcos (arcos variables-originales)
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

;;; Funci�n que genera todas las tuplas posibles de una serie de valores asignados a variables
;;; y comprueba que dichos valores cumplen cierta restricci�n. Si existe una tupla que cumple
;;; esto �ltimo el algoritmo acaba devolviendo T. Para ello se van a explorar todos los posibles valores
;;; de todos los dominios excepto de una variable especial a la que s�lo le asignamos un valor 
;;; pasado como argumento.
;;; Par�metros: 
;;;    * VALOR: El valor de la variable especial
;;;    * VARIABLE: La variable especial
;;;    * RESTRICCION: La restricci�n que hay que comprobar
;;;    * VARIABLES: Variables originales con sus respectivos dominios
;;;    * OTRAS-VARIABLES: Lista de variables que a�n quedan por asignarles un valor 
;;;    * TABLA: Tabla de dispersi�n con las asociaciones realizadas (variable,valor)
;;; Devuelve: T si se encontr� una assignaci�n consistente con RESTRICCION, NIL en caso contrario
;;;           (variable, lista_de_arcos_a_los_que_afecta)
;;; Pseudoc�digo:

;;; FUNCION VALOR-CONSISTENTE(VALOR VARIABLE RESTRICCION VARIABLES OTRAS-VARIABLES TABLA)
;;; 1. SI OTRAS-VARIABLES est� vac�o
;;;    1.1 Devolver la comprobaci�n de que RESTRICCI�N se cumple o no.
;;; 2. SINO
;;;    2.1 HACER RESULTADO igual a NIL
;;;		   VARIABLE-ACTUAL la primera variable de OTRAS-VARIABLES
;;;    2.2 SI es igual VARIABLE que VARIABLE-ACTUAL
;;;        2.2.1 A�adir a TABLA el par (VARIABLE,VALOR)
;;;        2.2.2 HACER RESULTADO igual a VALOR-CONSISTENTE(VALOR VARIABLE RESTRICCION VARIABLES (RESTO DE OTRAS-VARIABLES) TABLA)
;;;    2.3 SINO
;;;        2.3.1 Para cada VALOR-ACTUAL en el dominio de VARIABLE-ACTUAL hacer
;;;              2.3.1.1 A�adir a TABLA el par (VARIABLE-ACTUAL,VALOR-ACTUAL)
;;;              2.3.1.2 HACER RESULTADO igual a VALOR-CONSISTENTE(VALOR VARIABLE RESTRICCION VARIABLES (RESTO DE OTRAS-VARIABLES) TABLA)
;;;              2.3.1.3 SI RESULTADO es T Devolver RESULTADO
;;;		   2.3.2 Devolver RESULTADO

(defun valor-consistente (valor variable restriccion variables otras-variables tabla)
	(if (endp otras-variables)
		(progn
			(setf *contador-arcos* (1+ *contador-arcos*))
			(funcall (psr-restr-funcion restriccion) tabla)
		)
		(let* 
			(
				(variable-actual (car otras-variables))
				(dominio-actual (psr-var-dominio (gethash variable-actual variables)))
				(limite (1- (array-dimension dominio-actual 0)))
				(resultado nil)
			)
			(if (equal variable variable-actual)
				(progn
					(setf (gethash variable tabla) valor)
					(setq resultado (valor-consistente valor variable restriccion variables (cdr otras-variables) tabla))
				)
				(progn
					(loop for i from 0 to limite do
						(let
							(
								(valor-actual (aref dominio-actual i))
							)
					;;(loop for valor-actual in (psr-var-dominio (gethash variable-actual variables)) do
							(when valor-actual
								(setf (gethash variable-actual tabla) valor-actual)
								(setq resultado (valor-consistente valor variable restriccion variables (cdr otras-variables) tabla))
								(when resultado
									(return)
								)
							)
						)
					)
				)
			)
			resultado
		)			
	)
)
