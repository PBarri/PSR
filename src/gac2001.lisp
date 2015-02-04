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

;;; Función que devuelve, dada una tupla de valores (lista de valores) la siguiente tupla
;;; en orden lexicográfico inverso. Por ejemplo, si la tupla de partida es (1 2 3) la siguiente tupla
;;; sería (2 2 3). Si establecemos el máximo para cada valor en 4, las siguientes tuplas a partir
;;; de esta última serían (3 2 3) -> (4 2 3) -> (1 3 3) -> ...
;;; Parámetros: MAXIMO: Maximo valor para cada valor en la tupla.
;;;             TUPLA: Tupla original.
;;;             N: Número de componentes de la tupla a devolver.
;;; Devuelve: La siguiente tupla de TUPLA en orden lexicográfico.
;;;           Si hemos llegado a la última tupla hay que devolver NIL. 
;;;           Si TUPLA es NIL debe devolver la primera tupla (1 1 1)
;;; Pseudocódigo:

;;; 1. Si TUPLA es distinto de NIL
;;;    1.1 Si TUPLA = (MAXIMO MAXIMO ... MAXIMO)
;;;        1.1.1 Devolver NIL
;;;    1.2 Si no
;;;		   1.2.1 Hacer PRIMER-ELEMENTO igual al primer elemento de TUPLA
;;;        1.2.2 Si PRIMER-ELEMENTO < MAXIMO
;;;              1.2.2.1 Devolver (CONS PRIMER-ELEMENTO+1 (CDR TUPLA))
;;;        1.2.3 Si no
;;;              1.2.3.1 Si N = 0
;;;                      1.2.3.1.1 Devolver NIL
;;;              1.2.3.2 Si no      
;;;                      1.2.3.2.1 Devolver (CONS 1 SIGUIENTE-TUPLA(MAXIMO, (CDR TUPLA) (1- N)))
;;; 2. Si no
;;;    2.1 Devolver (1 1 ... 1) 

(defun siguiente-tupla (maximo tupla n)
	(if tupla
		(if (equal tupla (loop for i from 1 to n collect maximo))
			nil
			(let
				(
					(primer-elemento (car tupla))
				)
				(if (< primer-elemento maximo)
					(cons (1+ primer-elemento) (cdr tupla))
					(if (= n 0)
						nil
						(cons 1 (siguiente-tupla maximo (cdr tupla) (1- n)))
					)
				)
			)
		)
		(loop for i from 1 to n collect 1)
	)
)

;;; Función que devuelve, dada una tupla, la tupla sucesora en orden lexicográfico.
;;; Sólo se permiten aquellas tuplas cuyos correspondientes valores sean válidos en los dominios
;;; de las variables implicadas en una restricción dada. Además, una determinada variable debe tener el valor
;;; pasado como argumento.
;;; Parámetros: TUPLA: Tupla original.
;;;             VARIABLES: El conjunto de variables.
;;;             VARIABLE-ACTUAL: La variable a la que se le impone un valor.
;;;             VALOR-ACTUAL: El valor que debe tomar la variable actual.
;;;             RESTRICCION: La restricción que se está comprobando.
;;; Devuelve: La siguiente tupla que cumpla las condiciones descritas más arriba o NIL 
;;;           si no se encuentra dicha tupla.
;;; Pseudocódigo:

;;; FUNCION SUCC(TUPLA, VARIABLES, VARIABLE-ACTUAL, VALOR-ACTUAL, RESTRICCION)
;;; 1. Hacer DOMINIO igual al dominio de la VARIABLE-ACTUAL
;;;    Hacer NDOMINIO el número máximo de valores de DOMINIO
;;;    Hacer NARIDAD la aridad de la restricción
;;;    Hacer POSICION-VARIABLE-ACTUAL igual a la posición de la VARIABLE-ACTUAL en la lista de variables de la restricción
;;;    Hacer NUEVA-TUPLA igual a SIGUIENTE-TUPLA(NDOMINIO, TUPLA, NARIDAD)
;;;    Hacer FIN igual a NIL
;;; 2. Mientras NUEVA-TUPLA distinta de NIL y NO FIN
;;;    2.1 Si COMPRUEBA-DOMINIO(NUEVA-TUPLA,VARIABLES,RESTRICCION) Y
;;;           VALOR-ACTUAL = Valor del dominio de VARIABLE-ACTUAL apuntado por NUEVA-TUPLA
;;;        Hacer FIN igual a T
;;;    2.2 Si no
;;;        Hacer NUEVA-TUPLA igual a SIGUIENTE-TUPLA(NDOMINIO, TUPLA, NARIDAD)
;;; 3. Devolver NUEVA-TUPLA


;; La restriccion solamente se le pasa a esta funcion para saber las variables que participan.
;; Se comprueba si los valores existen en el dominio
;; La consistencia o no de una asignacion parcial se comprueba en la funcion que llama a succ
(defun succ (tupla variables variable-actual valor-actual restriccion)
	(let* 
		(
			(dominio (psr-var-dominio (gethash variable-actual variables)))
			(ndominio (array-dimension dominio 0))
			(naridad (length (psr-restr-variables restriccion)))
			(posicion-variable-actual (- naridad (length (member variable-actual (psr-restr-variables restriccion)))))
			(nueva-tupla (siguiente-tupla ndominio tupla naridad))
			(fin nil)
		)
		
		(loop while (and nueva-tupla (not fin)) do
			(if (and (comprueba-dominio nueva-tupla variables restriccion)
			         (= valor-actual (aref dominio (1- (nth posicion-variable-actual nueva-tupla)))))
				(progn
					(setq fin t)
				)
				(progn
					(setq nueva-tupla (siguiente-tupla ndominio nueva-tupla naridad))
				)
			)
		)
		nueva-tupla
	)
)

;;; Función que comprueba, dada una tupla, si todos los valores apuntados por los índices
;;; de esa tupla pertenecen al dominio de cada una delas correspondientes variables que 
;;; participan en cierta restricción.
;;; Parámetros: TUPLA: Tupla original.
;;;             VARIABLES: El conjunto de variables.
;;;             RESTRICCION: La restricción que se está comprobando.
;;; Devuelve: T si se cumple la condición descrita arriba, NIL en caso contrario.
;;; Pseudocódigo:

;;; FUNCION COMPRUEBA-DOMINIO(TUPLA VARIABLES RESTRICCION)
;;; 1. Hacer LISTA-VARIABLES igual a las variables de la RESTRICCION
;;;    Hacer RESULTADO igual a T
;;;    Hacer INDICE-EN-TUPLA igual a 0
;;; 2. Si TUPLA es distinta de NIL
;;;    2.1 Para cada VARIABLE en LISTA-VARIABLES hacer
;;;        2.1.1 Hacer DOMINIO igual al dominio de VARIABLE
;;;              Hacer INDICE-EN-DOMINIO igual al elemento INDICE-EN-TUPLA de TUPLA - 1
;;;              Hacer RESULTADO igual al valor del DOMINIO en INDICE-DOMINIO
;;;        2.1.2 Si RESULTADO es NIL
;;;              Devolver RESULTADO
;;;        2.1.3 Incrementar INDICE-EN-TUPLA
;;; 3. Devolver RESULTADO

(defun comprueba-dominio (tupla variables restriccion)
	(let
		(
			(lista-variables (psr-restr-variables restriccion))
			(resultado t)
			(indice-en-tupla 0)
		)
		(when tupla
			(loop for variable in lista-variables do
				(let* 
					(
						(dominio (psr-var-dominio (gethash variable variables)))
						(indice-en-dominio (1- (nth indice-en-tupla tupla)))
					)
					(setq resultado (aref dominio indice-en-dominio))
					(when (not resultado)
						(return)
					)
					(setq indice-en-tupla (1+ indice-en-tupla))
				)
			)
		)
		resultado
	)
)

;;; Función que comprueba, dada una tupla, si todos los valores apuntados por los índices
;;; de esa tupla están o no prohibidos para una restricción dada.
;;; Parámetros: TUPLA: Tupla original.
;;;             VARIABLES: El conjunto de variables.
;;;             RESTRICCION: La restricción que se está comprobando.
;;; Devuelve: T si se cumple la condición descrita arriba, NIL en caso contrario.
;;; Pseudocódigo:

;;; FUNCION VALOR-CONSISTENTE2001(TUPLA VARIABLES RESTRICCION)
;;; 1. Hacer TABLA igual a la tabla de dispersión vacía
;;;    Hacer INDICE-EN-TUPLA igual a 0
;;; 2. Para cada VARIABLE en las variables de RESTRICCION hacer
;;;    2.1 Hacer DOMINIO igual al dominio de VARIABLE
;;;        Hacer INDICE-EN-DOMINIO igual al elemento INDICE-EN-TUPLA de TUPLA - 1
;;;        Hacer VALOR igual al valor del DOMINIO en INDICE-DOMINIO
;;;    2.2 Introducir en TABLA la asociación (VARIABLE,VALOR)
;;;    2.3 Incrementar INDICE-EN-TUPLA
;;; 3. Devolver la llamada a la función descrita en la RESTRICCION con TABLA

(defun valor-consistente2001 (tupla variables restriccion)
	(let 
		(
			(tabla (make-hash-table))
			(indice-en-tupla 0)
		)
		(loop for variable in (psr-restr-variables restriccion) do
			(let* 
				(
					(dominio (psr-var-dominio (gethash variable variables)))
					(indice-en-dominio (1- (nth indice-en-tupla tupla)))
					(valor (aref dominio indice-en-dominio))
				)
				(setf (gethash variable tabla) valor)
				(setq indice-en-tupla (1+ indice-en-tupla))
			)
		)
		(setf *contador-arcos* (1+ *contador-arcos*))
		(funcall (psr-restr-funcion restriccion) tabla)
	)
)

;;; Función que actualiza el dominio de una variable dependiendo de una restricción (versión GAC2001)
;;; Parámetros: 
;;;    * VARIABLE: El nombre de la variable a calcular su nuevo dominio.
;;;    * RESTRICCION: La restriccion sobre la cuál calcular el nuevo dominio.
;;;    * VARIABLES: Lista de variables del PSR   
;;; Devuelve: El nuevo dominio revisado del VARIABLE
;;; Pseudocódigo:

;;; FUNCION ACTUALIZA-DOMINIO(VARIABLE,RESTRICCION,VARIABLES)
;;; 1. Hacer DOMINIO-ACTUAL igual al dominio de VARIABLE en VARIABLES
;;;    Hacer NUEVO-DOMINIO igual al dominio de VARIABLE en VARIABLES
;;;    Hacer LIMITE igual al tamaño del DOMINIO-ACTUAL - 1
;;; 2. Desde I = 0 hasta LIMITE hacer
;;;    2.1 Hacer A el valor I del DOMINIO-ACTUAL
;;;        Hacer TUPLA-SOPORTE el valor anterior de la tupla soporte para la clave (VARIABLE, RESTRICCION, A) 
;;;    2.2 Si A distinto de NIL Y (TUPLA-SOPORTE distinta de NIL O NO COMPRUEBA-DOMINIO(TUPLA-SOPORTE,VARIABLES,RESTRICCION))
;;;        2.2.1 Hacer TUPLA-SOPORTE igual al SUCC(TUPLA-SOPORTE, VARIABLES, VARIABLE, A, RESTRICCION)
;;;        2.2.2 MIENTRAS TUPLA-SOPORTE distinto de NIL Y NO VALOR-CONSISTENTE2001(TUPLA-SOPORTE, VARIABLES, RESTRICCION)
;;;              2.2.2.1 Hacer TUPLA-SOPORTE igual al SUCC(TUPLA-SOPORTE, VARIABLES, VARIABLE, A, RESTRICCION)
;;;        2.2.3 SI TUPLA-SOPORTE distinto de NIL
;;;				 2.2.3.1 Hacer TUPLA-SOPORTE último soporte de la clave (VARIABLE, RESTRICCION, A)
;;;              SI NO
;;;              2.2.3.2 Quitar A de NUEVO-DOMINIO
;;; 3. Devolver NUEVO-DOMINIO

(defun actualiza-dominio2001 (variable restriccion variables)
	(let*
		(
			(dominio-actual (psr-var-dominio (gethash variable variables)))
			(limite (1- (array-dimension dominio-actual 0)))
			(nuevo-dominio (psr-var-dominio (gethash variable variables)))
		)
		(setf *delete* nil)
		(loop for i from 0 to limite do
			(let*
				(
					(a (aref dominio-actual i))
					(tupla-soporte (gethash (list variable restriccion a) *ultimos-soportes*))
				)
				(when a
					(when (or (not tupla-soporte) (not (comprueba-dominio tupla-soporte variables restriccion)))
						(setq tupla-soporte (succ tupla-soporte variables variable a restriccion))
						(loop while (and tupla-soporte (not (valor-consistente2001 tupla-soporte variables restriccion))) do
							(setq tupla-soporte (succ tupla-soporte variables variable a restriccion))
						)
						(if tupla-soporte
							(setf (gethash (list variable restriccion a) *ultimos-soportes*) tupla-soporte)
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

(defun gac-2001 (variables)
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


