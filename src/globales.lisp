;;; PSR: Problema de Satisfacción de Restricciones.
;;; Estructura que define una variable en el PSR.
;;; Consta de dos campos:
;;; * nombre: El nombre de la variable. No tipado.
;;; * dominio: Array de posibles valores de la variable en cuestión.
(defstruct (psr-var (:print-function escribe-psr-var))
	nombre
	dominio)

(defun escribe-psr-var (variable &optional (canal t) profundidad)
	(format canal "~a:~a" (psr-var-nombre variable) (loop for i from 0 to (1- (array-dimension (psr-var-dominio variable) 0)) when (aref (psr-var-dominio variable) i) collect (aref (psr-var-dominio variable) i)))
)
	
;;; Estructura que define una restricción en el PSR.
;;; Consta de dos campos:
;;; * variables: Lista de variables que participan en la restricción. 
;;;              Si hay sólo dos variables estaremos ante una restricción binaria.
;;;              Si hay más de dos variables será una restricción múltiple.
;;; * funcion: Símbolo de la función que sirve para comprobar si la restricción se cumple o no
(defstruct (psr-restr (:print-function escribe-psr-restr))
	variables
	funcion)

(defun escribe-psr-restr (restriccion &optional (canal t) profundidad)
	(format canal "~a - ~a" (psr-restr-variables restriccion) (psr-restr-funcion restriccion)))

;;; Estructura que define un arco en el PSR.
;;; Cada arco se compondrá de dos campos:
;;; * variable: El nombre de la variable distinguida del arco.
;;; * restriccion: La restriccion correpondiente al arco representado.
(defstruct (arco (:print-function escribe-arco))
	variable
	restriccion)
	
(defun escribe-arco (arco &optional (canal t) profundidad)
	(format canal "~a - ~a" (arco-variable arco) (arco-restriccion arco)))

;;; Esta variable global almacenará todas las variables del PSR mediante una tabla de dispersión
;;; en la que la clave es el nombre de la variable y el valor la estructura de tipo variable.
;;; Por lo tanto, a través del nombre de la variable podremos acceder al resto de la 
;;; información de dicha variable
(defvar *variables*)
(defvar *variables-copia*)

;;; Variable global que almacena todas las restricciones del PSR en una lista.
(defvar *restricciones*)

;;; Contador de arcos procesados en un problema
(defvar *contador-arcos*)

;;; Variables para medir el tiempo de ejecución
(defvar *tiempo0*)
(defvar *tiempo1*)

;;; Tabla de dispersión global que servirá para guardar los pares de valores prohibidos para cada 
;;; restricción del problema. La clave será la lista de variables y el valor asociado una lista de 
;;; listas donde cada elemento es la tupla de valores correspondientes a las variables de la restricción
;;; que no se admiten en dicha restricción
;;; Ejemplo de asociación:
;;; ((x1 x2),((1 2)(5 4)(6 3)))
;;; Significa que la restricción "restr" con variables x1 y x2 tiene prohibidos los siguientes  
;;; pares de valores:
;;; (x1,x2) = (1,2)
;;; (x1,x2) = (5,4)
;;; (x1,x2) = (6,3)
(defvar *pares-prohibidos*)

;;; Tabla de dispersión global que servirá para guardar las tuplas de valores prohibidos para cada 
;;; restricción del problema. La clave será la lista de variables y el valor asociado una lista de 
;;; listas donde cada elemento es la tupla de valores correspondientes a las variables de la restricción
;;; que no se admiten en dicha restricción
;;; Ejemplo de asociación:
;;; ((x1 x2 x4),((1 2 3) (5 4 3) (6 3 1)))
;;; Significa que la restricción "restr" con variables x1, x2 y x4 tiene prohibidos las siguientes  
;;; tuplas de valores:
;;; (x1,x2,x4) = (1,2,3)
;;; (x1,x2,x4) = (5,4,3)
;;; (x1,x2,x4) = (6,3,1)
(defvar *tuplas-prohibidas*)

;;; Tabla de dispersión donde las claves son (vi vj valor-vi)
;;; y el valor es el soporte de vi en vj.
(defvar *ultimos-soportes*)

;;; Variable global para saber si ha cambiado el dominio o no
(defvar *delete*)
