;;; Función que calcula el factorial de un número
(defun fact (n)
	(if (= n 0)
		1
		(* n (fact (- n 1)))
	)
)

;;; Función que calcula el número de combinaciones sin repetición
;;; de N elementos tomados de M en M.
(defun C (n m)
	(/ (fact n) (* (fact (- n m)) (fact m)))
)
		
;;; Función que calcula el número de variaciones con repetición 
;;; de N elementos tomados de M en M.
(defun VR (n m)
	(expt n m)
)