# Trabajo Inteligencia Artificial 1: Problemas de Satisfacción de Restricciones


##### 1. Para ejecutar AC3 sobre el ejemplo del robot:

```lisp
CLISP> (load "ejecutar-ac3-binarias.lisp")
CLISP> (inicializa)
CLISP> (robot)
```

##### 2. Para ejecutar AC3 sobre un ejemplo aleatorio con restricciones binarias:

```lisp
CLISP> (load "ejecutar-ac3-binarias.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-ac3-binarias 3 4 2 11)
```

Donde los parámetros son, respectivamente
* Número de variables 
* Tamaño del dominio
* Número de restricciones
* Número de tuplas prohibidas

##### 3. Para ejecutar AC3 sobre un ejemplo aleatorio con restricciones de cualquier aridad:

```lisp
CLISP> (load "ejecutar-ac3-multiples.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-ac3-multiples 5 4 10 60 3)
```

Donde los parámetros son como los anteriores excepto que añadimos el último, que indica la aridad en las restricciones.

##### 4. Si queremos ejecutar una batería de problemas de AC3 aleatorios:

```lisp
CLISP> (load "ac3-estadisticas.lisp")
CLISP> (inicializa)
CLISP> (loop for nr from 0 to 4950 by 247 do (ac3-estadisticas 100 5 nr 1))
```
##### 5. Para ejecutar AC2001 sobre el ejemplo del robot:

```lisp
CLISP> (load "ejecutar-ac2001-binarias.lisp")
CLISP> (inicializa)
CLISP> (robot)
```
##### 6. Para ejecutar AC2001 sobre un ejemplo aleatorio con restricciones binarias:

```lisp
CLISP> (load "ejecutar-ac2001-binarias.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-ac2001-binarias 3 4 2 11)
```

Donde los parámetros son, respectivamente
* Número de variables 
* Tamaño del dominio
* Número de restricciones
* Número de tuplas prohibidas

##### 7. Para ejecutar GAC2001 sobre un ejemplo aleatorio con restricciones de cualquier aridad:

```lisp
CLISP> (load "ejecutar-ac2001-multiples.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-ac2001-multiples 5 4 10 60 3)
```
Donde los parámetros son como los anteriores excepto que añadimos el último, que indica la aridad en las restricciones.

##### 8. Si queremos ejecutar una batería de problemas de AC2001 aleatorios:

```lisp
CLISP> (load "ac2001-estadisticas.lisp")
CLISP> (inicializa)
CLISP> (loop for nr from 0 to 4950 by 247 do (ac2001-estadisticas 100 5 nr 1))
```
##### 9. Si queremos ejecutar el mismo ejemplo con restricciones binarias en AC3/AC2001

```lisp
CLISP> (load "ejecutar-juntos-binarias.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-juntos-binarias 3 4 2 11)
```
##### 10. Si queremos ejecutar el mismo ejemplo con restricciones de cualquier aridad en GAC3/GAC2001

```lisp
CLISP> (load "ejecutar-juntos-multiples.lisp")
CLISP> (inicializa)
CLISP> (ejecutar-juntos-multiples 5 4 10 60 3)
```
