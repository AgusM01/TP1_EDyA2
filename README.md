# Trabajo práctico 1 - Estructuras de Datos y Algoritmos 2.
Para almacenar un conjunto de puntos de un espacio métrico de n dimensiones, suele ser  útil una estructura
de datos que organiza los puntos del espacio en un  árbol binario, el cuál puede pensarse como una generalización
de los  árboles binarios de búsqueda.

Este tipo particular de  árboles emplea planos perpendiculares a uno de los ejes del sistema de coordenadas, de la
siguiente manera, cada nodo del  árbol almacena un punto p de dimensión n y un eje e, de manera que el hiperplano
que pasa por p y es perpendicular al eje e divide al espacio en dos semiespacios y los puntos que se encuentran a
un lado de este hiperplano (con la componente correspodiente al eje de coordenada e menor o igual al de p) son
representados por el subárbol izquierdo, y los que se encuentran del otro lado del hiperplano por el subárbol derecho.

Este trabajo práctico consta de funciones útiles para trabajar con este tipo de árboles tales como:
- Crear un árbol a partir de una lista de puntos dada (todos de la misma dimensión)
- Insertar un punto en un árbol (el punto debe ser de la misma dimensión que los demás puntos del árbol)
- Eliminar un nodo de un árbol.
- Dados dos puntos de 2 dimensiones, determinar si un punto de dos dimensiones dado forma parte del rectángulo que forman estos dos puntos.
- Dados dos puntos de 2 dimensiones, determinar si un conjunto de puntos de dos dimensiones forman parte del rectángulo formado por estos dos puntos.
