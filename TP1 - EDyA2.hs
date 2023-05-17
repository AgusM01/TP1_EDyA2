--Estructuras de datos y algoritmos II
--Trabajo Práctico N°1. Binner Martín y Merino Agustín


--Estructura del árbol de puntos
data NdTree p = Node (NdTree p) -- sub ́arbol izquierdo
                p -- punto
                (NdTree p) -- sub ́arbol derecho
                Int -- eje
                |Empty 
  deriving (Eq, Ord, Show) 

--Ejercicio 1.

class Punto p where
 dimension :: p -> Int -- devuelve el n ́umero de coordenadas de un punto
 coord :: Int -> p -> Double -- devuelve la coordenada k- ́esima de un punto (comenzando de 0)
 dist :: p -> p -> Double -- calcula la distancia entre dos puntos

--a)
--distN toma dos puntos y devuelve la distancia entre ellos.
distN :: Punto p => p -> p -> Double
distN p q = sqrt(sumCuadrados p q (dimension p - 1)) 

--sumCuadrados toma dos puntos y un Int y devuelve la suma de sus componenetes al cuadrado.
--Esta función es auxiliar de "distN", en ella se le pasa como parametro la dimension del 
--punto y recursivamente se calcula la suma hasta llegar a la primera coordenada.
sumCuadrados :: Punto p => p -> p -> Int -> Double
sumCuadrados p q 0 = (coord 0 p + coord 0 q) ^ 2
sumCuadrados p q n = sumCuadrados p q (n - 1) + ((coord n p + coord n q) ^ 2)

--b)
newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double)

--Punto de dos dimensiones
instance Punto Punto2d where
  dimension _ = 2

  coord 0 (P2d (x,_)) = x
  coord 1 (P2d (_,y)) = y

  dist = distN

--Punto de tres dimensiones
instance Punto Punto3d where
  dimension _ = 3

  coord 0 (P3d (x,_,_)) = x
  coord 1 (P3d (_,y,_)) = y
  coord 2 (P3d (_,_,z)) = z

  dist = distN 

--Ejercicio 2.

--fromList toma una lista de puntos y devuelve un arbol de puntos.  
fromList :: (Punto p, Eq p) => [p] -> NdTree p
fromList [] = Empty
fromList ps = fromList' ps 0

--fromList' toma una lista de puntos el nivel del arbol y devuelve un arbol de puntos.  
--Si la lista es vacia devuelve, el arbol vacio
--Sino, calcula la mediana de los puntos, toma dicho punto como nodo raiz y repite el proceso
--  recursivamente en el subarbol derecho y izquierdo.
fromList' :: (Punto p, Eq p) => [p] -> Int -> NdTree p
fromList' [] _ = Empty
fromList' ps level | null ps = Node Empty (head ps) Empty (mod level (dimension (head ps))) 
                   | otherwise = let
                                 dim = mod level (dimension (head ps))
                                 (menores, med, mayores) = mediana ps dim
                                 in Node (fromList' menores (level + 1)) med (fromList' mayores (level + 1)) dim

--msort toma una lista de puntos y su dimension y devuelve la lista de puntos ordenada
msort :: Punto p => [p] -> Int -> [p]
msort [] _ = []
msort [p] _ = [p]
msort ps dim = let 
               (ls, rs) = split ps 
               in
               merge (msort ls dim, msort rs dim) dim

--split toma una lista de puntos y devuelve una tupla de listas de puntos
split :: Punto p => [p] -> ([p], [p])
split [] = ([],[])
split [p] = ([p], [])
split (p : q : ps) = let 
                        (xs, ys) = split ps
                     in
                        (p : xs, q : ys)

--merge toma una tupla de listas de puntos y la dimension y devuelve una lista
--  de puntos ordenados de menor a mayor
merge :: Punto p => ([p],[p]) -> Int -> [p]
merge ([], ys) _ = ys
merge (xs, []) _ = xs 
merge (x : xs, y : ys) dim = if coord  dim x <= coord dim y
                            then x : merge (xs, y : ys) dim
                            else y : merge (x:xs, ys) dim
                  
--medianaAux toma una lista de puntos, la longitud de la lista y la dimension de los puntos
--  y devuelve una tupla con una lista de puntos menores a la mediana, la mediana y una lista 
--  de puntos mayores a la mediana
--  Es una función auxiliar de "mediana" la cual lleva recursivamente la longitud de la lista 
--    en donde tiene que buscar el punto medio.
medianaAux :: (Punto p, Eq p) => [p] -> Int -> Int-> ([p], p, [p]) 
medianaAux ps len dim | even len =  let medio = elementoMedio ps (div len 2 + 1)
                                    in elementoI ps medio dim
                      | otherwise = let medio = elementoMedio ps (div (len + 1) 2)
                                    in elementoI ps medio dim

--mediana toma una lista de puntos y la dimension y devuelve una tupla con 
--  una lista de puntos menores a la mediana, un la mediana y una lista de
--  puntos mayores a la mediana
mediana :: (Punto p, Eq p) => [p] -> Int -> ([p], p, [p])
mediana ps dim = medianaAux (msort ps dim) (length ps) dim

--elementoMedio toma una lista de puntos y un Int (contador) y devuelve un punto
--  Recorre la lista recursivamente restandole uno al contador hasta llegar al punto buscado.
elementoMedio :: Punto p => [p] -> Int -> p
elementoMedio (p:ps) 1 = p
elementoMedio (p:ps) i = elementoMedio ps (i - 1) 

--elementoI toma una lista de puntos, la mediana y la dimensión de los putos y devuelve una 
--  tupla con una lista de puntos menores a la mediana, la mediana y una lista de puntos mayores
--  a la mediana.
--  Checkea punto por punto debido a que los puntos iguales a la mediana en el eje el cual se está
--  comparando deben ir al subarbol izquierdo.
elementoI :: (Punto p, Eq p) => [p] -> p -> Int -> ([p], p, [p])
elementoI [] mediana dim = ([],mediana,[]) --(pa, p, ps)
elementoI (p:ps) mediana dim | p == mediana = let (men, med, may) = elementoI ps mediana dim 
                                              in (men, mediana, may)
                             | coord dim p <= coord dim mediana = let (men, med, may) = elementoI ps mediana dim
                                                                  in (p:men, med, may)
                             | coord dim p > coord dim mediana =  let (men, med, may) = elementoI ps mediana dim
                                                                  in (men, med, p:may)

--Ejercicio 3.

--insertar toma un punto y un árbol de puntos y devuelve un árbol de puntos.
--  Compara el eje por el cual se ordenanan los subarboles y se inserta en una hoja. 
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p n@(Node Empty q Empty dim) | coord dim p <= coord dim q = if dim == dimension q - 1
                                                                   then Node (Node Empty p Empty 0) q Empty dim
                                                                   else Node (Node Empty p Empty (dim + 1)) q Empty dim
                                      | otherwise = if dim == dimension q - 1
                                                                   then Node Empty q (Node Empty p Empty 0) dim
                                                                   else Node Empty q (Node Empty p Empty (dim + 1)) dim
insertar p (Node left q Empty dim) | coord dim p <= coord dim q = Node (insertar p left) q Empty dim
                                   | otherwise = if dim == dimension q - 1
                                                 then Node left q (Node Empty p Empty 0) dim
                                                 else Node left q (Node Empty p Empty (dim + 1)) dim
insertar p (Node Empty q right dim) | coord dim p <= coord dim q = if dim == dimension q - 1
                                                                   then Node (Node Empty p Empty 0) q Empty dim
                                                                   else Node (Node Empty p Empty (dim + 1)) q Empty dim
                                    | otherwise = Node Empty q (insertar p right) dim
insertar p (Node left q right dim) | coord dim p <= coord dim q = Node (insertar p left) q right dim
                                   | otherwise = Node left q (insertar p right) dim



--Ejercicio 4.   

--maximo toma un árbol de puntos y un Int (eje) y devuelve un punto. 
--  Es una función auxiliar de "eliminar" la cual busca el punto con la máxima
--  coordenada según un eje.  
maximo :: Punto p => NdTree p -> Int -> p
maximo (Node Empty q Empty eje) _ = q
maximo (Node l q Empty eje) ejeBuscado =  let p1 = maximo l ejeBuscado
                                              maxL = coord ejeBuscado p1
                                              raiz = coord ejeBuscado q
                                              maxTree = max maxL raiz
                                          in  if coord ejeBuscado p1 == maxTree then p1 
                                              else q
maximo (Node Empty q r eje) ejeBuscado =  let p1 = maximo r ejeBuscado
                                              maxR = coord ejeBuscado p1
                                              raiz = coord ejeBuscado q
                                              maxTree = max maxR raiz
                                          in  if coord ejeBuscado p1 == maxTree then p1 
                                              else q
maximo (Node l q r eje) ejeBuscado = let p1 = maximo l ejeBuscado
                                         p2 = maximo r ejeBuscado
                                         maxL = coord ejeBuscado p1
                                         maxR = coord ejeBuscado p2
                                         raiz = coord ejeBuscado q
                                         maxTree = max (max maxL maxR) raiz
                                      in if coord ejeBuscado p1 == maxTree then p1 
                                         else if coord ejeBuscado p2 == maxTree then p2
                                              else q


--minimo toma un árbol de puntos y un Int (eje) y devuelve un punto. 
--  Es una función auxiliar de "eliminar" la cual busca el punto con la menor
--  coordenada según un eje.  
minimo :: Punto p => NdTree p -> Int -> p
minimo (Node Empty q Empty eje) _ = q
minimo (Node l q Empty eje) ejeBuscado =  let p1 = minimo l ejeBuscado
                                              minL = coord ejeBuscado p1
                                              raiz = coord ejeBuscado q
                                              minTree = min minL raiz
                                          in  if coord ejeBuscado p1 == minTree then p1 
                                              else q
minimo (Node Empty q r eje) ejeBuscado =  let p1 = minimo r ejeBuscado
                                              minR = coord ejeBuscado p1
                                              raiz = coord ejeBuscado q
                                              minTree = min minR raiz
                                          in  if coord ejeBuscado p1 == minTree then p1 
                                              else q
minimo (Node l q r eje) ejeBuscado = let p1 = minimo l ejeBuscado
                                         p2 = minimo r ejeBuscado
                                         minL = coord ejeBuscado p1
                                         minR = coord ejeBuscado p2
                                         raiz = coord ejeBuscado q
                                         minTree = min (min minL minR) raiz
                                      in if coord ejeBuscado p1 == minTree then p1 
                                         else if coord ejeBuscado p2 == minTree then p2
                                              else q

--eliminar toma un punto y un árbol de puntos y devuelve un árbol
--  Busca un punto en el árbol, si está lo elimina y busca un candidato 
--  para reemplazarlo, este puede ser el punto con la menor coordenada 
--  en el eje del nodo eliminado en el subarbol derecho o el punto con 
--  mayor coordenada en el eje del nodo eliminado del subarbol izquierdo 
--  y devuelve el árbol modificado.   
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p (Node Empty q Empty eje) = Empty
eliminar p (Node l q r eje) | p == q && r /= Empty = let min = minimo r eje 
                                                     in Node l min (eliminar min r) eje
                            | p == q && l /= Empty = let max = maximo l eje
                                                     in Node (eliminar max l) max r eje
                            | coord eje p <= coord eje q = Node (eliminar p l) q r eje   
                            | otherwise = Node l q (eliminar p r) eje


--Ejercicio 5.

--a)
type Rect = (Punto2d, Punto2d)

--inRegionAux toma un punto2d, un Rect (tupla de puntos2d) y un Int (dimensión) y devuelve un Bool.
--  Es una función auxiliar de "inRegion" la cual compara las coordenadas del punto 
--  con las coordenadas de los puntos del rectangulo según un eje (dimensión). inRegion la llama
--  con dimensión igual 0 y esta compara recursivamente las coordenadas del punto con las siguientes
--  componentes de los puntos del rectangulo. 
--  Si las componentes del punto están entre las componentes del rectangulo, entonces devuelve True sino False.    
inRegionAux :: Punto2d -> Rect -> Int -> Bool   
inRegionAux x r@(p1, p2) dim  | dimension x == dim = True
                              | otherwise = coord dim x >= min (coord dim p1) (coord dim p2) &&
                                            coord dim x <= max (coord dim p1) (coord dim p2) && 
                                            inRegionAux x r (dim + 1) 
                                      
--inRegion toma un punto2d y un Rect (tupla de puntos2d) y devuelve un Bool.
--  Verifica si un punto2d está dentro de una región definida con dos puntos2d.
inRegion ::  Punto2d -> Rect -> Bool
inRegion x (p1, p2) = inRegionAux x (p1, p2) 0


--b)
--ortogonalSearch toma un árbol de punto2d y un Rect (tupla de puntos2d) y devuelve una lista de puntos2d
--  Busca todos los puntos que están dentro de un Rect utilizando la función "inRegion" y los devuelve en una lista
ortogonalSearch ::  NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = [] 
ortogonalSearch (Node l q r eje) rect@(p1, p2) | inRegion q rect = q : ortogonalSearch l rect ++ ortogonalSearch r rect
                                               | coord eje q < min (coord eje p1) (coord eje p2) = ortogonalSearch r rect
                                               | coord eje q > max (coord eje p1) (coord eje p2) = ortogonalSearch l rect   
                                               | otherwise = ortogonalSearch l rect ++ ortogonalSearch r rect
                                    
                

--------------------------------------------------------------------------------------------------------------------------------
--puntos del 2
p1 = P2d (7,2)
p2 = P2d (5,4)
p3 = P2d (9,6)
p4 = P2d (2,3)
p5 = P2d (4,7)
p6 = P2d (8,1)

plist = [p1,p2,p3,p4,p5,p6]

-- putos del 3

pI1 = P2d (1,0)
pI2 = P2d (3,0)
pI3 = P2d (10,10)

-- puntos del 4
pb1 = P2d (1,100)
pb2 = P2d (2,99)
pb3 = P2d (3,98)
pb4 = P2d (4,97)
pb5 = P2d (4,96)
pb6 = P2d (4,95)
pb7 = P2d (100,2)

pblist = [pb1,pb2,pb3,pb4,pb5,pb6]

{-Instance Punto Punto1d where 
 - Funciones de la clase
-} 
