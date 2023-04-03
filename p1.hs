{-

Ejercicio 1
Sean las siguientes deniciones de funciones:
- max2 (x, y) | x >= y = x
              | otherwise = y
max2 :: (float, float) -> float

- normaVectorial (x, y) = sqrt (x^2 + y^2)
normaVectorial :: (float, float) -> float

- subtract = flip (-)
subtract :: float -> float -> float

- predecesor = subtract 1
predecesor :: float -> float

- evaluarEnCero = \f -> f 0
evaluarEnCero :: (float -> a) -> a

- dosVeces = \f -> f.f
dosVeces :: (a -> a) -> (a -> a)

- flipAll = map flip
flipAll :: [a -> b -> c] -> [b -> a -> c]

- flipRaro = flip flip
flip :: (a -> b -> c) -> (b -> a -> c)
flipRaro :: b -> (a -> b -> c) -> a -> c

i. ¾Cuál es el tipo de cada función? (Asumir que todos los números son de tipo Float).
ii. ¾Alguna de las funciones anteriores no está curricada? De ser así, escribir la versión curricada junto con
su tipo para cada una de ellas.
paja

 -}

max2 (x, y)
  | x >= y = x
  | otherwise = y

normaVectorial (x, y) = sqrt (x ^ 2 + y ^ 2)

subtract = flip (-)

predecesor = Main.subtract 1

{-
>>> evaluarEnCero predecesor
-1

>>> dosVeces predecesor 0
-2
 -}
evaluarEnCero = \f -> f 0

dosVeces = \f -> f . f

flipAll = Prelude.map flip

flipRaro = flip flip

{-

Ejercicio 2
i. Denir la función curry, que dada una función de dos argumentos, devuelve su equivalente curricada.
ii. Denir la función uncurry, que dada una función curricada de dos argumentos, devuelve su versión no
curricada equivalente. Es la inversa de la anterior.
iii. ¾Se podría denir una función curryN, que tome una función de un número arbitrario de argumentos y
devuelva su versión curricada?

no creo que se pueda porque cuando uno trabaja con tuplas, se sabe de antemano cuantas dimensiones tiene
sin embargo, al ser n un parametro, la dimensionalidad de la tupla es una incognita
lo que si seria posible es usar listas en vez de tuplas
 -}

curry f a b = f (a, b)

uncurry f (a, b) = f a b

{-
Ejercicio 3
¾Cuál es el valor de esta expresión?
[ x | x <- [1..3], y <- [x..3], (x + y) `mod' 3 == 0 ]

>>>[ x | x <- [1..3], y <- [x..3], (x + y) `mod` 3 == 0 ]
[1,3]
 -}

{-

Ejercicio 4
Una tripla pitagórica es una tripla (a, b, c) de enteros positivos tal que a2 + b2 = c2.
La siguiente expresión intenta ser una denición de una lista (innita) de triplas pitagóricas:
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]
Explicar por qué esta denición no es útil. Dar una denición mejor.

no es util porque nunca va a recorrer todas las posibles combinaciones de a, b y c
se va a quedar tildado intentando agotar a con b y c fijados en 1
o agotar c con a y b fijados, no estoy seguro

[(a, b, c) | a <- [1..], b <-[1..a], c <- [1..b], a^2 + b^2 == c^2]

>>> [(a, b, c) | c <- [1..], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]

soy crack
esto genera las triplas pitagoricas, pero no las primitivas, sino todas
 -}

pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | c <- [1 ..], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

{-

Ejercicio 5
Generar la lista de los primeros mil números primos. Observar cómo la evaluación lazy facilita la implementación de esta lista.

me quede con las ganas de usar los elementos ya generados de primos en vez de hacer [2..x-1]
 -}

primos :: [Integer]
primos = take 1000 [x | x <- [2 ..], all ((/= 0) . mod x) [2 .. x - 1]]

{-

Ejercicio 6
Usando listas por comprensión, escribir la función partir :: [a] -> [([a], [a])] que, dada una lista
xs, devuelve todas las maneras posibles de partirla en dos sublistas xs1 y xs2 tales que xs1 ++ xs2 == xs.
Ejemplo: partir [1, 2, 3] → [([], [1, 2, 3]),([1], [2, 3]), ([1, 2], [3]),
([1, 2, 3], [])]

 -}

partir :: [a] -> [([a], [a])]
partir l = ([], l) : [splitAt (i + 1) l | i <- [0 .. length l - 1]]

{-
Ejercicio 7
Escribir la función listasQueSuman :: Int -> [[Int]] que, dado un número natural n, devuelve todas
las listas de enteros positivos (es decir, mayores o iguales que 1) cuya suma sea n. Para este ejercicio se permite
usar recursión explícita.

saque la respuesta de cubawiki, no me terminaba de cerrar la consigna
>>> listasQueSuman 1
>>> listasQueSuman 2
>>> listasQueSuman 3
[[1]]
[[1,1],[2]]
[[1,1,1],[1,2],[2,1],[3]]
 -}

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x : xs | x <- [1 .. n], xs <- listasQueSuman (n - x)]

{-
 Ejercicio 8
Denir en Haskell una lista que contenga todas las listas nitas de enteros positivos (esto es, con elementos
mayores o iguales que 1).
  -}

listasPositivas :: [[Int]]
listasPositivas = [xs | n <- [0 ..], xs <- listasQueSuman n]

{- Ejercicio 9 F
i. Redenir usando foldr las funciones sum, elem, (++), filter y map.
ii. Denir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
(>).
iii. Denir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
iv. Denir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.
vi. Denir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop. -}

sum :: Num a => [a] -> a
sum = foldr (+) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 n = foldr (\x rec -> (x == n) || rec) False

{- (++) ::[a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs -}

filter p = foldr (\x rec -> if p x then x : rec else rec)

map2 f = foldr (\x rec -> f x : rec) []

mejorSegun p = foldr1 (\x rec -> if p x then x else rec)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\rec x -> if null rec then [x] else rec ++ [x + last rec]) []

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (-) 0

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> concatMap (\perm -> [pre ++ [x] ++ suf | (pre, suf) <- partir perm]) rec) [[]]

{-
>>> sumasParciales [1,4,-1,0,5]
[1,5,4,4,9]

>>> permutaciones [1, 2, 3]
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
 -}

{- Ejercicio 10
i. Denir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición.
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
(en algún orden).
ii. Denir la función prefijos, que dada una lista, devuelve todos sus prejos.
Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]
iii. Denir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que
aparecen consecutivos en la lista original).
Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
(en algún orden). -}

partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x :) rec) [[]]

{- >>> partes [1, 2, 3]
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

>>> prefijos [5, 1, 2]
[[],[5],[5,1],[5,1,2]]

sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
sublistas [1, 2] → [[], [1], [2], [1, 2]]
se puede hacer con foldr??
-}

prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x :) rec) [[]]

{- sublistas :: [a] -> [[a]]
sublistas = foldr (\x rec -> map (x :) rec ++ rec) [[]]  -}

{- Ejercicio 11 F
Sean las siguientes funciones:
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x]
                                    else x:elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x:(entrelazar xs [])
                            else x:head ys:entrelazar xs (tail ys)

Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando
foldr. En caso contrario, explicar el motivo. -}

{- La recursión de elementosEnPosicionesPares es global, ya que accede a un
resultado anterior: el de la recursión sobre la cola de la cola de la lista (es decir
tail xs).

La recursión de entrelazar es estructural, porque hace recursión sobre la cola
de la lista (xs) pero no accede a la cola en sí, ni a resultados de recursiones
anteriores

>>> entrelazar [1, 3, 5] [2, 4, 6]
[1,2,3,4,5,6]

>>> entrelazar [1, 3, 5, 7, 8] [2, 4, 6]
[1,2,3,4,5,6,7,8]

>>> entrelazar [1, 3, 5] [2, 4, 6, 7, 8]
[1,2,3,4,5,6,7,8]

-}

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec ys else x : head ys : rec (tail ys)) id


{- Ejercicio 12 F
El siguiente esquema captura la recursión primitiva sobre listas.
recr::(a->[a]->b->b)->b->[a]->b
recr \_ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
a. Denir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).
b. Explicar por qué el esquema foldr no es adecuado para implementar la función sacarUna del punto anterior.
c. Denr la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento.
d. La función listasQueSuman del ejercicio 7, ¾se ajusta al esquema de recursión recr? ¾Por qué o por qué no? -}

{- Ejercicio 13
La técnica de Divide & Conquer consiste en dividir un problema en problemas más fáciles de resolver y luego
combinando los resultados parciales, lograr obtener un resultado general.
Para generalizar la técnica, crearemos el tipo DivideConquer denido como:
type DivideConquer a b = (a -> Bool) -- determina si es o no el caso trivial
-> (a -> b) -- resuelve el caso trivial
-> (a -> [a]) -- parte el problema en sub-problemas
-> ([b] -> b) -- combina resultados
-> a -- estructura de entrada
-> b -- resultado
Denir las siguientes funciones:
i. dc :: DivideConquer a b que implementa la técnica. Es decir, completar la siguiente denición:
dc trivial solve split combine x = ...
La forma en que funciona es, dado un dato x, verica si es o no un caso base utilizando la función trivial.
En caso de serlo, utilizaremos solve para dar el resultado nal. En caso de no ser un caso base, partimos
el problema utilizando la función split y luego combinamos los resultados recursivos utilizando combine.
Por ser este un esquema de recursión, puede utilizarse recursión explícita para denirlo.
ii. Implementar la función mergeSort :: Ord a => [a] -> [a] en términos de dc.
mergeSort = dc ... (se recomienda utilizar break y aplicación parcial para denir la función de combine).
iii. Utilizar el esquema dc para reimplementar map y filter.
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
 -}

{- Ejercicio 14
i. Denir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.
ii. Usando genLista, denir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo. -}

{- Ejercicio 15 F
Denir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
listas nitas e innitas.
i. mapPares, una versión de map que toma una función curricada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la curricación y utilizar evaluación parcial.
iii. mapDoble, una variante de mapPares, que toma una función curricada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
las dos listas. Esta función en Haskell se llama zipWith.
 -}

{- Ejercicio 16
i. Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus las. Esto quiere decir que cada matriz será una lista nita de listas nitas,
todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se dene como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
ii. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M, entonces el resultado debe tener M listas, todas de
longitud N.
trasponer :: [[Int]] -> [[Int]] -}

{- Ejercicio 17 F
Denimos la función generate, que genera listas en base a un predicado y una función, de la siguiente
manera:
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []
generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
\| otherwise = generateFrom stop next (xs ++ [next xs])
i. Usando generate, denir generateBase::([a] -> Bool) -> a -> (a -> a) -> [a], similar a
generate, pero con un caso base para el elemento inicial, y una función que, en lugar de calcular el siguiente
elemento en base a la lista completa, lo calcula a partir del último elemento. Por ejemplo: generateBase
(\l->not (null l) && (last l > 256)) 1 (*2) es la lista las potencias de 2 menores o iguales que 256.
ii. Usando generate, denir factoriales::Int -> [Int], que dado un entero n genera la lista de los
primeros n factoriales.
iii. Usando generateBase, denir iterateN :: Int -> (a -> a) -> a -> [a] que, toma un entero n, una
función f y un elemento inicial x, y devuelve la lista [x, f x, f (f x), ..., f ( ...(f x) ...)] de
longitud n. Nota: iterateN n f x = take n (iterate f x).
iv. Redenir generateFrom usando iterate y takeWhile. -}
