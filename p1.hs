import Data.List (intersect, union)

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
ii. Denir la función uncurry2, que dada una función curricada de dos argumentos, devuelve su versión no
curricada equivalente. Es la inversa de la anterior.
iii. ¾Se podría denir una función curryN, que tome una función de un número arbitrario de argumentos y
devuelva su versión curricada?

no creo que se pueda porque cuando uno trabaja con tuplas, se sabe de antemano cuantas dimensiones tiene
sin embargo, al ser n un parametro, la dimensionalidad de la tupla es una incognita
lo que si seria posible es usar listas en vez de tuplas
 -}

curry f a b = f (a, b)

uncurry2 f (a, b) = f a b

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

a. Denir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).
b. Explicar por qué el esquema foldr no es adecuado para implementar la función sacarUna del punto anterior.
c. Denr la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento.
d. La función listasQueSuman del ejercicio 7, ¾se ajusta al esquema de recursión recr? ¾Por qué o por qué no? -}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x == e then xs else x : rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec -> if n < x then n : x : xs else x : rec) []

{- >>>sacarUna 3 [2, 3, 4, 4, 5, 3]
[2,4,4,5,3]

>>> insertarOrdenado 4 [1, 2, 5, 6]
[1,2,4,5,6]

listas que suman usa recursion global
-}

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

type DivideConquer a b =
  (a -> Bool) -> -- determina si es o no el caso trivial
  (a -> b) -> -- resuelve el caso trivial
  (a -> [a]) -> -- parte el problema en sub-problemas
  ([b] -> b) -> -- combina resultados
  a -> -- estructura de entrada
  b -- resultado

dc :: DivideConquer a b
dc trivial solve split combine x = if trivial x then solve x else combine (map rec (split x))
  where
    rec = dc trivial solve split combine

mergeSort :: Ord a => [a] -> [a]
mergeSort = dc ((== 1) . length) id (\x -> [take (div (length x) 2) x, drop (div (length x) 2) x]) (\[xs, ys] -> foldr insertarOrdenado xs ys)

-- >>> mergeSort [5, 4, 3, 2, 1, 0]
-- [0,1,2,3,4,5]

{- Ejercicio 14
i. Denir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos,
a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.
ii. Usando genLista, denir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo. -}

genLista :: a -> (a -> a) -> Integer -> [a]
genLista inicial gen cant = foldr (\_ rec -> rec ++ [head rec]) [inicial] [0 .. cant]

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta desde hasta = genLista desde succ (hasta - desde)

-- >>> desdeHasta 3 7
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
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f a b = mapPares f (armarPares a b)

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

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

trasponer :: [[Int]] -> [[Int]]
trasponer m = foldr (zipWith (:)) (map (const []) m) m

-- >>> trasponer [[1,2], [3,4], [5,6]]
-- [[1,3,5],[2,4,6]]

{- Ejercicio 17 F
Denimos la función generate, que genera listas en base a un predicado y una función, de la siguiente
manera:
...

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

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
  | stop xs = init xs
  | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop inicial next = generate stop (\xs -> if null xs then next inicial else next (last xs))

-- >>>generateBase (\l->not (null l) && (last l > 256)) 1 (*2)
-- [2,4,8,16,32,64,128,256]
factoriales :: Int -> [Int]
factoriales n = generate ((n <) . length) (\xs -> if null xs then 1 else length xs * last xs)

-- >>>factoriales 6
-- [1,1,2,6,24,120]

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase ((n ==) . length) x f

{- generateFrom2 :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom2 stop next xs = takeWhile (not . stop) (iterate next xs)
no tipa
-}

{-
Ejercicio 18 F
i. Denir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar denida sólo para los enteros mayores o iguales que 0).
ii. Utilizando foldNat, denir la función potencia.
 -}

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f base 0 = base
foldNat f base n = f n (foldNat f base (n - 1))

potencia :: Integer -> Integer -> Integer
potencia base = foldNat (\_ rec -> rec * base) 1

-- >>>potencia 2 3
-- 16

{- En este ejercicio trabajaremos con matrices innitas representadas como funciones:
type MatrizInfinita a = Int->Int->a
donde el primer argumento corresponde a la la, el segundo a la columna y el resultado al valor contenido
en la celda correspondiente.
Por ejemplo, las siguientes deniciones:
identidad = \i j->if i==j then 1 else 0
cantor = \x y->(x+y)*(x+y+1)`div`2+y
pares = \x y->(x,y)
corresponden a las matrices:
identidad
1 0 0 · · ·
0 1 0 · · ·
0 0 1 · · ·
.
cantor
0 2 5 · · ·
1 4 8 · · ·
3 7 12 · · ·
.
pares
(0,0) (0,1) (0,2) · · ·
(1,0) (1,1) (1,2) · · ·
(2,0) (2,1) (2,2) · · ·
.

Denir las siguientes funciones:
i. fila::Int->MatrizInfinita a->[a] y columna::Int->MatrizInfinita a->[a] que, dado un índice,
devuelven respectivamente la la o la columna correspondiente en la matriz (en forma de lista innita).
Por ejemplo, fila 0 identidad devuelve la lista con un 1 seguido de innitos 0s.
ii. trasponer::MatrizInfinita a->MatrizInfinita a, que dada una matriz devuelve su transpuesta.
iii. mapMatriz::(a->b)->MatrizInfinita a->MatrizInfinita b,
filterMatriz::(a->Bool)->MatrizInfinita a->[a] y
zipWithMatriz::(a->b->c)->MatrizInfinita a->MatrizInfinita b->MatrizInfinita c, que se
comportan como map, filter y zipWith respectivamente, pero aplicadas a matrices innitas. En el caso
de filterMatriz no importa el orden en el que se devuelvan los elementos, pero se debe pasar una y sólo
una vez por cada posición de la matriz.
iv. suma::Num a=>MatrizInfinita a->MatrizInfinita a->MatrizInfinita a, y
zipMatriz::MatrizInfinita a->MatrizInfinita b->MatrizInfinita (a,b). Denir ambas utilizando zipWithMatriz.
 -}

type MatrizInfinita a = Int -> Int -> a

fila :: Int -> MatrizInfinita a -> [a]
fila i m = map (m i) [0 ..]

columna :: Int -> MatrizInfinita a -> [a]
columna i m = map (flip m i) [0 ..]

trasponerMat :: MatrizInfinita a -> MatrizInfinita a
trasponerMat = flip

mapMatriz :: (a -> b) -> MatrizInfinita a -> MatrizInfinita b
mapMatriz f m i j = f $ m i j

filterMatriz :: (a -> Bool) -> MatrizInfinita a -> [a]
filterMatriz p m = [m i j | z <- [0 ..], i <- [0 .. z], j <- [0 .. z], i + j == z, p (m i j)]

zipWithMatriz :: (a -> b -> c) -> MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita c
zipWithMatriz f m1 m2 i j = f (m1 i j) (m2 i j)

suma :: Num a => MatrizInfinita a -> MatrizInfinita a -> MatrizInfinita a
suma = zipWithMatriz (+)

zipMatriz :: MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita (a, b)
zipMatriz = zipWithMatriz (,)

{- Ejercicio 22 F
Sea el siguiente tipo, que representa a los árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
i. Denir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y dar su tipo.
ii. Denir las funciones esNil y cantNodos (para esNil puede utilizarse case en lugar de foldAB o recAB).
iii. Denir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 9, para árboles.
Se recomienda denir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil.
iv. Denir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
v. Justicar la elección de los esquemas de recursión utilizados para los tres puntos anteriores.
 -}

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB base _ Nil = base
foldAB base f (Bin sub1 x sub2) = f x (foldAB base f sub1) (foldAB base f sub2)

recAB :: b -> (a -> b -> b -> AB a -> AB a -> b) -> AB a -> b
recAB base _ Nil = base
recAB base f (Bin sub1 x sub2) = f x (recAB base f sub1) (recAB base f sub2) sub1 sub2

esNil :: AB a -> Bool
esNil ab = case ab of
  Nil -> True
  _ -> False

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\_ rec1 rec2 -> 1 + rec1 + rec2)

mejorSegúnAB :: (a -> a -> Bool) -> AB a -> a
mejorSegúnAB p (Bin sub1 x sub2) = recAB x (seleccionarMejor (quedarseConMejor p)) (Bin sub1 x sub2)
  where
    quedarseConMejor p x x2 = if p x x2 then x else x2

seleccionarMejor :: (a -> a -> a) -> a -> a -> a -> AB a -> AB a -> a
seleccionarMejor mejor x mejorSub1 mejorSub2 sub1 sub2
  | esNil sub1 && esNil sub2 = x
  | esNil sub1 = mejor x mejorSub2
  | esNil sub2 = mejor x mejorSub1
  | otherwise = mejor (mejor x mejorSub2) mejorSub1

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\x rec1 rec2 sub1 sub2 -> (esNil sub1 || extraer sub1 < x) && (esNil sub2 || extraer sub2 > x) && rec1 && rec2)

extraer (Bin _ x _) = x

{- Ejercicio 23
Dado el tipo AB a del ejercicio 22:
i. Denir las altura, ramas, cantHojas y espejo.
ii. Denir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos
tienen la misma forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y
recordar el ejercicio 16.
 -}
altura :: AB a -> Int
altura = foldAB 0 (\_ rec1 rec2 -> 1 + max rec1 rec2)

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\_ rec1 rec2 sub1 sub2 -> (if esNil sub1 && esNil sub2 then 1 else 0) + rec1 + rec2)

ramas = foldAB [[]] (\x ramas1 ramas2 -> map (x :) ramas1 ++ map (x :) ramas2)

espejo = foldAB Nil (\x esp1 esp2 -> Bin esp2 x esp1)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB esNil (\_ mismaSub1 mismaSub2 ab -> not (esNil ab) && mismaSub1 (izq ab) && mismaSub2 (der ab))
  where
    izq (Bin ab _ _) = ab
    der (Bin _ _ ab) = ab

{- Ejercicio 24
Se desea modelar en Haskell los árboles con información en las hojas (y sólo en ellas). Para esto introduciremos
el siguiente tipo:
data AIH a = Hoja a | Bin (AIH a) (AIH a)
a) Denir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita.
b) Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.
c) Denir la lista (innita) de todos los AIH cuyas hojas tienen tipo ()1
. Se recomienda denir una función
auxiliar. Para este ejercicio se permite utilizar recursión explícita.
d) Explicar por qué la recursión utilizada en el punto c) no es estructural.
 -}

data AIH a = Hoja a | Bin2 (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH base bin (Hoja a) = base a
foldAIH base bin (Bin2 a b) = bin (foldAIH base bin a) (foldAIH base bin b)

arbolesDeAltura :: Int -> [AIH ()]
arbolesDeAltura 1 = [Hoja ()]
arbolesDeAltura n = [Bin2 nMenos1 rec | nMenos1 <- arbolesDeAltura (n - 1), x <- [1 .. n - 1], rec <- arbolesDeAltura x] ++ [Bin2 rec nMenos1 | nMenos1 <- arbolesDeAltura (n - 1), x <- [1 .. n], rec <- arbolesDeAltura x]

{- recursion global etc -}

{-
>>> arbolesDeAltura 2
 -}
todosLosAIH = [arbolesDeAltura n | n <- [0 ..]]

{- Ejercicio 26 F
Las máquinas de estados no determinísticas (MEN) se pueden ver como una descripción de un sistema que,
al recibir como entrada una constante de un alfabeto (que en general llamamos Σ), y encontrándose en un
estado q, altera su estado según lo indique una función de transición (que en general llamamos δ). Observemos
que al ser una máquina no determinística, el resultado de esta función de transición no es un único estado sino
un conjunto de ellos. Modelaremos estos autómatas mediante el tipo MEN2
.
data MEN a b = AM {sigma :: [a], delta :: (a -> b -> [b])}
Luego, el sistema representado por m :: MEN a b que se encuentra en un estado q :: b , después de recibir
una entrada s :: a tal que s ∈ sigma m, se encontrará en alguno de los estados delta m s q (si esta lista
es vacía signica que s es una transición inválida, mientras que si contiene muchos estados, signica que puede
alcanzar cuaquiera de ellos, sin que podamos suponer nada sobre cuál será).

Se pide denir las siguientes funciones, sin utilizar recursión explícita:
3
.
i. a) agregarTransicion :: a -> b -> b -> MEN a b -> MEN a b que, dada una constante s y dos
estados q0 y qf , agrega al autómata la transición por s desde q0 a qf . Si lo necesita, puede suponer
que la transición no está previamente denida en el autómata, que s ya pertenece al alfabeto y que
está denida la igualdad para los tipos a y b, indicando qué suposiciones realiza y por qué.
b) interseccion :: Eq a => MEN a b -> MEN a c -> MEN a (b,c) que dados dos autómatas m y
n, devuelve el autómata intersección, cuyo alfabeto es la intersección de los dos alfabetos, cuyos
estados son el producto cartesiano del conjunto de estados de cada uno y que puede moverse de
(qm, qn) por el símbolo s al estado (q'm, q'n) si y solo si m puede moverse de qm a q'm por s y n puede
moverse de qn a q'n por el mismo s.
ii. consumir :: MEN a b -> b -> [a] -> [b] que, dados un autómata m, un estado q y una cadena de
símbolos ss, devuelve todos los estados en los que se puede encontrar m después de haber leido los símbolos
de ss (en ese orden), habiendo partido del estado q. Si lo necesita, puede suponer denida la igualdad
para los tipos a y b.
En el autómata de ejemplo, consumir MP LP q0 “pl” ❀ [q2,q3]
iii. trazas :: MEN a b -> b -> [[a]] que, dado un autómata m y un estado q, devuelve la lista con todas
las trazas posibles en m a partir de q, es decir, todas las cadenas de símbolos que pueden llevar a m desde
q a algún estado mediante transiciones válidas.
Asumir que existe al menos un ciclo en el autómata, por lo que la lista resultante es innita. Si lo necesita,
puede suponer que tanto el alfabeto como el resultado de las transiciones son nitos, y que está denida
la igualdad para los tipos a y b. Deberá indicar qué suposiciones realiza y por qué.
En el autómata de ejemplo, trazas MP LP q0 ❀ [[0p
0], [0p
0,
0
l
0], [0p
0,
0
l
0,
0p
0]]
 -}

data MEN a b = AM {sigma :: [a], delta :: a -> b -> [b]}

agregarTransicion :: (Eq a, Eq b) => a -> b -> b -> MEN a b -> MEN a b
agregarTransicion s q0 qf aut = AM {sigma = union [s] (sigma aut), delta = \trans estado -> delta aut trans estado ++ if trans == s && estado == q0 then [qf] else []}

interseccion :: Eq a => MEN a b -> MEN a c -> MEN a (b, c)
interseccion m n = AM {sigma = intersect (sigma m) (sigma n), delta = \trans (qm, qn) -> [(qPrimaM, qPrimaN) | qPrimaM <- delta m trans qm, qPrimaN <- delta n trans qn]}

consumir :: MEN a b -> b -> [a] -> [b]
consumir m q = foldl (\rec s -> concatMap (delta m s) rec) [q]

trazas :: MEN a b -> b -> [[a]]
trazas m q = Prelude.filter (not . null . consumir m q) [cadena | n <- [0 ..], cadena <- cadenasDeLong n]
  where
    cadenasDeLong = foldNat (\_ cadenasDeLongNMenos1 -> [s : cadenaNMenos1 | s <- sigma m, cadenaNMenos1 <- cadenasDeLongNMenos1]) []