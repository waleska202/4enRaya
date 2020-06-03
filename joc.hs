import System.IO
import System.Random
import Data.Array
import Data.List 





--ACLARACIONES:
--El jugador que tira primero sera el jugador X, el otro sera el jugador Y.



type Estrategia = String


type Pair a = (a, a)

--tipo de dato para definir una posicion del tablero:
--cada posicion es un pair de ints indicando la fila y la columna.
type Posicion = Pair Int

--tipo de dato para representar una lista de posiciones.
type Posiciones = [Posicion]

--tipo puntos para valorar un tablero.
type Puntos = Int

--tipo de dato del tablero: el tablero Tab(px, py, f, c) representa el tablero donde:
--px es la lista de posiciones en las que ha colocado ficha el jugador X.
--py es la lista de posiciones en las que ha colocado ficha el jugador Y.
--f es el numero de filas del tablero.
--c es el numero de columnas del tablero.
data Tablero = Tablero Int Int Posiciones Posiciones deriving Show


--tipo de datos arbol: nodo con una lista de hijos
data Arbol a = Nodo a [Arbol a]


--definir y poder cambiar la profundidad de busqueda del algoritmo minimax en la estrategia smart
profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6



----------------------------------------------------------
----------------------------------------------------------
------------------------UTILS-----------------------------
----------------------------------------------------------
----------------------------------------------------------
--funcion que dada una columna especificada devuelve la posicion libre de esa columna
columnaPos :: Int -> Tablero -> Posicion
columnaPos c t
    | [ x | x <- (posicionesLibres t), (second x) == c] /= [] = listaElem [ x | x <- (posicionesLibres t), (second x) == c]
    | otherwise = (-1,-1)

--funcion que dada una list de una sola posicion devuelve la posicion
listaElem :: Posiciones -> Posicion
listaElem ps = head ps



atoi::String->Int
atoi=read

----RANDOM

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result



--ORDENACION----------------------------------------------
insertM :: [Int] -> Int -> [Int]
insertM [] p = [p]
insertM (x:xs) n
        | n < x = n:(x:xs)
        | otherwise = concat [[x], insertM xs n]


--InsertionSortAlgorithm----------------------------------
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insertM (isort xs) x

----------------------------------------------------------
--funciones para acceder a los elementos de una posicion
first :: Posicion -> Int
first (a,_) = a

second :: Posicion -> Int
second (_,b) = b


--MATRICES------------------------------------------------

--funcion que genera la matriz de posiciones de la forma [[posiciones de fila 1][posiciones de fila 2][...]...]
generaMatriz :: Int -> Int -> [Posiciones]
generaMatriz f c
    | f-1 > 0 = (generaMatriz (f-1) c)++[(generaFilaDet (f-1) c)]
    | otherwise = [(generaFilaDet 0 c)]

--funcion que genera una lista de posiciones de una fila determinada y un numero de columnas
--ej: generaFilaDet 2 4 = [(2,0),(2,1),(2,2),(2,3)]
generaFilaDet :: Int -> Int -> Posiciones
generaFilaDet f c
    | c-1 > 0 = (generaFilaDet f (c-1))++[(f,c-1)]
    | otherwise = [(f,0)]


generaMatrizSimple :: Int -> Int -> Posiciones
generaMatrizSimple f c = concat (generaMatriz f c)


----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------


--definicion del tablero inicial donde no hay colocada ninguna ficha
tableroIni:: Int -> Int -> Tablero
tableroIni f c = Tablero f c [] []

--funcion que verifica si en el tablero t le toca tirar al jugador X
turnoDeX::Tablero -> Bool
turnoDeX (Tablero f c l1 l2) = length l1 == length l2


--funcion que verifica si soy el jugador X
soyX::Tablero -> Bool
soyX t 
    | turnoDeX t = True
    | otherwise = False

--funcion que devuelve la lista de posiciones del jugador contrario
posicionesContrario :: Tablero -> Posiciones
posicionesContrario (Tablero f c px py)
    | turnoDeX (Tablero f c px py) = py
    | otherwise = px

--funcion que devuelve mi lista de posiciones
posicionesMias :: Tablero -> Posiciones
posicionesMias (Tablero f c px py)
    | turnoDeX (Tablero f c px py) = px
    | otherwise = py

--funcion para colocar ficha en el tablero: 
coloca :: Tablero -> Posicion -> Tablero
coloca (Tablero f c l1 l2) p 
    | turnoDeX (Tablero f c l1 l2) = Tablero f c (p:l1) l2
    | otherwise = Tablero f c l1 (p:l2)


--funcion que comprueba si en el tablero ya no caben mas fichas
completo :: Tablero -> Bool
completo (Tablero f c l1 l2) = length l1 + length l2 == f * c

--funcion que comprueba si una posicion esta dentro de los limites del tablero
dentroLimites :: Posicion -> Tablero -> Bool
dentroLimites p (Tablero f c px py) = ((first p) < f) && ((second p) < c) && ((first p) > -1) && ((second p) > -1)


--funcion que comprueba si una posicion esta ocupada
posOcupada :: Posicion -> Tablero -> Bool
posOcupada p (Tablero f c px py) = (elem p px) || (elem p py)

--funcion que comprueba si una posicion esta disponible
posDisponible :: Posicion -> Tablero -> Bool
posDisponible x t
    | elem x (posicionesLibres t) = True
    | otherwise = False

--funcion que comprueba si una columna esta dentro del tablero
columnaCorrecta :: Int -> Tablero -> Bool
columnaCorrecta x (Tablero f c px py)
    | (x > -1) && (x < c) = True
    | otherwise = False

--funcion que comprueba si una posicion esta dentro del tablero
posDentroTablero :: Posicion -> Tablero -> Bool
posDentroTablero p (Tablero f c px py)
    | ((first p) > -1) && ((first p) < f) && ((second p) < c) && ((second p) > -1) = True
    | otherwise = False

--funciones que retornan determinada posicion contigua en el tablero

posAnteriorHorizontal :: Posicion -> Posicion
posAnteriorHorizontal p = ((first p),((second p)-1))

posPosteriorHorizontal :: Posicion -> Posicion
posPosteriorHorizontal p = ((first p),((second p)+1))

posSuperiorVertical :: Posicion -> Posicion
posSuperiorVertical p = (((first p)-1),(second p))

posInferiorVertical :: Posicion -> Posicion
posInferiorVertical p = (((first p)+1),(second p))

posSuperiorDiagonal1 :: Posicion -> Posicion
posSuperiorDiagonal1 p = (((first p)-1),((second p)+1))

posInferiorDiagonal1 :: Posicion -> Posicion
posInferiorDiagonal1 p = (((first p)+1),((second p)-1))

posSuperiorDiagonal2 :: Posicion -> Posicion
posSuperiorDiagonal2 p = (((first p)-1),((second p)-1))

posInferiorDiagonal2 :: Posicion -> Posicion
posInferiorDiagonal2 p = (((first p)+1),((second p)+1))

----------------------------------------------------------
----------------------------------------------------------
----------------COMPROBACION-4-EN-RAYA--------------------
----------------------------------------------------------
----------------------------------------------------------

--devuelve la primera lista seguida que encuentra
listaSeguida :: [Int] -> [Int]
listaSeguida [] = []
listaSeguida [x] = [x]
listaSeguida (x:xs) = if x==q-1 then [x]++(listaSeguida xs) else [x]
    where q = head xs

--devuelve la lista quitando los x primeros numeros seguidos
restoLista :: [Int] -> [Int]
restoLista [] = []
restoLista [x] = []
restoLista (x:xs) = if x==q-1 then restoLista xs else xs 
    where q = head xs

--funcion que devuelve la lista seguida mas larga que encuentra
listaSeguidaLarga :: [Int] -> [Int]
listaSeguidaLarga [] = []
listaSeguidaLarga [x] = [x]
listaSeguidaLarga (x:xs) = if length j >= length q then j else q
	where j = listaSeguida (x:xs)
	      q = listaSeguidaLarga (restoLista (x:xs))

--lista Menos l1 l2 l3: l3 son los elemenos de l1 que no estan en l2
listaMenos :: Posiciones -> Posiciones -> Posiciones
listaMenos [] _ = []
listaMenos l1 [] = l1
listaMenos l1 l2 = [ x | x <- l1, not (elem x l2)]

-----------------------------------------------------------
--COMRPOBACION-HORIZONTAL----------------------------------
-----------------------------------------------------------

--funcion que comprueba si en una fila horizontal del tablero el jugador tiene alguna ficha
--fila a comprobar -> posiciones del juador -> bool
tieneFichaHorizontal :: Int -> Posiciones -> Bool
tieneFichaHorizontal f l = [x | x <- l, first x == f] /= []


--funcion que coge todas las fichas de una fila horizontal de un jugador
--fila a comprobar -> posiciones del jugador -> lista de las columnas de esa fila en las que el jugador tiene ficha
fichasEnHorizontal :: Int -> Posiciones -> [Int]
fichasEnHorizontal f l = [second x | x <- l, first x == f]


--funcion que comprueba si una fila determinada tiene i en raya
tieneRayaHorizontalFila :: Int -> Int -> Posiciones -> Bool
tieneRayaHorizontalFila i f l = length (listaSeguidaLarga (isort ( fichasEnHorizontal f l))) >= i

--comprueba si un jugador tiene i en raya en horizontal
--numero de fichas que quieres comprobar si tiene en raya -> numero de filas del tablero -> Posiciones -> Bool
compruebaHorizontal :: Int -> Int -> Posiciones -> Bool
compruebaHorizontal i f p  
	| f-1 > 0 = (tieneRayaHorizontalFila i (f-1) p) || (compruebaHorizontal i (f-1) p)
	| otherwise = (tieneRayaHorizontalFila i (0) p)

--funcion que devuelve la fila que tiene i en raya
--solo puedes llamar a esta funcion si has comprobado que existe el i en raya
cualHorizontal :: Int -> Int -> Posiciones -> Int
cualHorizontal i f p  
  | (f-1 > -1) && (tieneRayaHorizontalFila i (f-1) p) = f-1
  | otherwise = (cualHorizontal i (f-1) p)

--dada una fila devuelve las columnas que hacen i en raya
cualHorizontalColumnas :: Int -> Int -> Posiciones -> [Int]
cualHorizontalColumnas i f l = listaSeguidaLarga (isort (fichasEnHorizontal f l))

--dada una fila y una lista de colunma devuelve la lisa de posiciones correspondientes
filaColumnasPos :: Int -> [Int] -> Posiciones
filaColumnasPos f l = [(f,y) | y <- l]



-----------------------------------------------------------
--COMRPOBACION-VERTICAL------------------------------------
-----------------------------------------------------------

--funcion que comprueba si en una fila horizontal del tablero el jugador tiene alguna ficha
--columna a comprobar,posiciones del juador -> bool
tieneFichaVertical :: Int -> Posiciones -> Bool
tieneFichaVertical c l = [x | x <- l, second x == c] /= []


--funcion que coge todas las fichas de una fila vertical de un jugador
--columna a comprobar, posiciones del jugador -> lista de las filas de esa columna en las que el jugador tiene ficha
fichasEnVertical :: Int -> Posiciones -> [Int]
fichasEnVertical c l = [first x | x <- l, second x == c]


--funcion que comprueba si una columna determinada tiene i en raya
tieneRayaVerticalColumna :: Int -> Int -> Posiciones -> Bool
tieneRayaVerticalColumna i c l = length (listaSeguidaLarga (isort ( fichasEnVertical c l))) >= i

--comprueba si un jugador tiene 4 en raya en vertical
--numero de columnas del tablero -> Posiciones -> Bool
compruebaVertical :: Int -> Int -> Posiciones -> Bool
compruebaVertical i c p  
	| c-1 > 0 = (tieneRayaVerticalColumna i (c-1) p) || (compruebaVertical i (c-1) p)
	| otherwise = (tieneRayaVerticalColumna i (0) p)

--funcion que devuelve la columna que tiene i en raya
--solo puedes llamar a esta funcion si has comprobado que existe el i en raya
cualVertical :: Int -> Int -> Posiciones -> Int
cualVertical i c p  
  | (c-1 > -1) && (tieneRayaVerticalColumna i (c-1) p) = c-1
  | otherwise = (cualVertical i (c-1) p)


--dada una columna devuelve las filas que hacen i en raya
cualVerticalFilas :: Int -> Int -> Posiciones -> [Int]
cualVerticalFilas i c l = listaSeguidaLarga (isort (fichasEnVertical c l))

--dada una columna y una lista de filas devuelve la lisa de posiciones correspondientes
columnaFilasPos :: Int -> [Int] -> Posiciones
columnaFilasPos c l = [(x,c) | x <- l]

-----------------------------------------------------------
--COMRPOBACION-DIAGONAL------------------------------------
-----------------------------------------------------------


--funcion que devuelve una lista de las diagonales de una matriz
diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))


--funcion que dadas las filas y las columnas de una matriz
--devuelve una lista de listas con posiciones donde cada una es una diagonal
diagonalsMatriz1 :: Int -> Int -> [Posiciones] 
diagonalsMatriz1 f c = diagonals (generaMatriz f c)

--lo mismo que diagonalsMatriz1 pero las diagonales inversas
diagonalsMatriz2 :: Int -> Int -> [Posiciones] 
diagonalsMatriz2 f c = diagonals ( reverse (generaMatriz f c))


--funcion que coge todas las fichas de una fila diagonal de un jugador trasformandolas a ints
--0 -> diagonal a comprobar -> posiciones del jugador -> lista de las posiciones de esa diagonal en las que el jugador tiene ficha(representadas con ints)
fichasEnDiagonal :: Int -> Posiciones -> Posiciones -> [Int]
fichasEnDiagonal i [] p = []
fichasEnDiagonal i (d:ds) p
	| elem d p = [i] ++ (fichasEnDiagonal (i+1) ds p)
	| otherwise = (fichasEnDiagonal (i+1) ds p)



--funcion que comprueba si una diagonal determinada tiene i en raya
tieneRayaUnaDiagonal :: Int -> Posiciones -> Posiciones -> Bool
tieneRayaUnaDiagonal i d p = length (listaSeguidaLarga (fichasEnDiagonal 0 d p)) >= i

--funcion que dada una daigonal y unas posiciones que tienen raya en esa diagonal quita las posiciones que formen parte de la raya
posicionesRayaDiagonal :: Int -> Posiciones -> Posiciones -> Posiciones
posicionesRayaDiagonal i d p 
    | tieneRayaUnaDiagonal i d p = [ x | x <- p, elem x d]
    | otherwise = []


--comprueba si en una lista de diagonales hay un i en raya en diagonal
--lista de las diagonales a comprobar -> Posiciones -> Bool
compruebaDiagonales :: Int -> [Posiciones] -> Posiciones -> Bool
compruebaDiagonales i [] p = False
compruebaDiagonales i (d:ds) p = (tieneRayaUnaDiagonal i d p) || (compruebaDiagonales i ds p)

--en una lista de diagonales te da la lista de posiciones que hacen i en raya en alguna diagonal
--lista de las diagonales a comprobar -> Posiciones -> Posiciones
posicionesDiagonales :: Int -> [Posiciones] -> Posiciones -> Posiciones
posicionesDiagonales i [] p = []
posicionesDiagonales i (d:ds) p
    | tieneRayaUnaDiagonal i d p = (posicionesRayaDiagonal i d p)
    | otherwise = (posicionesDiagonales i ds p)


compruebaDiagonalesDef :: Int -> Int -> Int -> Posiciones -> Bool
compruebaDiagonalesDef i f c p = (compruebaDiagonales i (diagonalsMatriz1 f c) p) || (compruebaDiagonales i (diagonalsMatriz2 f c) p)


compruebaDiagonales1 :: Int -> Int -> Int -> Posiciones -> Bool
compruebaDiagonales1 i f c p = (compruebaDiagonales i (diagonalsMatriz1 f c) p)


compruebaDiagonales2 :: Int -> Int -> Int -> Posiciones -> Bool
compruebaDiagonales2 i f c p = (compruebaDiagonales i (diagonalsMatriz2 f c) p)


-----------------------------------------------------------
--COMRPOBACION-TOTAL---------------------------------------
-----------------------------------------------------------

--funcion que comprueba si un jugador ha ganado
--filas, columnas, posiciones -> Bool
ganaJugador :: Int -> Int -> Posiciones -> Bool
ganaJugador f c p = (compruebaHorizontal 4 f p) || (compruebaVertical 4 c p) || (compruebaDiagonalesDef 4 f c p)

--funcion que comprueba si un jugador tiene algun 3 en raya
--filas, columnas, posiciones -> Bool
hay3EnRaya :: Int -> Int -> Posiciones -> Bool
hay3EnRaya f c p = (compruebaHorizontal 3 f p) || (compruebaVertical 3 c p) || (compruebaDiagonalesDef 3 f c p)

--funcion que comprueba si un jugador tiene algun 2 en raya
--filas, columnas, posiciones -> Bool
hay2EnRaya :: Int -> Int -> Posiciones -> Bool
hay2EnRaya f c p = (compruebaHorizontal 2 f p) || (compruebaVertical 2 c p) || (compruebaDiagonalesDef 2 f c p)

--funcion que indica si hay un ganador
hayGanador :: Tablero -> Bool
hayGanador (Tablero f c px py) = (ganaJugador f c px) || (ganaJugador f c py)

--funcion que comprueba si he ganado
ganoYo :: Tablero -> Bool
ganoYo (Tablero f c px py)
    | ganaJugador f c (posicionesMias (Tablero f c px py)) = True
    | otherwise = False

----------------------------------------------------------
----------------------------------------------------------
--------------------FIN-COMPROBACION----------------------
----------------------------------------------------------
----------------------------------------------------------




----------------------------------------------------------
----------------------------------------------------------
--------------------ESTRATEGIA-SMART----------------------
----------------------------------------------------------
----------------------------------------------------------


--dada una columna de un tablero deuelve la posicion libre en esa columna
--columna que quiero mirar -> numero de filas del tablero -> numero de columnas del tavlero -> px -> py -> Posicion
posicionLibreColumna :: Int -> Int -> Int -> Posiciones -> Posiciones -> Posicion
posicionLibreColumna ci f c px py
	| (elem ((f-1), ci) px) || (elem ((f-1), ci) py) = posicionLibreColumna ci (f-1) c px py
	| otherwise = ((f-1),ci)

--devuelve la lista de posiciones libres de un tablero
posicionesLibresPre :: Int -> Tablero -> Posiciones
posicionesLibresPre i (Tablero f c px py)
  | (posDentroTablero (posicionLibreColumna i f c px py) (Tablero f c px py)) && (i < c-1) = [(posicionLibreColumna i f c px py)] ++ (posicionesLibresPre (i+1) (Tablero f c px py))
	| (i < c-1) = (posicionesLibresPre (i+1) (Tablero f c px py))
  | (posDentroTablero (posicionLibreColumna i f c px py) (Tablero f c px py)) && (i == c-1) = [(posicionLibreColumna i f c px py)]
	| otherwise = []

--devuelve la lista de posiciones disponibles de un tablero
posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tablero f c px py) = posicionesLibresPre 0 (Tablero f c px py)


--devuelve la lista de todos los posibles tableros siguientes(1 paso)
tablerosPosibles :: Tablero -> [Tablero]
tablerosPosibles t
	| hayGanador t = []
	| otherwise = map (coloca t) (posicionesLibres t)

--hace el arbol de todas las posibilidades de tableros
generaArbol :: Tablero -> Arbol Tablero
generaArbol a = Nodo a (map generaArbol (tablerosPosibles a))


--funcion que dada una cota de poda de profundidad y un arbol devuelve el arbol podado
podaArbol :: Int -> Arbol a -> Arbol a
podaArbol n (Nodo x as) 
    | n == 0    = Nodo x []
    | otherwise = Nodo x (map (podaArbol (n-1)) as)


--devuelve la lista de las diferentes puntuaciones de un arbol de pair (puntos, tablero)
--donde cada pair representa los puntos que tiene cada tablero (como de buena es la jugada)
puntuaciones :: [Arbol (Puntos,Tablero)] -> [Puntos]
puntuaciones as = [a | Nodo (a, _) _ <- as]


--funcion que devuelve el arbol de (Puntos, Tablero) con los tableros con menos puntos
--funcionamiento basado en el algoritmo minimax
mini :: Arbol Tablero -> Arbol (Puntos,Tablero)
mini (Nodo t [])
    | hayGanador t = Nodo (1,t) []
    | otherwise      = Nodo (0,t) []
mini (Nodo t ts) = Nodo (minimum (puntuaciones l),t) l
    where l = map maxi ts


--funcion que devuelve el arbol de (Puntos, Tablero) con los tableros con mas puntos
--funcionamiento basado en el algoritmo minimax
maxi :: Arbol Tablero -> Arbol (Puntos,Tablero)
maxi (Nodo t []) 
    | hayGanador t = Nodo (-1,t) []
    | otherwise      = Nodo (0,t) []                                        
maxi (Nodo t ts) = Nodo (maximum (puntuaciones l),t) l
    where l = map mini ts



--funcion que selecciona el tablero del primer hijo de la raiz que tiene los mismos puntos que la raiz, de un arbol de (Puntos, Tablero)
cogeTablero :: Arbol (Puntos,Tablero) -> Tablero
cogeTablero (Nodo (v,_) ts) = 
    head [t | Nodo (v',t) _ <- ts, v'==v]


--funcion que dado un tablero retorna el tablero con el mejor movimiento posible dentro de su busqueda
mejorOpcion :: Tablero -> Tablero
mejorOpcion = 
    cogeTablero . maxi . podaArbol profundidadDeBusqueda . generaArbol


----------------------------------------------------------
----------------------------------------------------------
------------------FIN-ESTRATEGIA-SMART--------------------
----------------------------------------------------------
----------------------------------------------------------


----------------------------------------------------------
----------------------------------------------------------
-------------------ESTRATEGIA-GREEDY----------------------
----------------------------------------------------------
----------------------------------------------------------


--funcion que comprueba si el jugador contrario tiene 3 en raya
contrarioTiene3 :: Tablero -> Bool
contrarioTiene3 (Tablero f c px py)
    | (compruebaHorizontal 3 f (posicionesContrario (Tablero f c px py))) = True
    | (compruebaVertical 3 c (posicionesContrario (Tablero f c px py))) = True
    | (compruebaDiagonales1 3 f c (posicionesContrario (Tablero f c px py))) = True
    | (compruebaDiagonales2 3 f c (posicionesContrario (Tablero f c px py))) = True
    | otherwise = False


--funcion que comprueba si el jugador contrario tiene 3 en raya
jugadorTiene3 :: Int -> Int -> Posiciones -> Int
jugadorTiene3 f c p
    | (compruebaHorizontal 3 f p) = 1
    | (compruebaVertical 3 c p) = 2
    | (compruebaDiagonales1 3 f c p) = 31
    | (compruebaDiagonales2 3 f c p) = 32
    | otherwise = -1



--funcion que dadas filas columnas posicionesDisponibles para colocar ese turno y una lista de posiciones
--devuelve la posicion que hace 4 en raya si la hay, sino devuelve (-1,-1)
mirarCuatro :: Int -> Int -> Posiciones -> Posiciones -> Posicion
mirarCuatro f c [] psContrario = (-1,-1)
mirarCuatro f c (x:psLibres) psContrario 
    | (ganaJugador f c (x:psContrario)) = x
    | otherwise = mirarCuatro f c psLibres psContrario

--funcion que comprueba si hay que evitar un 4 en raya del contrincante
evitarContrarioBool :: Tablero -> Bool
evitarContrarioBool (Tablero f c px py)
    | ((mirarCuatro f c (posicionesLibres (Tablero f c px py)) (posicionesContrario(Tablero f c px py))) == (-1,-1)) = False
    | otherwise = True

--funcion que devuelve la posicion correspondiente a evitar un 4 en raya del contrincante
app1Completo :: Tablero -> Posicion
app1Completo (Tablero f c px py)
    | evitarContrarioBool (Tablero f c px py) = (mirarCuatro f c (posicionesLibres (Tablero f c px py)) (posicionesContrario(Tablero f c px py)))
    | otherwise = (-1,-1)

----------------------------------------------------------
----------------------------------------------------------

--dado una posicion y una lista de posiciones mira si la primera posicion hace 2 en raya con alguna de las de la lista
hay2EnRayaLista :: Int -> Int -> Posicion -> Posiciones -> Bool
hay2EnRayaLista f c p [] = False
hay2EnRayaLista f c p (x:ps)
    | hay2EnRaya f c [p,x] = True
    | otherwise = hay2EnRayaLista f c p ps

--dado una posicion y una lista de posiciones mira si la primera posicion hace 3 en raya con alguna de las de la lista
hay3EnRayaLista :: Int -> Int -> Posicion -> [Posiciones] -> Bool
hay3EnRayaLista f c p [] = False
hay3EnRayaLista f c p (x:ps)
    | hay3EnRaya f c ([p] ++ x) = True
    | otherwise = hay3EnRayaLista f c p ps

--funcion que devuelve una lista de listas donde cada lista interior es una pareja de posiciones que hacen 2 en raya
listaListas2 :: Int -> Int -> Posiciones -> [Posiciones]
listaListas2 f c [] = [[]]
listaListas2 f c (x:ps) = listaListas3 f c x ps ++ listaListas2 f c ps

--funcion que dada una posicion y una lista de posiciones devuelve la lista de listas de las parejas que hacen 2 en raya con esa posicion
listaListas3 :: Int -> Int -> Posicion -> Posiciones -> [Posiciones]
listaListas3 f c p [] = [[]]
listaListas3 f c p (x:ps)
    | hay2EnRaya f c [p, x] = [[p,x]] ++ listaListas3 f c p ps
    | otherwise = listaListas3 f c p ps

--funcion que comprueba si puedo hacer un 4 en raya
mirarCuatroMiasBool :: Tablero -> Bool
mirarCuatroMiasBool (Tablero f c px py)
    | ((mirarCuatro f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py))) == (-1,-1)) = False
    | otherwise = True

--funcion que dadas filas columnas posicionesDisponibles para colocar ese turno y una lista de posiciones de las fichas colocadas del jugadr
--devuelve la posicion que hace 3 en raya si la hay, sino devuelve (-1,-1)
mirarTres :: Int -> Int -> Posiciones -> Posiciones -> Posicion
mirarTres f c [] psContrario = (-1,-1)
mirarTres f c (x:psLibres) psContrario 
    | (hay3EnRayaLista f c x (listaListas2 f c psContrario)) = x
    | otherwise = mirarTres f c psLibres psContrario

--funcion que comprueba si puedo hacer un 3 en raya
mirarTresMiasBool :: Tablero -> Bool
mirarTresMiasBool (Tablero f c px py)
    | ((mirarTres f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py))) /= (-1,-1)) = True
    | otherwise = False

--funcion que dadas filas columnas posicionesDisponibles para colocar ese turno y una lista de posiciones de las fichas colocadas del jugadr
--devuelve la posicion que hace 3 en raya si la hay, sino devuelve (-1,-1)
mirarDos :: Int -> Int -> Posiciones -> Posiciones -> Posicion
mirarDos f c [] psContrario = (-1,-1)
mirarDos f c (x:psLibres) psContrario 
    | (hay2EnRayaLista f c x psContrario) = x
    | otherwise = mirarDos f c psLibres psContrario

--funcion que comprueba si puedo hacer un 2 en raya
mirarDosMiasBool :: Tablero -> Bool
mirarDosMiasBool (Tablero f c px py)
    | ((mirarDos f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py))) /= (-1,-1)) = True
    | otherwise = False

--gestion en caso de poder completar un 4 en raya
caso4 :: Tablero -> Posicion
caso4 (Tablero f c px py)
    | (mirarCuatroMiasBool (Tablero f c px py)) = (mirarCuatro f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py)))
    | otherwise = (-1,-1)

--gestion en caso de poder completar un 3 en raya
caso3 :: Tablero -> Posicion
caso3 (Tablero f c px py)
    | (mirarTresMiasBool (Tablero f c px py)) = (mirarTres f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py)))
    | otherwise = (-1,-1)

--gestion en caso de poder completar un 2 en raya
caso2 :: Tablero -> Posicion
caso2 (Tablero f c px py)
    | (mirarDosMiasBool (Tablero f c px py)) = (mirarDos f c (posicionesLibres (Tablero f c px py)) (posicionesMias(Tablero f c px py)))
    | otherwise = (-1,-1)

casoArbitrario :: Tablero -> Posicion
casoArbitrario (Tablero f c px py) = head (posicionesLibres (Tablero f c px py))

--funcion que devuelve la posicion en caso de tener que hacer el 'x' en raya mas largo posible
approach2 :: Tablero -> Posicion
approach2 t
    | (caso3 t) /= (-1,-1) = caso3 t
    | (caso2 t) /= (-1,-1) = caso2 t
    | otherwise = casoArbitrario t


--------------------------------------------------------------

--funcion que devuelve la siguiente posicion segun la estrategia greedy
greedyMovimiento :: Tablero -> Posicion
greedyMovimiento t
    | (caso4 t) /= (-1,-1) = caso4 t
    | (app1Completo t /= (-1,-1)) = app1Completo t
    | otherwise = approach2 t

--funcion que dado un tablero y una posicion, genera el nuevo tablero con la posicion añadida a mi lista de posiciones
genTab :: Tablero -> Posicion -> Tablero
genTab (Tablero f c px py) s
    | soyX (Tablero f c px py) = Tablero f c (s:px) py
    | otherwise = Tablero f c px (s:py)

greedyTablero :: Tablero -> Tablero
greedyTablero t = genTab t (greedyMovimiento t)


----------------------------------------------------------
----------------------------------------------------------
-------------------ESTRATEGIA RANDOM----------------------
----------------------------------------------------------
----------------------------------------------------------

--funcion que dada una columna devuelve la fila de la posicion libre de esa columna
filaCol :: Tablero -> Int -> Posicion
filaCol t c 
    | [ x | x <- (posicionesLibres t), (second x) == c] /= [] = head [ x | x <- (posicionesLibres t), (second x) == c]
    | otherwise = (-1,-1)

--funcion que devuelve la posicion de la estrategia random
randomPos :: Tablero -> Int -> Posicion
randomPos t c
    | length (posicionesLibres t) == 1 = head (posicionesLibres t)
    | filaCol t c == (-1, -1) = head (posicionesLibres t)
    | otherwise = filaCol t c

----------------------------------------------------------
----------------------------------------------------------
-------------------MOSTRAR-EL-TABLERO---------------------
----------------------------------------------------------
----------------------------------------------------------

separacionColumnas :: Int -> String
separacionColumnas 1 = "---"
separacionColumnas x = "--" ++ separacionColumnas (x-1)

muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tablero f c xs ys) p 
    | p `elem` xs = "|X"
    | p `elem` ys = "|Y"
    | otherwise   = "| "


muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t = 
    (concat . map (muestraPosicion t))


muestraTableroFilasColumnas :: Int -> Int -> Tablero -> String
muestraTableroFilasColumnas f c t
    | f-1 > 0 = (muestraTableroFilasColumnas (f-1) c t) ++ "\n" ++ (separacionColumnas c) ++ "\n" ++ muestraLinea t (generaFilaDet (f-1) c) ++ "|"
    | otherwise = muestraLinea t (generaFilaDet 0 c) ++ "|"



muestraTablero :: Tablero -> String
muestraTablero (Tablero f c xs ys) = (muestraTableroFilasColumnas f c (Tablero f c xs ys))


----------------------------------------------------------
----------------------------------------------------------
------------------------CONTROLES-------------------------
----------------------------------------------------------
----------------------------------------------------------


main :: IO ()
main = do
  putStrLn "4 EN RAYA"
  putStrLn "Escoge la estrategia que seguira el ordenador: (Smart/Greedy/Random)"
  e <- getLine    
  putStrLn "Escoge las filas del tablero:"   
  f <- getLine    
  putStrLn "Escoge las columnas del tablero:" 
  c <- getLine      
  putStrLn (muestraTablero (tableroIni (atoi f) (atoi c)))   
  putStrLn "Tiras primero? (s/n) "        
  x <- getLine                               
  if x == "s"                      
     then persona e (tableroIni (atoi f) (atoi c))           
     else ordenador e (tableroIni (atoi f) (atoi c))      



persona :: Estrategia -> Tablero -> IO ()
persona e t = do 
  putStrLn "\nEn que columna colocas ficha? (de 1 al numero de columnas): " 
  x <- getLine
  if not (columnaCorrecta ((atoi x)-1) t)
    then do
      putStr "Indica una columna que no este completa y que este dentro del tablero."
      persona e t
    else do  
      let p1 = columnaPos ((atoi x)-1) t
      if (not (posDisponible p1 t)) 
         then do
          putStr "Indica una columna que no este completa y que este dentro del tablero."
          persona e t                                   
         else do
          let t2 = coloca t p1    
          putStrLn (muestraTablero t2)                   
          if hayGanador t2                               
            then putStrLn "Gana persona"                 
            else if completo t2                         
               then putStrLn "Empate"             
               else ordenador e t2                   


ordenador :: Estrategia -> Tablero -> IO ()
ordenador e (Tablero f c px py) = do
  putStrLn "\nMovimiento ordenador:"            
  if e == "Greedy"
    then do
      let t2 = greedyTablero (Tablero f c px py)         
      putStrLn (muestraTablero t2)       
      if hayGanador t2                 
         then putStrLn "Gana ordenador"    
         else if completo t2           
             then putStrLn "Empate" 
             else persona e t2 
  else if e == "Smart"
    then do
      let t2 = mejorOpcion (Tablero f c px py)         
      putStrLn (muestraTablero t2)       
      if hayGanador t2                 
         then putStrLn "Gana ordenador"    
         else if completo t2           
             then putStrLn "Empate" 
             else persona e t2 
  else
   do 
    r1 <- (randInt 0 (c-1))
    let pos = randomPos (Tablero f c px py) r1
    let t2 = genTab (Tablero f c px py) pos
    putStrLn (muestraTablero t2)
    if hayGanador t2                 
         then putStrLn "Gana ordenador"    
         else if completo t2           
             then putStrLn "Empate" 
             else persona e t2