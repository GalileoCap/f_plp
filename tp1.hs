module MapReduce where

import Data.Ord
import Data.List
import Test.HUnit

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
-- Itera sobre todo el diccionario, una vez encuentra la clave buscada siempre devuelve True
belongs key dict = foldr hasKey False dict 
                 where hasKey (x, _) res = res || (x == key)

(?) :: Eq k => Dict k v -> k -> Bool
(?) dict key = belongs key dict
--Main> [("calle",[3]),("city",[2,1])] ? "city" 
--True

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
-- Itera sobre todo el diccionario, una vez que encuentra la clave buscada devuelve su valor
get key ((_, firstValue):dict) = foldr getKey firstValue dict
                               where getKey (x, thisValue) res = if x == key then thisValue else res


-- get key dict = getValue (foldr1 getKey dict)
--              where getKey (x, thisValue) res = if x == key then (x, thisValue) else res
--                    getValue (_, value) = value



(!) :: Eq k => Dict k v -> k -> v
(!) dict key = get key dict
--Main> [("calle",[3]),("city",[2,1])] ! "city" 
--[2,1]

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
-- Itera sobre todo el diccionario, y en el caso que encuentre la clave que se quiere cambiar le aplica la operación.
insertWith op key newValue dict = if dict ? key then map applyOp dict else (key, newValue):dict
                                where applyOp (thisKey, thisValue) = (thisKey, if thisKey == key then op thisValue newValue else thisValue)

--Main> insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] []))
--[(1,"lab"),(2,"p")]

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
-- Itera sobre el diccionario insertando las claves sobre un nuevo diccionario, y en el caso que haya colisión de claves concatena sus valores.
groupByKey dict = foldr (\(key, value) newDict -> insertWith (++) key [value] newDict) [] dict

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
-- Itera sobre d1 agregándolo a d2
unionWith solveColission d1 d2 = foldr (\(key, value) newDict -> insertWith solveColission key value newDict) d2 d1 -- TODO: Preguntar error dando vuelta d2 y d1
--Main> unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])]
--[("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])]


-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]

-- Ejercicio 6
distributionProcess :: Int -> [a] -> [[a]]
-- distributionProcess numberOfBins xs = getBins (foldr distribute (replicate numberOfBins [], 0) xs)
--                                     where distribute elem (bins, thisBin) = (addToBin thisBin elem bins, nextBin thisBin)
--                                           addToBin n elem bins = (take n bins) ++ [(elem:(bins!!n))] ++ (drop (n+1) bins)
--                                           getBins (bins, _) = bins
--                                           nextBin thisBin = mod (thisBin+1) numberOfBins

-- Itera sobre la lista bins agregando al primer bin y mandandolo al fondo para mantener una cantidad pareja.
distributionProcess numberOfBins xs = foldr distribute (replicate numberOfBins []) xs
                                    where distribute elem (bin:bins) = bins ++ [elem:bin]
-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> Dict k [v]
mapperProcess mapper elems = groupByKey (foldr applyMapper [] elems)
                           where applyMapper elem res = (mapper elem) ++ res

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [Dict k [v]] -> Dict k [v]
combinerProcess results = sorted (foldr f [] results)
                        where f newDict res = unionWith (++) newDict res
                              sorted res = sortBy (\(k1, _) (k2, _) -> compare k1 k2) res

-- Ejercicio 9
reducerProcess :: Reducer k v b -> Dict k [v] -> [b]
reducerProcess reducer dict = concat (map reducer dict)

-- Ejercicio 10
mapReduceOver :: (Eq k, Ord k) => Int -> Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduceOver machines mapper reducer elems = reducerProcess reducer (combinerProcess (map (mapperProcess mapper) (distributionProcess machines elems)))

mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce = mapReduceOver 100


--Funciones de prueba --

--Restos módulo 5

mapperRestos :: Mapper Int Int Int
mapperRestos n = [(n `mod` 5, n)]

reducerRestos :: Reducer Int Int (Int, Int)
reducerRestos (r, ns) = [(r, length ns)]

restosMod5 :: [Int] -> Dict Int Int
restosMod5 = mapReduce mapperRestos reducerRestos

--Clasificación de palabras
palabras :: [[(Int, [[Char]])]]
palabras = [[(1,["Hola","Chau"]),(2,["Perro","Gato"])],[(2,["Jirafa"])],[(3,["Casa"]),(4,["Tren", "Auto"]), (1, ["Saludos"])],[(2, ["Perro"]), (4, ["Barco"])]]

-- Monumentos por país

mapperMPP :: Mapper (Structure, Dict String String) String ()
mapperMPP (Monument, metadata) = let country = metadata ! "country"
                                 in [(country, ())]
mapperMPP _                  = []

reducerMPP :: Reducer String () (String, Int)
reducerMPP (country, units) = [(country, length units)]

monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = mapReduce mapperMPP reducerMPP

-- Monumentos top

mapperVPM :: Mapper String String ()
mapperVPM m = [(m, ())]

reducerVPM :: Reducer String () (String, Int)
reducerVPM (monument, units) = [(monument, length units)]

visitasPorMonumento :: [String] -> [(String, Int)]
visitasPorMonumento = mapReduce mapperVPM reducerVPM

mapperOPV :: Mapper (String, Int) Int String
mapperOPV (monument, visitCount) = [(-visitCount, monument)]

-- Acá se utiliza el orden por visitCount que provee mapReduce.
-- Sencillamente se descarta el número para devolver la lista
-- de monumentos ordenada por visitas.
reducerOPV :: Reducer Int String String
reducerOPV = snd

ordenarPorVisitas :: [(String, Int)] -> [String]
ordenarPorVisitas = mapReduce mapperOPV reducerOPV

monumentosTop :: [String] -> [String]
monumentosTop = ordenarPorVisitas.visitasPorMonumento
                

-- ------------------------ Ejemplo de datos para pruebas ----------------------
data Structure = Street | City | Monument deriving Show

items :: [(Structure, Dict String String)]
items = [
    (Monument, [
      ("name","Obelisco"),
      ("latlong","-36.6033,-57.3817"),
      ("country", "Argentina")]),
    (Street, [
      ("name","Int. Güiraldes"),
      ("latlong","-34.5454,-58.4386"),
      ("country", "Argentina")]),
    (Monument, [
      ("name", "San Martín"),
      ("country", "Argentina"),
      ("latlong", "-34.6033,-58.3817")]),
    (City, [
      ("name", "Paris"),
      ("country", "Francia"),
      ("latlong", "-24.6033,-18.3817")]),
    (Monument, [
      ("name", "Bagdad Bridge"),
      ("country", "Irak"),
      ("new_field", "new"),
      ("latlong", "-11.6033,-12.3817")])
    ]


------------------------------------------------
------------------------------------------------

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]
  
testsEj1 = test [
  ([("calle",[3]),("ciudad",[2,1])] ? "ciudad")  ~=? True,
  ([("calle",[3]),("ciudad",[2,1])] ? "perro")  ~=? False, --Agregar sus propios tests.
  ([("calle",[3]),("ciudad",[2,1])] ? "calle")  ~=? True
  ]

testsEj2 = test [
  [("calle","San Blas"),("ciudad","Hurlingham")] ! "ciudad" ~=? "Hurlingham", --Agregar sus propios tests.
  [("calle","San Blas"),("ciudad","Hurlingham")] ! "calle" ~=? "San Blas"
  ]

testsEj3 = test [
  (insertWith (++) 1 [99] [(1, [1]), (2, [2])]) ~=? [(1,[1,99]),(2,[2])], --Agregar sus propios tests.
  (insertWith (++) 1 [99] [(2, [2])]) ~=? [(1,[99]),(2,[2])],
  (insertWith (++) 1 [99, 30] [(1, [1]), (2, [2])]) ~=? [(1,[1, 99, 30]),(2,[2])],
  (insertWith max 1 99 [(1, 30)]) ~=? [(1,99)],
  (insertWith max 1 30 [(1, 88)]) ~=? [(1,88)]
  ]

testsEj4 = test [
  (groupByKey [("calle", "Jean Jaures"), ("ciudad", "Brujas"), ("ciudad", "Kyoto"), ("calle", "7")]) ~=? [("ciudad",["Kyoto","Brujas"]),("calle",["7","Jean Jaures"])],
  (groupByKey [(True, "Pizza"), (False, "Mayonesa"), (True, "Hamburguesa"), (False, "Jungla"), (False, "Tofu")]) ~=? [(True,["Hamburguesa","Pizza"]),(False,["Tofu","Jungla","Mayonesa"])],
  (groupByKey [(3, 6), (3, 9), (3, 15), (7, 21), (7, 35), (3, 15), (3, 21), (7, 14)]) ~=? [(3, [21, 15, 15, 9, 6]),(7, [14, 35, 21])]
  ]

testsEj5 = test [
  (unionWith (+) [("rutas",3)] [("rutas", 4), ("ciclos", 1)]) ~=? [("rutas",7),("ciclos",1)], --Agregar sus propios tests.
  (unionWith (++) [("cubiertas", [4]), ("trompetas", [3])] [("cubiertas", [12])]) ~=? [("trompetas", [3]), ("cubiertas", [12, 4])],
  (unionWith max [("cubiertas", 4), ("trompetas", 3)] [("cubiertas", 12)]) ~=? [("trompetas", 3), ("cubiertas", 12)]
  ]

testsEj6 = test [
  (distributionProcess 5 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]) ~=? [[5,10],[4,9],[3,8],[2,7,12],[1,6,11]],
  (distributionProcess 2 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]) ~=? [[2,4,6,8,10,12],[1,3,5,7,9,11]],
  (distributionProcess 1 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]) ~=? [[1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12]],
  (distributionProcess 12 [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10]) ~=? [[],[],[10],[9],[8],[7],[6],[5],[4],[3],[2],[1]]
  ]

testsEj7 = test [
  mapperProcess mapperRestos [1, 5, 10, 25, 3, 14, 4] ~=? [(1,[1]),(0,[25,10,5]),(3,[3]),(4,[4,14])],
  mapperProcess mapperMPP items ~=? [("Argentina",[(),()]),("Irak",[()])]
  ]

testsEj8 = test [
  (map (\(x,y)->(x,sort y)) $ combinerProcess palabras) ~=? [(1,["Chau","Hola","Saludos"]),(2,["Gato","Jirafa","Perro","Perro"]),(3,["Casa"]),(4,["Auto","Barco","Tren"])], --Agregar sus propios tests.
  combinerProcess [[("Medias", ["L", "XL"]), ("Alfajores", ["Maicena"])], [("Medias", ["S"])], [("Zapatillas", ["41", "42"])], [("Zapatillas", ["36", "37"])]] ~=? [("Alfajores",["Maicena"]),("Medias",["S","L","XL"]),("Zapatillas",["36","37","41","42"])]
  ]

testsEj9 = test [
  reducerProcess (\(x, xs)->x : nub xs)  [("Saludo:",["Chau","Hola","Saludos"]),("Mamífero:",["Gato","Jirafa","Perro","Perro"]),("Edificio:",["Casa"]),("Vehículo:",["Auto","Barco","Tren"])] ~=? ["Saludo:","Chau","Hola","Saludos","Mamífero:","Gato","Jirafa","Perro","Edificio:","Casa","Vehículo:","Auto","Barco","Tren"], --Agregar sus propios tests.
  reducerProcess (\(x, xs)-> if elem True xs then x ++ " " else "") [("Pizza", [True]), ("Queso", [False, False]), ("Harina", [False]), ("Tomate", [True]), ("Panceta", [True, True]), ("Pimienta", [False, True])] ~=? "Pizza Tomate Panceta Pimienta ",
  reducerProcess (\(x, xs)->max x (head xs) : []) [(3, [5]), (7, [5]), (9, [11, 13])] ~=? [5, 7, 11]
  ]

testsEj10 = test [
  sort (visitasPorMonumento ["m1","m2","m3","m2"]) ~=? [("m1",1),("m2",2),("m3",1)],
  [("Argentina",2),("Irak",1)] ~=? sort (monumentosPorPais items),
  monumentosTop ["m3","m2","m2","m3","m1","m2","m3","m3","m4","m1"] ~=? ["m3","m2","m1","m4"] --Agregar sus propios tests.
  ]
