module MapReduce where

import Data.List
import Data.Ord
import Test.HUnit

import Data.Function ((&))

-- ---------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k, v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs e = foldr (\(k, _) rec -> (k == e) || rec) False

(?) :: Eq k => Dict k v -> k -> Bool
(?) = flip belongs

--Main> [("calle",[3]),("city",[2,1])] ? "city"
--True

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get e = foldr (\(k, v) r -> if k == e then v else r) (error "key not found")

(!) :: Eq k => Dict k v -> k -> v
(!) = flip get

--Main> [("calle",[3]),("city",[2,1])] ! "city"
--[2,1]

-- Ejercicio 3
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f e_k e_v d
    | d ? e_k = foldr (\(k, v) r -> if k == e_k then (k, f v e_v) : r else (k, v) : r) [] d
    | otherwise = d ++ [(e_k, e_v)]

--Main> insertWith (++) 2 ['p'] (insertWith (++) 1 ['a','b'] (insertWith (++) 1 ['l'] []))
--[(1,"lab"),(2,"p")]

-- Ejercicio 4
groupByKey :: Eq k => [(k, v)] -> Dict k [v]
groupByKey = foldl (\rec (k, v) -> insertWith (++) k [v] rec) []

-- Main> groupByKey [("calle","Jean Jaures"),("ciudad","Brujas"), ("ciudad","Kyoto"),("calle","7")]
-- [("calle",["Jean Jaures","7"]),("ciudad",["Brujas","Kyoto"])]

-- Ejercicio 5
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f d1 d2 = foldl (\rec (k, v) -> insertWith f k v rec) d2 d1

--Main> unionWith (++) [("calle",[3]),("city",[2,1])] [("calle", [4]), ("altura", [1,3,2])]
--[("calle",[3,4]),("city",[2,1]),("altura",[1,3,2])]

-- ------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k, v)]
type Reducer k v b = (k, [v]) -> [b]

-- Ejercicio 6
rotate xs = last xs : init xs

distributionProcess :: Int -> [a] -> [[a]]
distributionProcess n = if n <= 0 then error "at least 1 partiton required" else foldl add_to_split (replicate n [])
  where
    add_to_split (h : t) x = rotate ((h ++ [x]) : t)

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> Dict k [v]
mapperProcess m = groupByKey . concatMap m

-- Ejercicio 8
combinerProcess :: (Eq k, Ord k) => [Dict k [v]] -> Dict k [v]
combinerProcess = sortBy (\(k1, _) (k2, _) -> if k1 < k2 then LT else GT) . foldr (unionWith (++)) []

-- Ejercicio 9
reducerProcess :: Reducer k v b -> Dict k [v] -> [b]
reducerProcess = concatMap

-- Ejercicio 10
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce mapper reducer xs =
    distributionProcess 100 xs
        & map (mapperProcess mapper)
        & combinerProcess
        & reducerProcess reducer

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
palabras = [[(1, ["Hola", "Chau"]), (2, ["Perro", "Gato"])], [(2, ["Jirafa"])], [(3, ["Casa"]), (4, ["Tren", "Auto"]), (1, ["Saludos"])], [(2, ["Perro"]), (4, ["Barco"])]]

-- Monumentos por país

mapperMPP :: Mapper (Structure, Dict String String) String ()
mapperMPP (Monument, metadata) =
    let country = metadata ! "country"
     in [(country, ())]
mapperMPP _ = []

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
monumentosTop = ordenarPorVisitas . visitasPorMonumento

-- ------------------------ Ejemplo de datos para pruebas ----------------------
data Structure = Street | City | Monument deriving (Show)

items :: [(Structure, Dict String String)]
items =
    [
        ( Monument
        ,
            [ ("name", "Obelisco")
            , ("latlong", "-36.6033,-57.3817")
            , ("country", "Argentina")
            ]
        )
    ,
        ( Street
        ,
            [ ("name", "Int. Güiraldes")
            , ("latlong", "-34.5454,-58.4386")
            , ("country", "Argentina")
            ]
        )
    ,
        ( Monument
        ,
            [ ("name", "San Martín")
            , ("country", "Argentina")
            , ("latlong", "-34.6033,-58.3817")
            ]
        )
    ,
        ( City
        ,
            [ ("name", "Paris")
            , ("country", "Francia")
            , ("latlong", "-24.6033,-18.3817")
            ]
        )
    ,
        ( Monument
        ,
            [ ("name", "Bagdad Bridge")
            , ("country", "Irak")
            , ("new_field", "new")
            , ("latlong", "-11.6033,-12.3817")
            ]
        )
    ]

------------------------------------------------
------------------------------------------------

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests =
    test
        [ "ejercicio1" ~: testsEj1
        , "ejercicio2" ~: testsEj2
        , "ejercicio3" ~: testsEj3
        , "ejercicio4" ~: testsEj4
        , "ejercicio5" ~: testsEj5
        , "ejercicio6" ~: testsEj6
        , "ejercicio7" ~: testsEj7
        , "ejercicio8" ~: testsEj8
        , "ejercicio9" ~: testsEj9
        , "ejercicio10" ~: testsEj10
        ]

testsEj1 =
    test
        [ True ~=? ([("calle", [3]), ("ciudad", [2, 1])] ? "ciudad")
        , False ~=? ([("calle", [3]), ("ciudad", [2, 1])] ? "perro")
        , False ~=? ([("lampara", [50]), ("escritorio", [12])] ? "ciudad")
        , False ~=? ([("lampara", [50])] ? "escritorio")
        , False ~=? ([] ? "calle")
        , True ~=? ([("calle", [50])] ? "calle")
        ]

testsEj2 =
    test
        [ "Hurlingham" ~=? [("calle", "San Blas"), ("ciudad", "Hurlingham")] ! "ciudad"
        , [50] ~=? ([("calle", [50])] ! "calle")
        ]

testsEj3 =
    test
        [ [(1, [1, 99]), (2, [2])] ~=? insertWith (++) 1 [99] [(1, [1]), (2, [2])]
        , [(1, [1]), (2, [2]), (3, [99])] ~=? insertWith (++) 3 [99] [(1, [1]), (2, [2])]
        , [(1, [1])] ~=? insertWith (++) 1 [1] [(1, [])]
        , [(1, [1, 2])] ~=? insertWith (++) 1 [2] (insertWith (++) 1 [1] [])
        , [(1, [1, 1, 2, 3, 4])] ~=? insertWith (++) 1 [1, 2, 3, 4] [(1, [1])]
        , [(1, [1, 2, 3, 4])] ~=? insertWith union 1 [1, 2, 3, 4] [(1, [1])]
        ]

testsEj4 =
    test
        [ [("calle", ["Jean Jaures", "7"]), ("ciudad", ["Brujas", "Kyoto"])] ~=? groupByKey [("calle", "Jean Jaures"), ("ciudad", "Brujas"), ("ciudad", "Kyoto"), ("calle", "7")]
        , [("calle", [1, 2, 4]), ("ciudad", [3, 5])] ~=? groupByKey [("calle", 1), ("calle", 2), ("ciudad", 3), ("calle", 4), ("ciudad", 5)]
        ]

testsEj5 =
    test
        [ [("rutas", 7), ("ciclos", 1)] ~=? unionWith (+) [("rutas", 3)] [("rutas", 4), ("ciclos", 1)]
        , [("rutas", [1, 2, 3]), ("ciclos", [4, 5, 6])] ~=? unionWith union [("rutas", [1, 2, 3])] [("rutas", [1, 2, 3]), ("ciclos", [4, 5, 6])]
        ]

testsEj6 =
    test
        [ [[3, 8], [2, 7, 12], [1, 6, 11], [5, 10], [4, 9]] ~=? distributionProcess 5 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        , [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]] ~=? distributionProcess 1 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        , ([[]] :: [[Int]]) ~=? distributionProcess 1 []
        , ([[], [], []] :: [[Int]]) ~=? distributionProcess 3 []
        ]

testsEj7 =
    test
        [ [(1, [1]), (0, [5, 10, 25]), (3, [3]), (4, [14, 4])] ~=? mapperProcess mapperRestos [1, 5, 10, 25, 3, 14, 4]
        , [] ~=? mapperProcess mapperRestos []
        ]

testsEj8 =
    test
        [ [(1, ["Chau", "Hola", "Saludos"]), (2, ["Gato", "Jirafa", "Perro", "Perro"]), (3, ["Casa"]), (4, ["Auto", "Barco", "Tren"])] ~=? (map (\(x, y) -> (x, sort y)) $ combinerProcess palabras)
        , [("cuatro cinco seis", [4, 5, 6]), ("uno dos tres", [1, 2, 3])] ~=? (map (\(x, y) -> (x, sort y)) $ combinerProcess [[("uno dos tres", [1]), ("cuatro cinco seis", [4, 5]), ("uno dos tres", [2])], [("cuatro cinco seis", [6]), ("uno dos tres", [3])]])
        ]

testsEj9 =
    test
        [ ["Saludo:", "Chau", "Hola", "Saludos", "Mamífero:", "Gato", "Jirafa", "Perro", "Edificio:", "Casa", "Vehículo:", "Auto", "Barco", "Tren"] ~=? reducerProcess (\(x, xs) -> x : nub xs) [("Saludo:", ["Chau", "Hola", "Saludos"]), ("Mamífero:", ["Gato", "Jirafa", "Perro", "Perro"]), ("Edificio:", ["Casa"]), ("Vehículo:", ["Auto", "Barco", "Tren"])]
        , ([] :: [String]) ~=? reducerProcess (\(x, xs) -> x : nub xs) []
        ]

testsEj10 =
    test
        [ [("m1", 1), ("m2", 2), ("m3", 1)] ~=? sort (visitasPorMonumento ["m1", "m2", "m3", "m2"])
        , sort (monumentosPorPais items) ~=? [("Argentina", 2), ("Irak", 1)]
        , ["m3", "m2", "m1", "m4"] ~=? monumentosTop ["m3", "m2", "m2", "m3", "m1", "m2", "m3", "m3", "m4", "m1"]
        , ([] :: [String]) ~=? monumentosTop []
        ]
