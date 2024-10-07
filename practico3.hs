--Actividad 1:
hd :: [a] -> a
hd [] = error "Lista vacía"
hd (x:xs) = x

tl :: [a] -> [a]
tl [] = error "Lista vacía"
tl (x:xs) = xs

lst :: [a] -> a
lst [] = error "Lista vacía"
lst [x] = x
lst (x:xs) = lst xs
-- lst xs = hd(reverse xs)

init2 :: [a] -> [a]
init2 [] = []
init2 [x] = []
init2 (x:xs) = x:(init2 xs)

--Actividad 2:
reversa :: [a] -> [a]
reversa [] = []
--reversa [x] = [x] --Redundante, puede encajar en el caso siguiente (porque 3 = 3:[])
reversa (x:xs) = (reversa xs) ++ (x:[])

--Actividad 3:
iguales :: Eq a => [a] -> [a] -> Bool
iguales [] [] = True
iguales [] (x:xs) = False --Para evitar falta de patterns
iguales (x:xs) [] = False --Para evitar falta de patterns
iguales (x:xs) (y:ys) = if (length(x:xs) == length(y:ys)) && x == y then iguales xs ys else False

--Actividad 4:
palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo xs = if xs == reversa(xs) then True else False

--Actividad 5:
esPrimo :: Int -> Bool
esPrimo x = listaDivisores x == [1, x]

--Adicional:
listaDivisores :: Int -> [Int]
listaDivisores x = [n | n<-[1..x], x `mod` n == 0]

--Actividad 6:
listaPrimosMenores :: Int -> [Int]
listaPrimosMenores n = [x | x<-[1..n], n `mod` x == 0 && (x `div` n) < n]

--Actividad 7:
listaPrimos :: Int -> [Int]
listaPrimos n = [x | x<-[1..n], esPrimo x]

--Actividad 8:
cortar :: Int -> Int -> [Char] -> [Char]
cortar = undefined

--Actividad 9:
{-Otra forma:
digitos n | n<0 = digitos (abs n)
          | n<10 = [n]  
          | otherwise = digitos (n `div` 10) ++ [n `mod` 10]
-}

{- digitos 0 = []
   digitos x | x < 10 = [] 
    -}

digitos :: Int -> [Int]
digitos n = digitos2 (abs n)

digitos2 :: Int -> [Int]
digitos2 n | n<10 = [n]
           | otherwise = digitos2 (n `div` 10) ++ [n `mod` 10]

cantDigitos :: Int -> Int
cantDigitos x = length(show x) 

--Actividad 11: Generar una lista infinita de unos.
infUnos :: [Int]
infUnos = [1,1..] 

-- Actividad 12: Generar una lista infinita de naturales comenzando desde un número dado.
infNs :: Int -> [Int]
infNs n = [n, n+1..] 

--Actividad 13: Generar una lista con los primeros n naturales.
natDesdeN :: Int -> [Int]
natDesdeN n = [1..n] 

--Actividad 14: (*) Retornar los primeros 5 elementos de una lista infinita de enteros positivos.
primerosCinco :: [Int]
primerosCinco = take 5 [1,2..] 

-- | FUNCIONES DE ALTO ORDEN:

-- Actividad 15: Dada una lista de enteros, retornar la lista de sus cuadrados:
listaCuadrados :: [Int] -> [Int]
listaCuadrados [] = []
listaCuadrados (x:xs) = map (^2) (x:xs)

-- Actividad 16: Dado un entero positivo, retornar la lista de sus divisores.
listaDivisores' :: Int -> [Int]
listaDivisores' = undefined 

-- Actividad 17: Dada una lista de naturales, obtener la lista que contenga solo los números primos de la lista original.
soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos (x:xs) = filter esPrimo (x:xs)

-- Actividad 18: Dada una lista de naturales, retornar la suma de los cuadrados de la lista:
sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados (x:xs) = foldr (+) 0 (listaCuadrados (x:xs))

-- Actividad 19: Dada una lista de naturales, retornar la lista con sus sucesores:
listaSucc :: [Int] -> [Int]
listaSucc [] = []
listaSucc (x:xs) = map (+ 1) (x:xs)
 
--Actividad 20: Dada una lista de enteros, sumar todos sus elementos:
sumaElementos :: [Int] -> Int
sumaElementos [] = 0
sumaElementos (x:xs) = foldr (+) 0 (x:xs)

--Actividad 21: Definir el factorial usando fold:
factorialFold :: Int -> Int
factorialFold 0 = 1
factorialFold x = foldr (*) 1 [x, (x-1)..1]

--Actividad 22: Redefinir la función and tal que and xs se verifica si todos los elementos de xs son verdaderos.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = foldr (&&) True (x:xs)

{-Actividad 23: Usando foldl o foldr definir una función tam::[a]->Int que devuelve
la cantidad de elementos de una lista dada. Dar un ejemplo en los cuales
foldr y foldl evaluen diferente con los mismos parametros.
-}
--hint: Función que incremente la izquierda.
tamFoldR :: [a] -> Int
tamFoldR [] = 0
tamFoldR (x:xs) = undefined

--Utilizando listas por comprensión resolver:
--Actividad 24: Dada una lista de enteros, retornar sus sucesores:
listaComprensionSucc :: [Int] -> [Int]
listaComprensionSucc [] = []
listaComprensionSucc (x:xs) = drop 1 (x:([n | n<-[(minimum (x:xs))..(maximum (x:xs) + 1)], n == x+1])) ++ listaComprensionSucc(xs)

--Actividad 32:
cumpleCon :: [Int]
cumpleCon = [n | n<-[1..5000], n `mod` 1 == 0 && n `mod` 2 == 1 && n `mod` 3 == 2 && n `mod` 4 == 3 && n `mod` 5 == 4 && n `mod` 6 == 5 && n `mod` 7 == 6 && n `mod` 8 == 7 && n `mod` 9 == 8 && n `mod` 10 == 9]

