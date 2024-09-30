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
digitos :: Int -> [Int]
digitos = undefined

--Actividad 32:
cumpleCon :: [Int]
cumpleCon = [n | n<-[1..5000], n `mod` 1 == 0 && n `mod` 2 == 1 && n `mod` 3 == 2 && n `mod` 4 == 3 && n `mod` 5 == 4 && n `mod` 6 == 5 && n `mod` 7 == 6 && n `mod` 8 == 7 && n `mod` 9 == 8 && n `mod` 10 == 9]
