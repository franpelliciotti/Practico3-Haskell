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

esPrimo :: Int -> Bool
esPrimo x = listaDivisores x == [1, x]
listaDivisores :: Int -> [Int]
listaDivisores x = [n | n<-[1..x], x `mod` n == 0]
