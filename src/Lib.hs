module Lib
    ( arrastar,
    checkArrastar,
    soma,
    deslizar
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

shift :: (Eq a, Num a) => [a] -> [a]
shift [] = []
shift [x] = [x]
shift xs
    | ult /= 0 = (shift (init xs)) ++ [ult] -- o ultimo sendo /= zero, ele permanece
    | penult /= 0 = (shift (init $ init xs)) ++ [0,penult] -- nesse caso o ult é zero, entãp ele é excluído
    | otherwise = (shift (init xs)) ++ [0]--caso o ultimon e penultimo iguais a zero: prossegue e os mantenha assim até o fim
    where 
        ult = last xs
        penult = last $ init xs

checkArrastar :: (Eq a, Num a) => [a] -> Bool
checkArrastar [] = True
checkArrastar [x] = True
checkArrastar (x:xs)
    | x /= 0 && head xs == 0 = False
    | otherwise = checkArrastar xs

arrastar :: (Eq a, Num a) => [a] -> [a]
arrastar [] = []
arrastar [x] = [x]
arrastar xs
    | checkArrastar xs = xs
    | otherwise = arrastar xsArrastado
    where 
        xsArrastado = shift xs

soma :: (Eq a, Num a) => [a] -> [a]
soma [] = []
soma [x] = [x]
soma xs
    | ult /= penult = (soma (init xs)) ++ [ult]
    | otherwise = (soma (init $ init xs)) ++ [0, ult + penult]
    where
        ult = last xs
        penult = last $ init xs


arrastaESoma :: (Eq a, Num a) => [a] -> [a]
arrastaESoma [] = []
arrastaESoma [x] = [x]
arrastaESoma xs
    | okArrasto = soma xs
    | otherwise = arrastaESoma (arrastar xs)
    where
        okArrasto = checkArrastar xs

deslizar :: (Eq a, Num a) => [a] -> [a]
deslizar [] = []
deslizar [x] = [x]
deslizar xs
    | okArrasto = xsModificada
    | otherwise = arrastar xsModificada
    where
        xsModificada = arrastaESoma xs
        okArrasto = checkArrastar $ xsModificada
        