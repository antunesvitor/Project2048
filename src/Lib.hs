module Lib
    ( arrastar,
    checkArrastar,
    soma,
    deslizar,
    transposta,
    comparaEstadoLinha,
    comparaEstado,
    checkMovimentosPossiveis,
    checkGameOver,
    decide,
    swipeUp,
    swipeDown,
    swipeLeft,
    swipeRight
    ) where
    
import Control.Concurrent

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


pegarColuna :: (Eq a, Num a) => [[a]] -> Int -> [a]
pegarColuna [[]] _ = []
pegarColuna xss i 
    |i >= length (head xss) = []
    |otherwise = [xs !! i | xs <- xss]


transposta :: (Eq a, Num a) => [[a]] -> [[a]]
transposta [] = []
transposta xss = recRot xss 0

recRot :: (Eq a, Num a) => [[a]] -> Int -> [[a]]
recRot [] _ = []
recRot xss i 
    | coluna == [] = []
    | otherwise = coluna:(recRot xss (i+1))
    where
        coluna = pegarColuna xss i

-- A funcao que recebe o tabuleiro inteiro e retorna ele com um deslize pra direita
swipeRight :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeRight [] = []
swipeRight xss = map deslizar xss

-- A funcao que recebe o tabuleiro inteiro e retorna ele com um deslize pra esquerda
swipeLeft :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeLeft [] = []
swipeLeft xss = map reverse $ map deslizar ssx
    where
        ssx = map reverse xss

swipeDown :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeDown [] = []
swipeDown xss = transposta $ swipeRight xssT
    where
        xssT = transposta xss

swipeUp :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeUp [] = []
swipeUp xss = transposta $ swipeLeft xssT
    where
        xssT = transposta xss
    
comparaEstadoLinha :: (Num a, Eq a) => [a] -> [a] -> Bool
comparaEstadoLinha [] [] = True                                 -- Quer dizer que ambas as listas esvariaram logo são iguais
comparaEstadoLinha [] ys = False                                -- Quer dizer que apenas uma esvaziou e outra possui itens, trivialmente são diferentes
comparaEstadoLinha xs [] = False                                -- idem
comparaEstadoLinha (x:xs) (y:ys)
    | y == x = comparaEstadoLinha xs ys
    | otherwise = False                                         -- Se apenas um item for diferente então é falso

comparaEstado :: (Num a, Eq a) => [[a]] -> [[a]] -> Bool
comparaEstado [] []  = True
comparaEstado [] yss = False
comparaEstado xss [] = False
comparaEstado (xs:xss) (ys:yss) 
    | comparaEstadoLinha xs ys = comparaEstado xss yss
    | otherwise = False

canSwipeRight :: (Num a, Eq a) => [[a]] -> Bool
canSwipeRight xss = not $ comparaEstado rightSwiped xss
    where
        rightSwiped = swipeRight xss

canSwipeLeft ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeLeft xss = not $ comparaEstado leftSwiped xss
    where
        leftSwiped = swipeLeft xss

canSwipeUp ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeUp xss = not $ comparaEstado upSwiped xss
    where
        upSwiped = swipeUp xss

canSwipeDown ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeDown xss = not $ comparaEstado downSwiped xss
    where
        downSwiped = swipeDown xss

checkMovimentosPossiveis :: (Num a, Eq a) => [[a]] -> [Bool]
checkMovimentosPossiveis xss = [canSwipeUp xss] ++ [canSwipeRight xss] ++ [canSwipeDown xss] ++ [canSwipeLeft xss]

allFalse :: [Bool] -> Bool
allFalse [] = True
allFalse [x] = not x
allFalse (x:xs)
    | not x = (not x) && allFalse xs
    | otherwise = False

checkGameOver :: (Num a, Eq a) => [[a]] -> Bool
checkGameOver xss = allFalse $ checkMovimentosPossiveis xss

-- a maquina joga, recebe um tabuleiro e devolve ele movido, assumimos que ele tem algum movimento disponível(no jogo isso será checado antes dessa função ser chamada)
decide :: (Num a, Eq a) => [[a]] -> [[a]]
decide xss 
    | canSwipeUp xss = swipeUp xss
    | canSwipeRight xss = swipeRight xss
    | canSwipeLeft xss = swipeLeft xss
    | canSwipeDown xss = swipeDown xss
    | otherwise = xss

decideDelay :: (Num a, Eq a) => [[a]] -> IO [[a]]
decideDelay  xss = do
    threadDelay 500000
    let newXss = decide xss
    return newXss
