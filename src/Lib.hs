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

-- As quatro Funções principais para deslizar o tabuleiro
        -- Deslizam um tabuleiro para algum lado (Cima, baixo, direta ou esquerda) e então adicionam um novo numero 2 em uma posição que é zero
swipeRight :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeRight xss 
    | canSwipeRight xss = adicionaNovoNumero $ swipeRight' xss
    | otherwise = xss

swipeLeft :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeLeft xss 
    | canSwipeLeft xss = adicionaNovoNumero $ swipeLeft' xss
    | otherwise = xss

swipeDown :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeDown xss 
    | canSwipeDown xss = adicionaNovoNumero $ swipeDown' xss
    | otherwise = xss

swipeUp :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeUp xss 
    | canSwipeUp xss = adicionaNovoNumero $ swipeUp' xss
    | otherwise = xss

-- Funcões Auxiliares: 
        -- swipeRight', swipeLeft', swipeUp', swipeDown':
            -- São as funções que apenas deslizam o tabuleiro para algum lado sem adicionar nenhum numero
swipeRight' :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeRight' [] = []
swipeRight' xss = map deslizar xss

swipeLeft' :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeLeft' [] = []
swipeLeft' xss = map reverse $ map deslizar ssx
    where
        ssx = map reverse xss

swipeDown' :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeDown' [] = []
swipeDown' xss = transposta $ swipeRight' xssT
    where
        xssT = transposta xss

swipeUp' :: (Num a, Eq a ) => [[a]]-> [[a]]
swipeUp' [] = []
swipeUp' xss = transposta $ swipeLeft' xssT
    where
        xssT = transposta xss


        
    -- adicionaNovoNumero: esta função recebe um tabuleiro e adiciona um 2 em uma posição zero
adicionaNovoNumero ::  (Num a, Eq a ) => [[a]]-> [[a]]
adicionaNovoNumero xss 
    | enderecoAleatorio == (5,5) = xss
    |otherwise =alterarIndice xss enderecoAleatorio 2
    where
        enderecoAleatorio = escolherEndereco xss



    -- Funções Auxiliares dos swipeRight', swipeUp', etc:
        -- delizar: desliza uma linha para a direita, recebe um array de numeros e retorna ele com os numeros iguais e vizinhos somados e os substitui nos indices superiores cujo valor é zero
deslizar :: (Eq a, Num a) => [a] -> [a]
deslizar [] = []
deslizar [x] = [x]
deslizar xs
    | okArrasto = xsModificada
    | otherwise = arrastar xsModificada
    where
        xsModificada = arrastaESoma xs
        okArrasto = checkArrastar $ xsModificada
        
        -- transposta: recebe uma matriz e retorna a transposta dela
transposta :: (Eq a, Num a) => [[a]] -> [[a]]
transposta [] = []
transposta xss = recRot xss 0

        --  Funções Auxiliares de deslizar:
            -- arrastaESoma: se array ja estiver "arrastado" (de acordo com checkArrastar) então ele executa a função soma no array se não ele chama a si msm com arrastar xs
arrastaESoma :: (Eq a, Num a) => [a] -> [a]
arrastaESoma [] = []
arrastaESoma [x] = [x]
arrastaESoma xs
    | okArrasto = soma xs
    | otherwise = arrastaESoma (arrastar xs)
    where
        okArrasto = checkArrastar xs

            -- checkArrastar: avalia se existe zero com indice superior de não-zeros, isto é, se há [2,2,2,0] ou [2,0,0,2] então retorna false, pois o array não está "arrastado"
            -- caso seja arrays do tipo [0,0,2,2],[0,2,32,16],[0,0,0,2] retorna true
checkArrastar :: (Eq a, Num a) => [a] -> Bool
checkArrastar [] = True
checkArrastar [x] = True
checkArrastar (x:xs)
    | x /= 0 && head xs == 0 = False
    | otherwise = checkArrastar xs



        -- Funções auxiliares de  arrastaESoma
            -- arrastar: recebe um array, joga todos os números para indices superiores se houver um zero nos mesmos e preenche com zero o lugar desses numeros, 
                -- ou seja, transforma [2,0,0,0] ou [0,2,0,0] em [0,0,0,2],  [2,4,0,0] ou [2,0,4,0] em [0,0,2,4] e assim por diante
arrastar :: (Eq a, Num a) => [a] -> [a]
arrastar [] = []
arrastar [x] = [x]
arrastar xs
    | checkArrastar xs = xs
    | otherwise = arrastar xsArrastado
    where 
        xsArrastado = shift xs

            -- soma: recebe um array e para cada numero soma se tiver um vizinho com indice superior, [2,2,2,2] vira [0,4,0,4]
soma :: (Eq a, Num a) => [a] -> [a]
soma [] = []
soma [x] = [x]
soma xs
    | ult /= penult = (soma (init xs)) ++ [ult]
    | otherwise = (soma (init $ init xs)) ++ [0, ult + penult]
    where
        ult = last xs
        penult = last $ init xs



        -- Função auxiliar de arrastar: 
            --shift: recebe um array e para o não-zero de maior valor sobe um indice para ele e insere zero na posição anterior
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


        --Funções auxiliares de transposta:
            -- RecRot e pegar coluna
recRot :: (Eq a, Num a) => [[a]] -> Int -> [[a]]
recRot [] _ = []
recRot xss i 
    | coluna == [] = []
    | otherwise = coluna:(recRot xss (i+1))
    where
        coluna = pegarColuna xss i

pegarColuna :: (Eq a, Num a) => [[a]] -> Int -> [a]
pegarColuna [[]] _ = []
pegarColuna xss i 
    |i >= length (head xss) = []
    |otherwise = [xs !! i | xs <- xss]


    -- Funções auxiliares de adionarNovoNumero:
        -- Alterar indice: recebe um tabuleiro, as coordenadas e um valor, e retorna o msm tabuleiro com o valor substituido na dada coordenada (linha, coluna)
alterarIndice :: (Num a, Eq a) => [[a]] -> (Int, Int) -> a -> [[a]]
alterarIndice xss (linha, coluna) valor = alterarIndice' xss (linha,coluna) valor 0 0

        -- alterarIndice': O msm que alterar indice só que recursivo
            -- colunaAtual e linhaAtual sempre são o indice do x e xs respectivamente dentro de seus métodos 
alterarIndice' :: (Num a, Eq a) => [[a]] -> (Int, Int) -> a -> Int -> Int -> [[a]]
alterarIndice' (xs: xss) (linha, coluna) valor linhaAtual colunaAtual 
    | linhaAtual < linha =  xs:(alterarIndice' xss (linha, coluna) valor (linhaAtual + 1) colunaAtual)
    | linhaAtual == linha = (atualizarLinha xs coluna valor 0) : xss

        --atualizarLinha: o msm que alterarIndice' só que em nivel de um array, recebe um array, um endereço de coluna, um valor e uma colunaAtual pela recursividade
            -- colunaAtual é sempre o indice x durante a execução
atualizarLinha :: (Num a, Eq a ) => [a] -> Int -> a -> Int -> [a]
atualizarLinha (x:xs) coluna valor colunaAtual
    | colunaAtual < coluna = x:(atualizarLinha xs coluna valor (colunaAtual + 1) )
    | colunaAtual == coluna = valor:xs


        -- Essa função recebe uma matriz e retorna um endereço ~supostamente~ aleatorio de uma lista de posições zeros
escolherEndereco :: (Num a, Eq a) => [[a]] -> (Int, Int)
escolherEndereco xss 
    | zeros == [] = (5,5)
    | otherwise  = head zeros
    where
        zeros = listarZerosMatriz xss
-- escolherEndereco xss = zeros !! indice
--     where
--         zeros = listarZerosMatriz xss
--         indice = getStdRandom(randomR (0,4))



-- Dada uma matriz retorna os endereços (linha, coluna) de onde está os zeros
listarZerosMatriz :: (Num a, Eq a) => [[a]] -> [(Int, Int)]
listarZerosMatriz xss = listarZerosMatriz' xss 0

listarZerosMatriz' :: (Num a , Eq a ) => [[a]] -> Int -> [(Int, Int)]
listarZerosMatriz' [] _ = []
listarZerosMatriz' (xs:xss) linha = coordenadasDeZeros ++ listarZerosMatriz' xss (linha + 1)
    where
        listaDeZeros = listarZeros xs 0
        coordenadasDeZeros = [(linha, i)| i <- listaDeZeros]

-- Recebe um array de ints e uma coluna (assume que será sempre zero quando esta função ser chamada), e retorna um arrayb 
listarZeros :: (Num a, Eq a) => [a] -> Int -> [Int]
listarZeros [] col = []
listarZeros [a]  col
    | a == 0 =  [col]
    | otherwise = []

listarZeros (x:xs) col
    | x == 0 = col:(listarZeros xs (col + 1))
    | otherwise = (listarZeros xs (col + 1))

comparaEstadoLinha :: (Num a, Eq a) => [a] -> [a] -> Bool
comparaEstadoLinha [] [] = True                                 -- Quer dizer que ambas as listas esvariaram logo são iguais
comparaEstadoLinha [] ys = False                                -- Quer dizer que apenas uma esvaziou e outra possui itens, trivialmente são diferentes
comparaEstadoLinha xs [] = False                                -- idem
comparaEstadoLinha (x:xs) (y:ys)
    | y == x = comparaEstadoLinha xs ys
    | otherwise = False                                         -- Se apenas um item for diferente então é falso

-- recebe duas matrizes e retorna se as duas são iguais ou não
comparaEstado :: (Num a, Eq a) => [[a]] -> [[a]] -> Bool
comparaEstado [] []  = True
comparaEstado [] yss = False
comparaEstado xss [] = False
comparaEstado (xs:xss) (ys:yss) 
    | comparaEstadoLinha xs ys = comparaEstado xss yss
    | otherwise = False

-- As funções abaixo retornam se é possível deslizar para algum lado
canSwipeRight :: (Num a, Eq a) => [[a]] -> Bool
canSwipeRight xss = not $ comparaEstado rightSwiped xss
    where
        rightSwiped = swipeRight' xss

canSwipeLeft ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeLeft xss = not $ comparaEstado leftSwiped xss
    where
        leftSwiped = swipeLeft' xss

canSwipeUp ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeUp xss = not $ comparaEstado upSwiped xss
    where
        upSwiped = swipeUp' xss

canSwipeDown ::  (Num a, Eq a) => [[a]] -> Bool
canSwipeDown xss = not $ comparaEstado downSwiped xss
    where
        downSwiped = swipeDown' xss

checkMovimentosPossiveis :: (Num a, Eq a) => [[a]] -> [Bool]
checkMovimentosPossiveis xss = [canSwipeUp xss] ++ [canSwipeRight xss] ++ [canSwipeDown xss] ++ [canSwipeLeft xss]

-- Função auxiliar para checar se todos os indices de um array de Bool são falsos, retorna false se tiver pelo menos um true na lista
allFalse :: [Bool] -> Bool
allFalse [] = True
allFalse [x] = not x
allFalse (x:xs)
    | not x = (not x) && allFalse xs
    | otherwise = False

-- checa se é fim de jogo
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