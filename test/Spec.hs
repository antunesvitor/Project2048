module Main where

import Lib

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = (testGroup "projeto 2048 testes" [checkarrastarTest, arrastarTest,
                                            somaTest, deslizarTest, rotacionarAntiHorarioTest,
                                            comparaEstadoLinhaTeste, comparaEstadoTeste,
                                            checkGameOverTest, movimentacoesPossiveisTeste])

checkarrastarTest = testGroup "checkArrastar"
        [ testCase "test1" (assertEqual "Test 1" True (checkArrastar [0,0,0,2])),
        testCase "test2" (assertEqual "Test 2" True (checkArrastar [0,0,2,2])),
        testCase "test3" (assertEqual "Test 3" True (checkArrastar [0,2,2,2])),
        testCase "test4" (assertEqual "Test 4" True (checkArrastar [2,2,2,2])),
        testCase "test5" (assertEqual "Test 5" True (checkArrastar [0,0,0,0])),
        testCase "test6" (assertEqual "Test 6" False (checkArrastar [0,0,2,0])),
        testCase "test7" (assertEqual "Test 7" False (checkArrastar [2,0,2,0])),
        testCase "test8" (assertEqual "Test 8" False (checkArrastar [2,2,2,0])),
        testCase "test9" (assertEqual "Test 9" False (checkArrastar [2,0,0,0])),
        testCase "test10" (assertEqual "Test 10" False (checkArrastar [2,0,0,2])),
        testCase "test11" (assertEqual "Test 11" False (checkArrastar [0,2,0,0]))
        ]

arrastarTest = testGroup "arrastar"
    [
        testCase "test1" (assertEqual "Test 1" [0,0,0,2] (arrastar [2,0,0,0])),
        testCase "test2" (assertEqual "Test 2" [0,0,0,2] (arrastar [0,2,0,0])),
        testCase "test3" (assertEqual "Test 3" [0,0,0,2] (arrastar [0,0,2,0])),
        testCase "test4" (assertEqual "Test 4" [0,0,0,2] (arrastar [0,0,0,2])),
        testCase "test5" (assertEqual "Test 5" [0,0,2,2] (arrastar [2,2,0,0])),
        testCase "test6" (assertEqual "Test 6" [0,0,2,2] (arrastar [0,2,2,0])),
        testCase "test7" (assertEqual "Test 7" [0,0,2,2] (arrastar [0,2,0,2])),
        testCase "test8" (assertEqual "Test 8" [0,0,2,2] (arrastar [2,0,0,2])),
        testCase "test9" (assertEqual "Test 9" [0,0,2,2] (arrastar [2,0,2,0])),
        testCase "test10" (assertEqual "Test 10" [0,0,2,2] (arrastar [0,2,2,0])),
        testCase "test11" (assertEqual "Test 11" [0,0,2,2] (arrastar [0,0,2,2])),
        testCase "test12" (assertEqual "Test 12" [0,2,2,2] (arrastar [2,2,2,0])),
        testCase "test13" (assertEqual "Test 13" [0,2,2,2] (arrastar [2,0,2,2])),
        testCase "test14" (assertEqual "Test 14" [0,2,2,2] (arrastar [2,2,0,2])),
        testCase "test15" (assertEqual "Test 15" [0,2,2,2] (arrastar [0,2,2,2])),
        testCase "test16" (assertEqual "Test 16" [2,2,2,2] (arrastar [2,2,2,2]))
    ]

somaTest = testGroup "soma"
    [
        testCase "test1" (assertEqual "Test 1" [0,0,0,2] (soma [0,0,0,2])),
        testCase "test2" (assertEqual "Test 2" [0,0,0,4] (soma [0,0,2,2])),
        testCase "test3" (assertEqual "Test 3" [0,2,0,4] (soma [0,2,2,2])),
        testCase "test4" (assertEqual "Test 4" [0,4,0,4] (soma [2,2,2,2])),
        testCase "test5" (assertEqual "Test 5" [0,0,4,4] (soma [0,2,2,4])),
        testCase "test6" (assertEqual "Test 6" [0,0,8,8] (soma [0,4,4,8])),
        testCase "test7" (assertEqual "Test 7" [4,0,4,8] (soma [4,0,4,8])),
        testCase "test8" (assertEqual "Test 8" [0,2,4,8] (soma [0,2,4,8])),
        testCase "test9" (assertEqual "Test 9" [0,0,8,8] (soma [0,4,4,8])),
        testCase "test10" (assertEqual "Test 10" [2,4,8,16] (soma [2,4,8,16])),
        testCase "test11" (assertEqual "Test 11" [16,0,16,16] (soma [16,8,8,16])),
        testCase "test12" (assertEqual "Test 12" [12,8,0,16] (soma [12,8,8,8])),
        testCase "test13" (assertEqual "Test 13" [0,8,0,64] (soma [4,4,32,32])),
        testCase "test14" (assertEqual "Test 14" [0,16,16,32] (soma [8,8,16,32]))
    ]

deslizarTest = testGroup "deslizar"
    [
        testCase "test1" (assertEqual "Test 1" [0,0,0,2] (deslizar [0,0,0,2])),
        testCase "test2" (assertEqual "Test 2" [0,0,0,4] (deslizar [0,0,2,2])),
        testCase "test3" (assertEqual "Test 3" [0,0,2,4] (deslizar [0,2,2,2])),
        testCase "test4" (assertEqual "Test 4" [0,0,4,4] (deslizar [2,2,2,2])),
        testCase "test5" (assertEqual "Test 5" [0,0,4,4] (deslizar [2,2,2,2])),
        testCase "test6" (assertEqual "Test 6" [0,0,4,4] (deslizar [0,2,2,4])),
        testCase "test7" (assertEqual "Test 7" [0,0,0,8] (deslizar [0,0,4,4])),
        testCase "test8" (assertEqual "Test 8" [0,4,4,8] (deslizar [2,2,4,8])),
        testCase "test9" (assertEqual "Test 9" [0,0,4,4] (deslizar [2,2,2,2])),
        testCase "test10" (assertEqual "Test 10" [0,16,16,32] (deslizar [8,8,16,32])),
        testCase "test11" (assertEqual "Test 11" [0,0,8,64] (deslizar [4,4,32,32])),
        testCase "test12" (assertEqual "Test 12" [0,4,8,2] (deslizar [4,4,4,2])),
        testCase "test13" (assertEqual "Test 13" [0,8,8,2] (deslizar [4,4,8,2]))
    ]

rotacionarAntiHorarioTest = testGroup "rotacionarAntiHorario"
    [
        testCase "test1" (assertEqual "Test 1" [[15,20],[25,30]] (transposta [[15,25],[20,30]])),
        testCase "test2" (assertEqual "Test 2" [[10,10,10],[15,15,15],[20,20,20]] (transposta [[10,15,20],[10,15,20],[10,15,20]])),
        testCase "test3" (assertEqual "Test 3" [[1,1,1,1],[2,2,2,2],[3,3,3,3],[4,4,4,4]] (transposta [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]])),
        testCase "test4" (assertEqual "Test 4" [[1,1,1,1,1],[2,2,2,2,2],[3,3,3,3,3],[4,4,4,4,4],[5,5,5,5,5]] (transposta [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]])),
        testCase "test5" (assertEqual "Test 5" [[10,30,50],[20,40,60]] (transposta [[10,20],[30,40],[50,60]])),
        testCase "test6" (assertEqual "Test 6" [[10],[20],[30],[40],[50]] (transposta [[10,20,30,40,50]])),
        testCase "test7" (assertEqual "Test 7" [[10,20,30,40,50]] (transposta [[10],[20],[30],[40],[50]])),
        testCase "test8" (assertEqual "Test 8" [[1,4,7],[2,5,8],[3,6,9]] (transposta [[1,2,3],[4,5,6],[7,8,9]]))
    ]
    
comparaEstadoLinhaTeste = testGroup "compararEstadoLinha"
    [
        testCase "teste1" (assertEqual "Test 1" True (comparaEstadoLinha ([1,2,3,4]) ([1,2,3,4]))),
        testCase "teste2" (assertEqual "Test 2" False (comparaEstadoLinha ([1,2,3,4]) ([1,2,3,4,5]))),
        testCase "teste3" (assertEqual "Test 3" True (comparaEstadoLinha ([]) ([]))),
        testCase "teste4" (assertEqual "Test 4" False (comparaEstadoLinha ([1,1,1,2]) ([1,1,1,1]))),
        testCase "teste5" (assertEqual "Test 5" False (comparaEstadoLinha ([1,1,2,1]) ([1,1,1,1]))),
        testCase "teste6" (assertEqual "Test 6" False (comparaEstadoLinha ([1,2,1,1]) ([1,1,1,1]))),
        testCase "teste7" (assertEqual "Test 7" False (comparaEstadoLinha ([2,1,1,1]) ([1,1,1,1]))),
        testCase "teste8" (assertEqual "Test 8" False (comparaEstadoLinha ([1,1,1,1]) ([1,1,1,2]))),
        testCase "teste9" (assertEqual "Test 9" False (comparaEstadoLinha ([1,1,1,1]) ([1,1,2,1]))),
        testCase "teste10" (assertEqual "Test 10" False (comparaEstadoLinha ([1,1,1,1]) ([1,2,1,1]))),
        testCase "teste11" (assertEqual "Test 11" False (comparaEstadoLinha ([1,1,1,1]) ([2,1,1,1])))
    ]

comparaEstadoTeste = testGroup "compararEstadoLinha"
    [
        testCase "teste1" (assertEqual "Test 1" True (comparaEstado ([[1,2,3,4]]) ([[1,2,3,4]]))),
        testCase "teste2" (assertEqual "Test 2" False (comparaEstado ([[1,2,3,4]]) ([[1,2,3,4],[5]]))),
        testCase "teste3" (assertEqual "Test 3" True (comparaEstado ([]) ([])))
    ]

movimentacoesPossiveisTeste = testGroup "movimentacoesPossiveis"
    [
        testCase "teste1" (assertEqual "Test 1" [False, True,  True,  True]   (checkMovimentosPossiveis [[2,2,2,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]])),
        testCase "teste2" (assertEqual "Test 2" [True,  True,  True,  True]   (checkMovimentosPossiveis [[2,2,2,2],[0,0,0,0],[2,2,2,2],[0,0,0,0]])),
        testCase "teste3" (assertEqual "Test 3" [True,  True,  True,  True]   (checkMovimentosPossiveis [[0,0,0,0],[0,2,0,0],[0,0,0,0],[0,0,0,0]])),
        testCase "teste4" (assertEqual "Test 4" [True,  True,  False, True]   (checkMovimentosPossiveis [[0,0,0,0],[0,0,0,0],[0,0,0,0],[2,2,2,2]])),
        testCase "teste5" (assertEqual "Test 5" [True,  True,  False, True]   (checkMovimentosPossiveis [[0,0,0,0],[0,0,0,0],[0,0,0,0],[2,2,2,2]])),
        testCase "teste6" (assertEqual "Test 6" [False, False, False, False]  (checkMovimentosPossiveis [[4,8,128,256],[2,4,64,128],[4,8,128,16],[32,2,16,32]])),
        testCase "teste7" (assertEqual "Test 7" [True,  False, True,  False]  (checkMovimentosPossiveis [[4,8,128,256],[2,4,128,4],[4,8,128,16],[32,2,16,32]])),
        testCase "teste8" (assertEqual "Test 8" [False, True,  False, True]   (checkMovimentosPossiveis [[4,4,128,256],[2,32,64,128],[4,8,128,16],[32,2,16,32]])),
        testCase "teste9" (assertEqual "Test 9" [False, False, False, True]   (checkMovimentosPossiveis [[0,8,128,256],[0,4,64,128],[0,8,128,16],[0,2,16,32]])),
        testCase "teste10" (assertEqual "Test 10" [True, True, False, False]  (checkMovimentosPossiveis [[1024,8,128,0],[2,4,64,128],[4,8,128,16],[32,2,16,32]])),
        testCase "teste11" (assertEqual "Test 11" [False, True,  True,  True]   (checkMovimentosPossiveis [[4,8,128,256],[2,4,64,128],[4,8,128,16],[32,2,0,32]]))
    ]

checkGameOverTest = testGroup "checkGameOver" 
    [
        testCase "teste1" (assertEqual "Test 1" False (checkGameOver [[2,2,2,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]])),
        testCase "teste2" (assertEqual "Test 2" True  (checkGameOver [[4,8,128,256],[2,4,64,128],[4,8,128,16],[32,2,16,32]])),
        testCase "teste3" (assertEqual "Test 3" False (checkGameOver [[1024,8,128,0],[2,4,64,128],[4,8,128,16],[32,2,16,32]])),
        testCase "teste4" (assertEqual "Test 4" True  (checkGameOver [[2,4,2,4],[4,2,4,2],[2,4,2,4],[4,2,4,2]])),
        testCase "teste5" (assertEqual "Test 5" True  (checkGameOver [[2,4,8,16],[16,8,4,2],[32,64,128,256],[2,4,8,16]])),
        testCase "teste6" (assertEqual "Test 6" False (checkGameOver [[2,4,8,16],[16,8,4,2],[32,128,128,256],[2,4,8,16]]))

    ] 
