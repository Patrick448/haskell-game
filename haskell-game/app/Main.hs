module Main (main) where

import Utilities

-- Função principal
main :: IO ()  
main = do
    initMenu
    level <- levelMenu
    num <- numberOfDiceMenu
    gameState <- generateFaces num
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "CONFIGURAÇÃO INICIAL DO JOGO:"
    putStrLn "+--------------------------------------------------------------------------+\n"
    printDice gameState
    startMenu gameState
    putStrLn "Agora que você entendeu como escolher, vamos dar início ao nosso jogo!"
    putStrLn "+--------------------------------------------------------------------------+"
    if level == 1
        then do
            putStrLn "\n+--------------------------------------------------------------------------+"
            putStrLn "+--------------- Vamos dar início no jogo com o nível FÁCIL ---------------+"
            easyLevel gameState
        else if level == 2
            then do
                putStrLn "\nVamos dar início no jogo com o nível DIFÍCIL"
                difficultLevel gameState
            else return ()

