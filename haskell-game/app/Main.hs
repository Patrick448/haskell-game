-- Maria Eduarda de Medeiros Simonassi _ 202365119A
-- Patrick Canto de Carvalho _ 201935026

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
            easyLevel gameState 0
            putStrLn "+----------------------- Terminamos o jogo! -------------------------------+"
        else if level == 2
            then do
            putStrLn "\n+--------------------------------------------------------------------------+"
            putStrLn "+-------------- Vamos dar início no jogo com o nível DIFICIL --------------+"
            difficultLevel gameState 0
            putStrLn "+----------------------- Terminamos o jogo! -------------------------------+"
            else return ()

