module Utilities (initMenu, levelMenu, numberOfDiceMenu, startMenu, easyLevel, difficultLevel, randomNum,generateFaces, readInt, printDice, printDie) where

import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.Random

-- Menus 
initMenu :: IO ()
initMenu = do
    putStrLn "+-----------------------------------+"
    putStrLn "  _______________________________"
    putStrLn " |                               |"
    putStrLn " |  Bem-vindo ao jogo dos dados  |"
    putStrLn " |_______________________________|"
    putStrLn ""
    putStrLn "+-----------------------------------+\n"

levelMenu :: IO Int
levelMenu = do
    putStrLn "+---------------------------------------------------------------------+"
    putStrLn "Para iniciarmos o jogo, você precisa definir qual NÍVEL deseja jogar."
    putStrLn "Para o nível FÁCIL, digite o número 1."
    putStrLn "Para o nível DIFÍCIL, digite o número 2."
    putStrLn "+---------------------------------------------------------------------+"
    input <- readInt
    case input of
        Just level ->
            if level == 1 || level == 2
                then return level
                else do
                    putStrLn "+--- Valor inválido! Digite apenas 1 ou 2 para escolher seu nível. ---+"
                    levelMenu 
        Nothing -> do
            putStrLn "+--------- Entrada inválida! Digite apenas números válidos. ----------+"
            levelMenu

numberOfDiceMenu :: IO Int
numberOfDiceMenu = do
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "Agora você precisa escolher a QUANTIDADE INICIAL DE DADOS para o nosso jogo."
    putStrLn "Atenção: digite um valor inteiro."
    putStrLn "+--------------------------------------------------------------------------+"
    input <- readInt
    case input of
        Just numberOfDice -> do
            return numberOfDice
        Nothing -> do
            putStrLn "+------------ Entrada inválida! Digite apenas números válidos. ------------+"
            numberOfDiceMenu

startMenu :: GameState -> IO ()
startMenu (GameState dice) = do
    let num = length dice
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "Para podermos dar início ao nosso jogo vamos estabelecer ALGUMAS REGRAS:"
    putStrLn "1. Quando for necessário você escolher um dado, preciso que você me \nindique um valor de 1 até a quantidade n de dados no jogo atual."
    putStrLn "Vamos fazer um teste?"
    putStrLn $ "Digite um valor de 1 até " ++ show num  ++ " para indicar qual dado da mesa você deseja ver."
    putStrLn "+--------------------------------------------------------------------------+"
    choosenDie <- readInt
    case choosenDie of
        Just val -> 
            if val >= 1 && val <= num
            then do
                putStrLn "+----------------------- Esse foi o dado escolhido: -----------------------+"
                printDie (dice !! (val - 1))
            else do
                putStrLn "+------------- Você escolheu um valor inválido para os dados. -------------+" 
                startMenu (GameState dice)
        Nothing -> do
            putStrLn "+------------- Você escolheu um valor inválido para os dados. -------------+"
            startMenu (GameState dice)

easyLevel :: GameState -> IO ()
easyLevel (GameState dice) = do
    putStrLn "coisas do nível fácil"

difficultLevel :: GameState -> IO ()
difficultLevel (GameState dice) = do
    putStrLn "No nível difícil eu começo a jogada!"
    choosenDie <- randomNum (length dice)
    putStrLn $ "Escolhi mexer no dado " ++ show choosenDie
    printDie (dice !! (choosenDie - 1))

-- Funçãoes auxiliares 
randomNum :: Int -> IO Int
randomNum n = do
    gen <- getStdGen
    let (randomNumber, newGen) = randomR (1, n) gen :: (Int, StdGen)
    setStdGen newGen
    return (randomNumber)

generateFaces :: Int -> IO GameState
generateFaces n = do
    numbers <- replicateM n (randomNum 6)
    return (GameState numbers)

readInt :: IO (Maybe Int)
readInt = do
    str <- getLine
    return (readMaybe str :: Maybe Int)

printDice :: GameState -> IO ()
printDice (GameState dice) = do
    putStrLn "+------------------- Faces voltadas para cima na mesa ---------------------+"
    mapM_ printDiceLine (chunksOf 6 dice)

printDiceLine :: [Int] -> IO ()
printDiceLine dice = do
    putStrLn $ concatMap (\_ -> "   +---+    ") dice
    putStrLn $ concatMap (\num -> "   | " ++ show num ++ " |    ") dice
    putStrLn $ concatMap (\_ -> "   +---+    ") dice

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

printDie :: Int -> IO ()
printDie dieNum = do
    putStrLn $ "   +---+    "
    putStrLn $ "   | " ++ show dieNum ++ " |    "
    putStrLn $ "   +---+    "


-- Tipos de dados gerados
data GameState = GameState [Int] deriving Show