module Utilities (initMenu, levelMenu, numberOfDiceMenu, generateFaces, printDice, startMenu, easyLevel, difficultLevel) where

import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.Random

initMenu :: IO ()
initMenu = do
    putStrLn "+---------------------------------------------------------------------+"
    putStrLn "                   _______________________________"
    putStrLn "                  |                               |"
    putStrLn "                  |  Bem-vindo ao jogo dos dados  |"
    putStrLn "                  |_______________________________|"
    putStrLn ""
    putStrLn "+---------------------------------------------------------------------+\n"

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

isOver :: GameState -> Bool
isOver (GameState dice)
    | length dice == 0 = True
    | otherwise        = False

easyLevel :: GameState -> Int -> IO ()
easyLevel (GameState dice) turn = do
    let currentTurn = turn + 1
    if currentTurn == 3 
        then do return ()
    else do 
        putStrLn "\n"
        printDice (GameState dice)
        let num = length dice
        let playerTurn = isOdd currentTurn
        if playerTurn 
        then do
            humanPlayerTurn (GameState dice) currentTurn
        else do
            putStrLn "\nÉ a minha vez de jogar!"
    
humanPlayerTurn :: GameState -> Int -> IO ()
humanPlayerTurn (GameState dice) turn = do
    let num = length dice
    putStrLn $ "É a sua vez de jogar! Escolha um dado de 1 até " ++ show num ++ " para mexer."
    choosenDie <- readInt
    case choosenDie of
        Just val -> 
            if val >= 1 && val <= num
            then do
                putStrLn "+------------------------ Você escolheu esse dado: ------------------------+"
                printDie (dice !! (val - 1))
                chooseMove (GameState dice) val turn
            else do
                putStrLn "+-------------------- Você escolheu um valor inválido. ---------------------+" 
                humanPlayerTurn (GameState dice) turn 
        Nothing -> do
            putStrLn "+-------------------- Você escolheu um valor inválido. ---------------------+" 
            humanPlayerTurn (GameState dice) turn   

chooseMove :: GameState -> Int -> Int -> IO ()
chooseMove (GameState dice) chosenDie turn = do
    let dieValue = getValueAtIndex (GameState dice) chosenDie
    case dieValue of
        Just value ->
            if value == 1
            then do
                putStrLn "A única opção para esse dado é ser removido do jogo.\nDado removido."
                putStrLn "+--------------------------------------------------------------------------+"
                easyLevel (GameState dice) turn 
            else if value == 2
                then do
                    putStrLn "A única opção para esse dado é ser virado para a face 1.\nDado modificado."
                    putStrLn "+--------------------------------------------------------------------------+"
                    easyLevel (GameState dice) turn
                    else if value == 3
                        then do 
                            putStrLn "Você tem duas opções para esse dado."
                            putStrLn "Digite o número correspondente a opção que deseja realizar."
                            putStrLn "1. Virar o dado para a face 1."
                            putStrLn "2. Virar o dado para a face 2."
                            option <- readInt
                            easyLevel (GameState dice) turn
                        else if value == 4
                            then do 
                                putStrLn "Você tem duas opções para esse dado."
                                putStrLn "Digite o número correspondente a opção que deseja realizar."
                                putStrLn "1. Virar o dado para a face 1."
                                putStrLn "2. Virar o dado para a face 2."
                                option <- readInt
                                easyLevel (GameState dice) turn
                            else if value == 5
                                then do 
                                    putStrLn "Você tem três opções para esse dado."
                                    putStrLn "Digite o número correspondente a opção que deseja realizar."
                                    putStrLn "1. Virar o dado para a face 1."
                                    putStrLn "2. Virar o dado para a face 3."
                                    putStrLn "3. Virar o dado para a face 4."
                                    option <- readInt
                                    easyLevel (GameState dice) turn
                                else if value == 6 
                                    then do 
                                        putStrLn "Você tem quatro opções para esse dado."
                                        putStrLn "Digite o número correspondente a opção que deseja realizar."
                                        putStrLn "1. Virar o dado para a face 2."
                                        putStrLn "2. Virar o dado para a face 3."
                                        putStrLn "3. Virar o dado para a face 4."
                                        putStrLn "4. Virar o dado para a face 5."
                                        option <- readInt  
                                        easyLevel (GameState dice) turn                                 
                                    else return ()
        Nothing -> putStrLn "Valor inválido."

getValueAtIndex :: GameState -> Int -> Maybe Int
getValueAtIndex (GameState dice) n
    | n >= 0 && n < length dice = Just (dice !! (n-1))
    | otherwise = Nothing

difficultLevel :: GameState -> IO ()
difficultLevel (GameState dice) = do
    let num = length dice
    putStrLn $ "coisa do difícil" ++ show num ++ "!"

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
    putStrLn "+--------------------------------------------------------------------------+"

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

isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0

data GameState = GameState [Int] deriving Show