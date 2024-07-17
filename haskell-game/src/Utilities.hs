module Utilities (initMenu, levelMenu, numberOfDiceMenu, generateFaces, printDice, startMenu, easyLevel, difficultLevel) where

import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.Random

initMenu :: IO ()
initMenu = do
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "                   _______________________________"
    putStrLn "                  |                               |"
    putStrLn "                  |  Bem-vindo ao jogo dos dados  |"
    putStrLn "                  |_______________________________|"
    putStrLn ""
    putStrLn "+--------------------------------------------------------------------------+"

levelMenu :: IO Int
levelMenu = do             
    putStrLn "+--------------------------------------------------------------------------+"
    putStrLn "Para iniciarmos o jogo, você precisa definir qual NÍVEL deseja jogar."
    putStrLn "Para o nível FÁCIL, digite o número 1."
    putStrLn "Para o nível DIFÍCIL, digite o número 2."
    putStrLn "+--------------------------------------------------------------------------+"
    input <- readInt
    case input of
        Just level ->
            if level == 1 || level == 2
                then return level
                else do
                    putStrLn "+------ Valor inválido! Digite apenas 1 ou 2 para escolher seu nível. -----+"
                    levelMenu 
        Nothing -> do
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
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

easyLevel :: IO GameState -> Int -> IO ()
easyLevel ioGameState turn = do
    gameState <- ioGameState 
    let currentTurn = turn + 1
    let gameIsOver = isOver gameState
    if gameIsOver
        then return ()
    else do 
        putStrLn "\n"
        printDice gameState
        let playerTurn = isOdd currentTurn
        if playerTurn 
        then do
            let newGame = humanPlayerTurn ioGameState 
            easyLevel newGame currentTurn  
        else do
            putStrLn "\nÉ a minha vez de jogar!"
    
humanPlayerTurn :: IO GameState -> IO GameState
humanPlayerTurn ioGameState = do
    gameState <- ioGameState 
    let (GameState dice) = gameState
    let num = length dice
    putStrLn $ "É a sua vez de jogar! Escolha um dado de 1 até " ++ show num ++ " para mexer."
    choosenDie <- readInt
    case choosenDie of
        Just val -> 
            if val >= 1 && val <= num
            then do
                putStrLn "+------------------------ Você escolheu esse dado: ------------------------+"
                printDie (dice !! (val - 1))
                newGame <- chooseMove (return gameState) val
                return newGame
            else do
                putStrLn "+-------------------- Você escolheu um valor inválido. ---------------------+" 
                humanPlayerTurn ioGameState
        Nothing -> do
            putStrLn "+-------------------- Você escolheu um valor inválido. ---------------------+" 
            humanPlayerTurn ioGameState  

chooseMove :: IO GameState -> Int -> IO GameState
chooseMove ioGameState chosenDiePosition = do
    gameState <- ioGameState 
    let (GameState dice) = gameState
    let dieValue = getValueAtIndex (GameState dice) chosenDiePosition
    option <- case dieValue of
        Just value ->
            if value == 1
            then 
                return chooseMove1
            else if value == 2
                then
                    return chooseMove2
                else if value == 3
                    then 
                        return chooseMove3
                    else if value == 4
                        then 
                            return chooseMove4
                        else if value == 5
                            then 
                                return chooseMove5
                            else if value == 6 
                                then 
                                    return chooseMove6  
                                else
                                    fail "Erro: valor do dado inválido."
        Nothing -> do
            fail "Erro: valor do dado inválido."
    newGameState <- changeGameState (return (gameState)) (chosenDiePosition - 1) option
    return newGameState 

chooseMove6 :: IO (Maybe Int) 
chooseMove6 = do
    putStrLn "Você tem quatro opções para esse dado."
    putStrLn "Digite o número correspondente a opção que deseja realizar."
    putStrLn "Para virar para a face 2. DIGITE 2."
    putStrLn "Para virar para a face 3. DIGITE 3."
    putStrLn "Para virar para a face 4. DIGITE 4."
    putStrLn "Para virar para a face 5. DIGITE 5."
    option <- readInt
    case option of
        Just opt ->
            if 2 <= opt && opt <= 3 
                then do
                    return (Just opt)
            else do
                putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
                chooseMove6
        Nothing -> do
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
            chooseMove6           

chooseMove1 :: IO (Maybe Int)  
chooseMove1 = do
    putStrLn "A única opção para esse dado é ser removido do jogo."
    putStrLn "+--------------------------------------------------------------------------+"
    return (Just 0)

chooseMove2 :: IO (Maybe Int)  
chooseMove2 = do 
    putStrLn "A única opção para esse dado é ser virado para a face 1."
    putStrLn "+--------------------------------------------------------------------------+"
    return (Just 1)

chooseMove3 :: IO (Maybe Int)  
chooseMove3 = do 
    putStrLn "Você tem duas opções para esse dado."
    putStrLn "Digite o número correspondente a opção que deseja realizar."
    putStrLn "Para virar para a face 1. DIGITE 1."
    putStrLn "Para virar para a face 2. DIGITE 2."
    option <- readInt
    case option of
        Just opt -> 
            if opt >= 1 && opt <= 2
                then do 
                    return (Just opt)
            else do
                putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
                chooseMove3
        Nothing -> do
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
            chooseMove3
             
chooseMove4 :: IO (Maybe Int) 
chooseMove4 = do
    putStrLn "Você tem duas opções para esse dado."
    putStrLn "Digite o número correspondente a opção que deseja realizar."
    putStrLn "Para virar para a face 1. DIGITE 1."
    putStrLn "Para virar para a face 2. DIGITE 2."
    option <- readInt
    case option of
        Just opt -> 
            if opt >= 1 && opt <= 2
                then do 
                    return (Just opt)
            else do
                putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
                chooseMove4
        Nothing -> do
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
            chooseMove4

chooseMove5 :: IO (Maybe Int) 
chooseMove5 = do
    putStrLn "Você tem três opções para esse dado."
    putStrLn "Digite o número correspondente a opção que deseja realizar."
    putStrLn "Para virar para a face 1. DIGITE 1."
    putStrLn "Para virar para a face 3. DIGITE 3."
    putStrLn "Para virar para a face 4. DIGITE 4."
    option <- readInt
    case option of
        Just opt -> 
            if opt == 1 || opt == 3 || opt == 4
                then do 
                    return (Just opt)
            else do
                putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
                chooseMove5   
        Nothing -> do 
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
            chooseMove4                         

changeGameState :: IO GameState -> Int -> IO(Maybe Int) -> IO GameState
changeGameState ioGameState chosenDiePosition option = do
    (GameState dice) <- ioGameState 
    opt <- option
    let newDice = case opt of
                    Just 0 -> removeAt chosenDiePosition dice
                    Just opt_ -> modifyAt chosenDiePosition opt_ dice
                    Nothing -> fail "Erro: valor do dado inválido."
    return (GameState newDice)

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n + 1) xs

modifyAt :: Int -> a -> [a] -> [a]
modifyAt n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

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