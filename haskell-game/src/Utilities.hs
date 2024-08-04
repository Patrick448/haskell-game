-- Maria Eduarda de Medeiros Simonassi _ 202365119A
-- Patrick Canto de Carvalho _ 201935026

module Utilities (initMenu, levelMenu, numberOfDiceMenu, generateFaces, printDice, startMenu, easyLevel, difficultLevel) where

import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.Random
import Data.List (delete, sort, findIndex, nub)

-- Menus de escolha -- 
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

-- Implementação do nível fácil 
easyLevel :: GameState -> Int -> IO ()
easyLevel gameState turn = do
    let currentTurn = turn + 1
    let gameIsOver = isOver gameState
    if gameIsOver 
        then do 
            if isOdd currentTurn
                then do
                    putStrLn "+------------------------- Eu ganhei o jogo! ------------------------------+"
                else do
                    putStrLn "+------------------------ Você ganhou o jogo! -----------------------------+"
    else do 
        putStrLn "\n\n"
        printDice gameState
        let playerTurn = isOdd currentTurn
        if playerTurn 
            then do
                newGameHuman <- humanPlayerTurn gameState
                easyLevel newGameHuman currentTurn
            else do
                newGameComputer <- computerEasyLevelTurn gameState
                easyLevel newGameComputer currentTurn


-- Implementação da jogada do jogador humano    
humanPlayerTurn :: GameState -> IO GameState
humanPlayerTurn gameState = do
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
                humanPlayerTurn (GameState dice)
        Nothing -> do
            putStrLn "+-------------------- Você escolheu um valor inválido. ---------------------+" 
            humanPlayerTurn (GameState dice) 


-- Escolha dos dados e movimentos para a jogada do humano
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
            fail "Erro: valor do dado inválido. ta caindo aqui"
    newGameState <- changeGameState (return (gameState)) (chosenDiePosition - 1) option
    return newGameState 

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
            if 2 <= opt && opt <= 5 
                then do
                    return (Just opt)
            else do
                putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
                chooseMove6
        Nothing -> do
            putStrLn "+----------- Entrada inválida! Digite apenas números válidos. -------------+"
            chooseMove6                       


-- Escolha dos dados e movimentos para a jogada do computador no nível fácil 
chooseMoveComputer :: [Int] -> IO (Maybe Int)
chooseMoveComputer optVec = do
    randOpt <- randomNum (length optVec)
    let option = (optVec !! (randOpt - 1))
    return (Just option)

computerEasyLevelTurn :: GameState -> IO GameState
computerEasyLevelTurn gameState = do
    let (GameState dice) = gameState
    let num = length dice
    choosenDiePosition <- (randomNum num)
    putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
    printDie (dice !! (choosenDiePosition - 1))
    let dieValue = getValueAtIndex (GameState dice) choosenDiePosition
    option <- case dieValue of
        Just value ->
            if value == 1
            then return (Just 0)
            else if value == 2
                then return (Just 1)
                else if value == 3
                    then (chooseMoveComputer [1,2])
                    else if value == 4
                        then (chooseMoveComputer [1,2])
                        else if value == 5
                            then (chooseMoveComputer [1,3,4])
                            else if value == 6 
                                then (chooseMoveComputer [2,3,4,5])
                                else
                                    fail "Erro: valor do dado inválido."
        Nothing -> do
            fail "Erro: valor do dado inválido."
    newGameState <- changeGameState (return (gameState)) (choosenDiePosition - 1) (return option)
    return newGameState 


-- Implementação do nível difícil
difficultLevel :: GameState -> Int -> IO ()
difficultLevel gameState turn = do
    let currentTurn = turn + 1
    let gameIsOver = isOver gameState
    if gameIsOver 
        then do 
            if not (isOdd currentTurn)
                then do
                    putStrLn "+------------------------- Eu ganhei o jogo! ------------------------------+"
                else do
                    putStrLn "+------------------------ Você ganhou o jogo! -----------------------------+"
    else do 
        putStrLn "\n\n"
        printDice gameState
        let playerTurn = not (isOdd currentTurn)
        if playerTurn 
            then do
                newGameHuman <- humanPlayerTurn gameState
                difficultLevel newGameHuman currentTurn
            else do
                newGameComputer <- computerHardLevelTurn gameState
                difficultLevel newGameComputer currentTurn


-- Escolha dos dados e movimentos para a jogada do computador no nível difícil
chooseHardMove1 :: Int -> GameState ->  IO GameState
chooseHardMove1 n gameState = do
    let (GameState dice) = gameState
    let dieValue = getValueAtIndex (GameState dice) (n+1)
    case dieValue of
        Just value ->
            if value == 1
                then do
                    return (GameState (removeAt n dice))
                else if value == 2
                    then do
                        (changeGameState (return gameState) n (return (Just 1)))
                    else if value == 3 || value == 4
                        then do
                            (changeGameState (return gameState) n (return (Just 2)))
                        else if value == 5
                            then do
                                (changeGameState (return gameState) n (return (Just 3)))
                            else do
                                (changeGameState (return gameState) n (return (Just 4)))
        Nothing -> do
            fail "Erro: valor do dado inválido."

chooseHardMoveMoreThan2 :: GameState -> IO GameState
chooseHardMoveMoreThan2 gameState = do
    let (GameState dice) = gameState
    let equal2or5 = all (\value -> value == 2 || value == 5) dice
    if equal2or5
        then do
            choosenDiePosition <- (randomNum (length dice))
            putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
            printDie (dice !! (choosenDiePosition - 1))
            changeGameState (return gameState) (choosenDiePosition - 1) (return (Just 1))
    else do
        let filteredDice = filter (\value -> value /= 2 && value /= 5) dice
        let removedPairsDice = (removePairs (==) (sort filteredDice))
        let removedSevenSumDice = removeSumSevenPairs (sort removedPairsDice)
        let zeroDice = length removedSevenSumDice == 0
        let oneDie = length removedSevenSumDice == 1
        let twoDice = length removedSevenSumDice == 2
        if zeroDice
            then do
                choosenDiePosition <- (randomNum (length dice))
                putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                printDie (dice !! (choosenDiePosition - 1))
                chooseHardMove1 (choosenDiePosition - 1) gameState
        else if oneDie
            then do
                let index = findIndex (== (removedSevenSumDice !! 0)) dice
                case index of
                    Just i -> do
                        putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                        printDie (dice !! i)
                        chooseHardMove1 i gameState
                    Nothing -> do
                        fail "Erro: valor do dado inválido."
        else if twoDice
            then do
                let index1 = findIndex (== (removedSevenSumDice !! 0)) dice
                let index2 = findIndex (== (removedSevenSumDice !! 1)) dice
                case index1 of
                    Just i1 -> 
                        case index2 of
                            Just i2 -> 
                                chooseHardMove2 i1 i2 gameState
                            Nothing -> do
                                fail "Erro: valor do segundo dado inválido."
                    Nothing -> do
                        fail "Erro: valor do primeiro dado inválido."

        else do
            fail "Erro: Combinação de dados inválida."

chooseHardMove2 :: Int -> Int -> GameState -> IO GameState
chooseHardMove2 i1 i2 gameState = do 
    let (GameState dice) = gameState 
    let firstDieValue = getValueAtIndex (GameState dice) (i1+1)
    let secondDieValue = getValueAtIndex (GameState dice) (i2+1)
    case firstDieValue of
        Just fDie -> 
            case secondDieValue of 
                Just sDie ->
                    if fDie == sDie -- Ambos dados tem a mesma face
                        then do
                            putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                            printDie (dice !! i1)
                            if fDie == 1 -- Ambas faces são 1
                                then changeGameState (return gameState) i1 (return (Just 0))
                            else if fDie == 2 -- Ambas faces são 2
                                then changeGameState (return gameState) i1 (return (Just 1))
                            else if fDie == 3 -- Ambas faces são 3
                                then changeGameState (return gameState) i1 (return (Just 2))
                            else if fDie == 4 -- Ambas faces são 4
                                then changeGameState (return gameState) i1 (return (Just 2))
                            else if fDie == 5 -- Ambas faces são 5
                                then changeGameState (return gameState) i1 (return (Just 3))
                            else if fDie == 6 -- Ambas faces são 6
                                then changeGameState (return gameState) i1 (return (Just 4))
                            else 
                                fail "Erro: Combinação de dados e valores inválido"
                    else if fDie + sDie == 7 -- Os dados são diferentes e a soma é igual a sete
                        then do
                            choosenDiePosition <- randomNum 2
                            if choosenDiePosition == 1  -- De forma randômica, iremos mexer no primeiro dado 
                                then do
                                    putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                                    printDie (dice !! 0)
                                    if fDie == 1 
                                        then changeGameState (return gameState) i1 (return (Just 0))
                                    else if fDie == 2 
                                        then changeGameState (return gameState) i1 (return (Just 1))
                                    else if fDie == 3 
                                        then changeGameState (return gameState) i1 (return (Just 2))
                                    else if fDie == 4 
                                        then changeGameState (return gameState) i1 (return (Just 2))
                                    else if fDie == 5 
                                        then changeGameState (return gameState) i1 (return (Just 3))
                                    else if fDie == 6 
                                        then changeGameState (return gameState) i1 (return (Just 4))
                                    else 
                                        fail "Erro: Combinação de dados e valores inválido"     
                            else if choosenDiePosition == 2 -- De forma randômica, iremos mexer no segundo dado    
                                then do
                                    putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                                    printDie (dice !! 1)
                                    if sDie == 1 
                                        then changeGameState (return gameState) i2 (return (Just 0))
                                    else if sDie == 2 
                                        then changeGameState (return gameState) i2 (return (Just 1))
                                    else if sDie == 3 
                                        then changeGameState (return gameState) i2 (return (Just 2))
                                    else if sDie == 4 
                                        then changeGameState (return gameState) i2 (return (Just 2))
                                    else if sDie == 5 
                                        then changeGameState (return gameState) i2 (return (Just 3))
                                    else if sDie == 6 
                                        then changeGameState (return gameState) i2 (return (Just 4))
                                    else 
                                        fail "Erro: Combinação de dados e valores inválido"   
                            else 
                                fail "Erro: Combinação de dados e valores inválido" 
                    else if fDie + sDie /= 7 -- Os dados são diferentes e a soma é diferente de sete
                        then do
                            if fDie < sDie -- O primeiro valor é menor que o segundo
                                then do
                                    putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                                    printDie (dice !! i2)
                                    changeGameState (return gameState) i2 (return (Just fDie))
                            else if sDie < fDie -- O segundo valor é menor que o primeiro 
                                then do
                                    putStrLn "+-------------------------- Eu escolhi esse dado: -------------------------+"
                                    printDie (dice !! i1)
                                    changeGameState (return gameState) i1 (return (Just sDie))
                            else
                                fail "Erro: Combinação de dados e valores inválido"
                    else 
                        fail "Erro: Combinação de dados e valores inválido"
                Nothing -> do
                    fail "Erro: valor do segundo dado inválido."
        Nothing -> do
            fail "Erro: valor do primeiro dado inválido."
        
computerHardLevelTurn :: GameState -> IO GameState
computerHardLevelTurn gameState = do
    let (GameState dice) = gameState
    let num = length dice
    if num == 1  -- Caso exista 1 dado no jogo 
        then chooseHardMove1 0 gameState
    else if num == 2  -- Caso existam 2 dados no jogo 
        then chooseHardMove2 0 1 gameState
    else -- Caso existam mais de 2 dados no jogo
        chooseHardMoveMoreThan2 gameState



-- Modificar o estado atual do jogo
changeGameState :: IO GameState -> Int -> IO(Maybe Int) -> IO GameState
changeGameState ioGameState chosenDiePosition option = do
    (GameState dice) <- ioGameState 
    opt <- option
    let newDice = case opt of
                    Just 0 -> removeAt chosenDiePosition dice
                    Just opt_ -> modifyAt chosenDiePosition opt_ dice
                    Nothing -> fail "Erro: valor do dado inválido."
    return (GameState newDice)       


-- Funções auxiliares
removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n + 1) xs

modifyAt :: Int -> a -> [a] -> [a]
modifyAt n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

getValueAtIndex :: GameState -> Int -> Maybe Int
getValueAtIndex (GameState dice) n
    | n > 0 && n <= length dice = Just (dice !! (n-1))
    | otherwise = Nothing

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

isOver :: GameState -> Bool
isOver (GameState dice)
    | length dice == 0 = True
    | otherwise        = False

removePairs :: (a -> a -> Bool) -> [a] -> [a]
removePairs _ [] = []  
removePairs _ [x] = [x]  
removePairs cond (x:y:xs)
    | cond x y  = removePairs cond xs 
    | otherwise = x : removePairs cond (y:xs)  

removeSumSevenPairs :: [Int] -> [Int]
removeSumSevenPairs xs = removeValues xs valuesToRemove
  where
    pairsToRemove = findPairsToRemove (sort xs)
    
    valuesToRemove = nub $ concatMap (\(a, b) -> [a, b]) pairsToRemove
    
    removeValues :: [Int] -> [Int] -> [Int]
    removeValues [] _ = []
    removeValues (y:ys) values
      | y `elem` values = removeValues ys (delete y values)
      | otherwise = y : removeValues ys values

    findPairsToRemove :: [Int] -> [(Int, Int)]
    findPairsToRemove [] = []
    findPairsToRemove (z:zs) = [(z, y) | y <- zs, z + y == 7] ++ findPairsToRemove zs

-- Criação da estrutura de dados 
data GameState = GameState [Int] deriving Show