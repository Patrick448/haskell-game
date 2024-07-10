import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.Random

-- Menus 
initMenu :: IO ()
initMenu = do
    putStrLn " _______________________________"
    putStrLn "|                               |"
    putStrLn "|  Bem-vindo ao jogo dos dados  |"
    putStrLn "|_______________________________|"
    putStrLn ""

levelMenu :: IO Int
levelMenu = do
    putStrLn "Para iniciarmos o jogo, você precisa definir qual NÍVEL deseja jogar."
    putStrLn "Para o nível FÁCIL, digite o número 1."
    putStrLn "Para o nível DIFÍCIL, digite o número 2.\n"
    input <- readInt
    case input of
        Just level ->
            if level == 1 || level == 2
                then return level
                else do
                    putStrLn "\nValor inválido! Digite apenas 1 ou 2 para escolher seu nível.\n"
                    levelMenu 
        Nothing -> do
            putStrLn "\nEntrada inválida! Digite apenas números válidos.\n"
            levelMenu

numberOfDiceMenu :: IO Int
numberOfDiceMenu = do
    putStrLn "\nAgora você precisa escolher a QUANTIDADE INICIAL DE DADOS para o nosso jogo."
    putStrLn "Atenção: digite um valor inteiro.\n"
    input <- readInt
    case input of
        Just numberOfDice -> do
            return numberOfDice
        Nothing -> do
            putStrLn "\nEntrada inválida! Digite apenas números válidos.\n"
            numberOfDiceMenu

startMenu :: GameState -> IO ()
startMenu (GameState dice) = do
    let num = length dice
    putStrLn "\nPara podermos dar início ao nosso jogo vamos estabelecer ALGUMAS REGRAS:"
    putStrLn "1. Quando for necessário você escolher um dado, preciso que você me indique um valor de 1 até a quantidade n de dados no jogo atual."
    putStrLn "Vamos fazer um teste?"
    putStrLn $ "Me indique um valor de 1 até " ++ show num  ++ " para indicar qual dado você deseja ver"
    choosenDie <- readInt
    case choosenDie of
        Just val -> 
            if val >= 1 && val <= num
            then do
                putStrLn "Esse foi o dado escolhido!"
                printDie (dice !! (val - 1))
            else do
                putStrLn "\nVocê escolheu um valor inválido para os dados." 
                startMenu (GameState dice)
        Nothing -> do
            putStrLn "\nVocê escolheu um valor inválido para os dados."
            startMenu (GameState dice)

startEasy :: GameState -> IO ()
startEasy (GameState dice) = do
    putStrLn "coisas do nível fácil"

startDifficult :: GameState -> IO ()
startDifficult (GameState dice) = do
    putStrLn "No nível difícil eu começo a jogada!"
    choosenDie <- randomNum (length dice)
    putStrLn $ "Escolhi mexer no dado " ++ show choosenDie
    printDie (dice !! (choosenDie - 1))

-- Funçãoes auxiliares 
randomDiceNum :: IO Int
randomDiceNum = do
    gen <- getStdGen
    let (randomNumber, newGen) = randomR (1, 6) gen :: (Int, StdGen)
    setStdGen newGen
    return (randomNumber)

randomNum :: Int -> IO Int
randomNum n = do
    gen <- getStdGen
    let (randomNumber, newGen) = randomR (1, n) gen :: (Int, StdGen)
    setStdGen newGen
    return (randomNumber)

generateFaces :: Int -> IO GameState
generateFaces n = do
    numbers <- replicateM n randomDiceNum
    return (GameState numbers)

readInt :: IO (Maybe Int)
readInt = do
    str <- getLine
    return (readMaybe str :: Maybe Int)

printDice :: GameState -> IO ()
printDice (GameState dice) = do
    putStrLn "\nFaces voltadas para cima na mesa\n"
    putStrLn $ concatMap (\_ -> "   +---+    ") dice
    putStrLn $ concatMap (\num -> "   | " ++ show num ++ " |    ") dice
    putStrLn $ concatMap (\_ -> "   +---+    ") dice

printDie :: Int -> IO ()
printDie dieNum = do
    putStrLn $ "   +---+    "
    putStrLn $ "   | " ++ show dieNum ++ " |    "
    putStrLn $ "   +---+    "


-- Tipos de dados gerados
data GameState = GameState [Int] deriving Show

-- Função principal
main :: IO ()  
main = do
    initMenu
    level <- levelMenu
    num <- numberOfDiceMenu
    gameState <- generateFaces num
    putStrLn "\nConfiguração inicial do jogo:"
    printDice gameState
    startMenu gameState
    putStrLn "Agora que você entendeu como escolher um dado, vamos dar início ao nosso jogo!"
    if level == 1
        then do
            putStrLn "\nVamos dar início no jogo com o nível FÁCIL"
            startEasy gameState
        else if level == 2
            then do
                putStrLn "\nVamos dar início no jogo com o nível DIFÍCIL"
                startDifficult gameState
            else return ()

