-- se368.hs
-- Sidar Ergisi
-- Functional Programming Assignment 2
-- Tic-Tac-Toe game using Decision Trees

module Main where

import Base
import DecisionTree
import Control.Concurrent

-- -- A helper function to choose the first Just value, or fallback to the second
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y  = y

-- Q1: Convert the board to a readable string
showBoard :: Board -> String
showBoard board = unlines $ map formatRow (chunksOf 3 board)
  where
    formatRow = unwords . map showCell
    showCell Nothing      = "_"
    showCell (Just One)   = "X"
    showCell (Just Two)   = "O"

-- break list into chunks of 3
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Q2: Turn (row, col) into board index
toPos :: (Int, Int) -> Maybe Int
toPos (r, c)
  | r >= 0 && r <= 2 && c >= 0 && c <= 2 = Just (r * 3 + c)
  | otherwise = Nothing

-- Q3: Look up a position on the board
lookupBoard :: Board -> Int -> Maybe Player
lookupBoard board idx
  | idx >= 0 && idx < length board = board !! idx
  | otherwise = Nothing

-- Q4: Add player move to board (only if it’s empty tho)
addToGameBoard :: Board -> Int -> Player -> Maybe Board
addToGameBoard board idx player =
  case lookupBoard board idx of
    Nothing -> Just (take idx board ++ [Just player] ++ drop (idx + 1) board)
    Just _  -> Nothing

-- Q5: Check for a win
checkWin :: Board -> Maybe Player
checkWin board = 
  let lines = [ [0,1,2], [3,4,5], [6,7,8]
              , [0,3,6], [1,4,7], [2,5,8]
              , [0,4,8], [2,4,6] ]
      checkLine [a,b,c] = 
        case (board !! a, board !! b, board !! c) of
          (Just p1, Just p2, Just p3) | p1 == p2 && p2 == p3 -> Just p1
          _ -> Nothing
  in foldr (\line acc -> checkLine line `orElse` acc) Nothing lines

-- Q6: Horizontal line
hline :: IO ()
hline = putStrLn (replicate 20 '-')

-- Q7: Pick which channel based on player
select :: Player -> Chan Int -> Chan Int -> Chan Int
select One ch1 _ = ch1
select Two _ ch2 = ch2

-- Q8: Write the same thing twice to a channel
writeChanTwice :: Chan a -> a -> IO ()
writeChanTwice ch msg = writeChan ch msg >> writeChan ch msg

-- Q9: Game logic – runs every turn
gameServer :: Player -> Board -> Chan Int -> Chan Int -> Chan Result -> IO (Maybe Player)
gameServer currentPlayer board ch1 ch2 resultChan = do
  hline
  putStrLn $ "Player " ++ show currentPlayer ++ ", enter your move:"
  move <- readChan $ select currentPlayer ch1 ch2
  putStrLn $ "Player " ++ show currentPlayer ++ " attempted to move to " ++ show move
  case addToGameBoard board move currentPlayer of
    Nothing -> do
      putStrLn "Invalid move!"
      writeChanTwice resultChan (Continue board)
      gameServer (flipPlayer currentPlayer) board ch1 ch2 resultChan
    Just newBoard -> do
      putStrLn (showBoard newBoard)
      case checkWin newBoard of
        Just winner -> do
          putStrLn $ "Player " ++ show winner ++ " wins!"
          writeChanTwice resultChan (Win winner newBoard)
          return (Just winner)
        Nothing ->
          if Nothing `elem` newBoard
            then do
              writeChanTwice resultChan (Continue newBoard)
              gameServer (flipPlayer currentPlayer) newBoard ch1 ch2 resultChan
            else do
              putStrLn "It's a draw!"
              writeChanTwice resultChan (Draw newBoard)
              return Nothing

-- Q10: Run a full tournament (with score and rematch)
gameServerStart :: Player -> (Chan Coordination, Chan Coordination) -> (Chan Int, Chan Int) -> (Int, Int) -> Chan Result -> IO ()
gameServerStart startingPlayer (coord1, coord2) (ch1, ch2) (score1, score2) resultChan = do
  outcome <- gameServer startingPlayer (replicate 9 Nothing) ch1 ch2 resultChan
  let (newScore1, newScore2) = case outcome of
        Just One -> (score1 + 1, score2)
        Just Two -> (score1, score2 + 1)
        Nothing  -> (score1, score2)
  hline
  putStrLn $ "Score Board: Player One = " ++ show newScore1 ++ " | Player Two = " ++ show newScore2
  hline
  putStrLn "Play again? Player One needs to say Y/N?"
  response <- getLine
  case response of
    "Y" -> do
      writeChan coord1 Again
      writeChan coord2 Again
      gameServerStart (flipPlayer startingPlayer) (coord1, coord2) (ch1, ch2) (newScore1, newScore2) resultChan
    "N" -> do
      writeChan coord1 Stop
      writeChan coord2 Stop
      putStrLn "End of tournament!"
    _ -> do
      putStrLn "Invalid input, please enter Y or N."
      gameServerStart startingPlayer (coord1, coord2) (ch1, ch2) (newScore1, newScore2) resultChan

-- Q11: Game starter
startGame :: (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()) -> (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()) -> IO ()
startGame player1 player2 = do
  coord1 <- newChan
  coord2 <- newChan
  ch1    <- newChan
  ch2    <- newChan
  result <- newChan
  forkIO $ player1 One coord1 ch1 result
  forkIO $ player2 Two coord2 ch2 result
  gameServerStart One (coord1, coord2) (ch1, ch2) (0, 0) result

-- Q12: Convert user input (row and column) to board position
parseInput :: String -> String -> Maybe Int
parseInput rowStr colStr = do
  row <- case rowStr of {"1" -> Just 0; "2" -> Just 1; "3" -> Just 2; _ -> Nothing}
  col <- case colStr of {"A" -> Just 0; "a" -> Just 0; "B" -> Just 1; "b" -> Just 1; "C" -> Just 2; "c" -> Just 2; _ -> Nothing}
  toPos (row, col)

-- Q13: Human player
humanPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
humanPlayer player coordChan moveChan resultChan = tournamentLoop
  where
    tournamentLoop = do
      gameLoop
      case player of
        One -> do
          putStrLn "Play again? Y or N"
          resp <- getLine
          case resp of
            "Y" -> writeChan coordChan Again >> tournamentLoop
            "N" -> writeChan coordChan Stop
            _   -> putStrLn "Invalid input." >> tournamentLoop
        Two -> do
          msg <- readChan coordChan
          case msg of
            Again -> tournamentLoop
            Stop  -> return ()

    gameLoop = do
      putStr "Enter row (1-3): "
      row <- getLine
      putStr "Enter column (A-C): "
      col <- getLine
      case parseInput row col of
        Nothing   -> putStrLn "Invalid input!" >> gameLoop
        Just move -> writeChan moveChan move

      result <- readChan resultChan
      handleResult result

    handleResult (Win p _)    = putStrLn $ "Game over! Player " ++ show p ++ " wins!"
    handleResult (Draw _)     = putStrLn "Game over! It's a draw!"
    handleResult (Continue _) = gameLoop

-- Q14: AI player using decision trees
aiPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
aiPlayer player coordChan moveChan resultChan = gameLoop ([], [])
  where
    header = map show [0..8]
    gameLoop trainingData = do
      result <- playGame (replicate 9 Nothing) [] trainingData
      let (finalBoard, updatedData) = result
          label = case checkWin finalBoard of
                    Just winner | winner == player -> Yes
                    _ -> No
          newRow = boardToRow player finalBoard
          newTrainingData = addRow updatedData (newRow, label)
      msg <- readChan coordChan
      case msg of
        Again -> gameLoop newTrainingData
        Stop  -> return ()
    playGame board moves trainingData = do
      result <- readChan resultChan
      case result of
        Continue newBoard -> do
          let move = pickBestMove player newBoard (learnTree trainingData bestGain)
          writeChan moveChan move
          playGame newBoard (move:moves) trainingData
        Win _ b  -> return (b, trainingData)
        Draw b   -> return (b, trainingData)
    boardToRow :: Player -> Board -> Row
    boardToRow p = map (showCell p)
    showCell :: Player -> Maybe Player -> String
    showCell _ Nothing = "Nothing"
    showCell p (Just pl)
      | p == pl   = "Me"
      | otherwise = "Them"
    pickBestMove :: Player -> Board -> DecisionTree -> Int
    pickBestMove p b tree =
      let possibleMoves = [i | i <- [0..8], b !! i == Nothing]
          scored = [(i, scoreMove (makeMove i)) | i <- possibleMoves]
          makeMove i = take i b ++ [Just p] ++ drop (i + 1) b
          scoreMove board' = case infer tree header (boardToRow p board') of
                               Just (Yes, conf) -> conf
                               Just (No, conf)  -> -conf
                               Nothing          -> 0
      in fst $ maximumBy compareSnd scored
    compareSnd (_, a) (_, b) = compare a b
    maximumBy _ [x] = x
    maximumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == GT then y else acc) x xs

main :: IO ()
main = startGame humanPlayer aiPlayer