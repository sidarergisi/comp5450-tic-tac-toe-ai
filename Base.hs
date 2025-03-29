module Base where

-- Players in the game
data Player = One | Two
  deriving (Eq, Show)

-- Swap player over
flipPlayer :: Player -> Player
flipPlayer One = Two
flipPlayer Two = One

-- A board is a list of Maybe Player where
-- * Nothing = empty postion
-- * Just One = player one has played here
-- * Just Two = player two has played here
type Board = [Maybe Player]

-- Result of a move
data Result =
    Continue Board
    -- ^ Move was received and here is the updated board
  | Win Player Board
    -- ^ A player has won with the following board as the current game state
  | Draw Board
    -- ^ The game is a draw (tie) with the following board as the current game state
  deriving (Eq, Show)

-- Co-orination of the tournament
data Coordination =
    Stop
    -- ^ Stop tournament
  | Again
    -- ^ Continue tournament
  deriving (Eq, Show)