--Author: Ryuichiro S
--Date: 2015/07/01
--教授が配布したじゃんけんプログラムを参考にしました

module GlicoJanken where
import System.Random

import Prelude hiding (cycle)

data Move = Rock | Paper | Scissors
            deriving (Show, Eq)

type Tournament = ([Move],[Move])

outcome :: Move -> Move -> Integer
outcome Rock Scissors  = 3    -- The first Move beats the second.
outcome Scissors Paper = 6
outcome Paper Rock     = 6
outcome x y
  | x == y             = 0    -- The first Move draws with the second.
  | otherwise          = 0   -- The first Move loses against the second.

tournamentOutcomeLeft :: Tournament -> Integer
tournamentOutcomeLeft ([],_)  = 0
tournamentOutcomeLeft (_,[])  = 0
tournamentOutcomeLeft ((x:xs),(y:ys))
  = outcome x y + tournamentOutcomeLeft (xs, ys)


tournamentOutcomeRight :: Tournament -> Integer
tournamentOutcomeRight ([],_)  = 0
tournamentOutcomeRight (_,[])  = 0
tournamentOutcomeRight ((x:xs),(y:ys))
  = outcome y x + tournamentOutcomeRight (xs, ys)


type Strategy = [Move] -> IO Move

rock, paper, scissors :: Strategy
rock _      = return Rock
paper _     = return Paper
scissors _  = return Scissors

cycle :: Strategy
cycle moves
  = case mod (length moves) 3 of
     0 -> return Rock
     1 -> return Paper
     2 -> return Scissors


-- random numbers ranging from 0 to 2
random3 :: IO Int
random3 = getStdRandom (randomR (0,2))

-- a random strategy
randomSt :: [Move] -> IO Move
randomSt _ = 
  do x <- random3
     case x of
      0 -> return Rock
      1 -> return Paper
      2 -> return Scissors

echoOrRandom :: Strategy
echoOrRandom t@(latest:rest) =
  do x <- random3
     case x of
      0 -> randomSt t
      1 -> return latest
      2 -> return latest
echoOrRandom [] = randomSt []

-- Echo the last move.
echo :: Strategy
echo (latest:rest) = return latest
echo []            = return Rock


-- You can play interactively against a particular strategy using the function
play :: Strategy -> IO ()
play strategy =
  playInteractive strategy ([], [])

playInteractive :: Strategy -> Tournament -> IO ()
playInteractive s t@(machine's, user's) =
  do
    if ((tournamentOutcomeLeft t) >= 20 || (tournamentOutcomeRight t) >= 20)
       then showResults t
       else do putStrLn (show t) -- debug
               line <- getLine
               let ch = head line
               if not (elem ch "rpsRPS")
                 then showResults t
                 else do next <- s user's    ------ Monadic way, not using the let-construct.
                         let userMove = convertMove ch  --- Non-monadic function is called within let-construct
                         putStrLn ("\nI play: " ++ show next ++
                                   " you play: " ++ show userMove)
                         playInteractive s (next:machine's, userMove:user's)

convertMove :: Char -> Move
convertMove 'r'   = Rock
convertMove 's'   = Scissors
convertMove 'p'   = Paper
convertMove 'R'   = Rock
convertMove 'S'   = Scissors
convertMove 'P'   = Paper

showResults :: Tournament -> IO ()
showResults t
  = do putStrLn (show(tournamentOutcomeLeft t))
       putStrLn (show(tournamentOutcomeRight t))
