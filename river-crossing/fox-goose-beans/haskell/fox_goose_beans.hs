-- Fox, goose and bag of beans puzzle
-- https://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle

-- Code origin : https://github.com/wizzup/river-crossing-puzzle
-- MIT License

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Text.Printf (printf)
import System.Exit (exitSuccess)
import System.IO   (stdin, hSetEcho, hSetBuffering
                   ,BufferMode (NoBuffering))

import Data.Set  (Set)
import qualified Data.Set as S

data ItemType = Fox
              | Goose
              | Beans
              | Human
  deriving (Eq, Ord)

instance Show ItemType where
  show Fox   = "🐺"
  show Goose = "🐦"
  show Beans = "👝"
  show Human = "🚹"

-- use Set for collection of items since ordering is not important
type Items = Set ItemType

data BoatLoc = Start
             | Finish
  deriving (Show, Eq)

data State = State {
  start :: Items,      -- items at start position
  boatLoc :: BoatLoc,  -- boat location (start or finish)
  onBoat :: Items,     -- passengers on the boat
  finish :: Items      -- item at finish position
  }

instance Eq State where
  a == b = (start a  == start b)
        && (finish a == finish b)

instance Show State where
  show State{start, boatLoc, onBoat, finish}
    = showStart <> showBoat <> showFinsh
    where
      showStart = replicate (8 - 2 * length start) ' '
               <> (S.toList start >>= show)
      showBoat = case boatLoc of
                   Start  -> printf "<%s>====" showPassengers
                   Finish -> printf "====<%s>" showPassengers
      showPassengers = replicate (4 - 2 * length onBoat) ' '
                    <> (S.toList onBoat >>= show)
      showFinsh = S.toList finish >>= show

-- check if a state is bad (fox eats goose, goose eats beans)
isBadState :: State -> Bool
isBadState State{..}
  = case boatLoc of
      Start  -> bad finish
      Finish -> bad start
 where
  bad' is x = x `S.isSubsetOf` is 
           && Human `S.notMember` is
  bad is = or $ bad' is <$> [fg,gb]
  fg = S.fromList [Fox,Goose]
  gb = S.fromList [Goose,Beans]

isGoodState :: State -> Bool
isGoodState = not . isBadState

-- pre initial
preInitState :: State
preInitState = State {
  start = S.fromList [Fox, Goose, Beans, Human],
  boatLoc = Start,
  onBoat = S.empty,
  finish = S.empty
}

-- initial state
initState :: State
initState = State {
  start = S.fromList [Fox, Goose, Beans],
  boatLoc = Start,
  onBoat = S.singleton Human,
  finish = S.empty
}

-- target final state
finalState :: State
finalState = State {
  start = S.empty,
  boatLoc = Finish,
  onBoat = S.singleton Human,
  finish = S.fromList [Fox, Goose, Beans]
}

postFinalState :: State
postFinalState = State {
  start = S.empty,
  boatLoc = Finish,
  onBoat = S.empty,
  finish = S.fromList [Fox, Goose, Beans, Human]
}

crossRiver :: State -> State
crossRiver s@State{ boatLoc }
  = case boatLoc of
      Start  -> s { boatLoc = Finish }
      Finish -> s { boatLoc = Start }

-- NOTE:: no overloaded (len onBoat >2 ) check
load :: ItemType -> State -> State
load i s@State{..}
  | i `S.member` start  && boatLoc == Start = ss
  | i `S.member` finish && boatLoc == Finish = fs
  | otherwise = s
 where
  ss = s {
        start = (S.\\) start (S.singleton i),
        onBoat = S.fromList [Human, i]
    }
  fs = s {
        onBoat = S.fromList [Human, i],
        finish = (S.\\) finish (S.singleton i)
    }

unLoad :: ItemType -> State -> State
unLoad i s@State{..}
  | i `S.member` onBoat  && boatLoc == Start  = ss
  | i `S.member` onBoat  && boatLoc == Finish = fs
  | otherwise = undefined
 where
  ss = s {
        start = S.union start (S.singleton i),
        onBoat = S.fromList [Human]
    }
  fs = s {
        onBoat = S.fromList [Human],
        finish = S.union finish (S.singleton i)
    }

toggle :: ItemType -> State -> State
toggle i s@State{..}
  | S.member i onBoat  = unLoad i s
  | S.size onBoat == 1 = load i s
  | otherwise          = s

--------------------------------------------------------------------------------
-- IO Monad
--------------------------------------------------------------------------------

help :: IO ()
help = do
  mapM_ putStrLn
    ["Instruction:"
    ,""
    ,"<SPACE> for taking <..> (the boat) crossing the river"
    ,"<j> for load/unload " <> show Fox   <> "(Fox)"
    ,"<k> for load/unload " <> show Goose <> "(Goose)"
    ,"<l> for load/unload " <> show Beans <> "(Beans)"
    ,"<q> for quit the game (giving up)"
    ,""
    ,"press anykey to close this help"
    ]
  _ <- getChar
  pure ()

-- VT100 escape code
esc :: String -> String
esc s = '\ESC' : s

clearScreen :: IO ()
clearScreen = clear >> home
 where
  clear = putStr $ esc "[2J"
  home  = putStr $ esc "[H"

-- Fancy state status
stateStatus :: Int -> State -> IO ()
stateStatus n s = putStrLn $ printf "%s %s %d" ss sc n
 where
  ss = show s
  sc = if isGoodState s
       then green "  OK"
       else red "FAIL"
  green = col "[32m"
  red   = col "[31m"
  col c t = esc c <> t <> esc "[0m"

exit :: IO ()
exit = putStrLn "Giving up." >> exitSuccess

loop :: Int -> State -> IO ()
loop n s
  | s /= finalState = play
  | otherwise       = done
 where
  done = do
      clearScreen
      print postFinalState
      putStrLn $ printf "You cross the river %d times" n
      putStrLn "Well done."

  play = do
    clearScreen
    stateStatus n s

    if isGoodState s
    then do
      c <- getChar
      case c of
        'q' -> exit
        'j' -> loop n $ toggle Fox s
        'k' -> loop n $ toggle Goose s
        'l' -> loop n $ toggle Beans s
        ' ' -> loop (succ n) $ crossRiver s
        _   -> do
                help
                loop n s
    else do
      clearScreen
      stateStatus n s
      putStrLn "You have failed, anykey to restart again."
      _ <- getChar
      main

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  clearScreen
  print preInitState
  help
  loop 0 initState
