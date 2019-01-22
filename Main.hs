module Main (main) where

import Prelude hiding (log)

import Control.Monad
import Data.Time
import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hideCursor
  , showCursor
  , setSGR
  , setTitle
  )
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Punch

type LOG = Log LocalTime

getPunchFilePath :: IO FilePath
getPunchFilePath = (</> ".punch") <$> getEnv "HOME"
  -- Can't use `getHomeDirectory` because `directory` requires (via `unix`)
  -- `time <1.9`.

readLog :: IO (Either (PunchError LocalTime) LOG)
readLog = fmap parseLog . readFile =<< getPunchFilePath

data Row a
  = HLine
  | Info Bool (String, a) -- ^ Boolean says whether to highlight the line
  deriving (Functor)

getPeriods :: AbstractTime time => Log time -> [Row (MeasuredTime time)]
getPeriods log =
  [Info False (m, totalTime $ intervals log') | (m, log') <- listPeriods log]

putStrLnBright :: String -> IO ()
putStrLnBright str = do
  setSGR
    [ SetConsoleIntensity BoldIntensity
    , SetColor Foreground Vivid White
    , SetColor Background Dull Black
    ]
  putStr (str ++ " ")
  setSGR [Reset]
  putStrLn ""
  -- Note that this setting only works on dark background.

printTable :: [Row String] -> IO ()
printTable tab = sequence_
  [ case row of
      HLine -> putStrLn hline
      Info bright (label, str) ->
        let row' = take w1 (label ++ repeat ' ') ++ "  :  " ++ str
        in if bright
          then putStrLnBright row'
          else putStrLn row'
  | row <- tab
  ]
  where
    w1 = min 16 $ maximum $ (0 :) [length label | Info _ (label, _) <- tab]
    w2 = maximum $ (0 :) [length info | Info _ (_, info) <- tab]
    w = w1 + 5 + w2 + 1
    hline = replicate w '-'

showTotal :: NominalDiffTime -> String
showTotal dt = show hours ++ " hours, " ++ show (todMin t) ++ " minutes"
  where
    (days, t) = timeToDaysAndTimeOfDay dt
    hours = todHour t + fromInteger (24 * days)

getLocalTime :: IO LocalTime
getLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

showSummary :: LOG -> IO ()
showSummary log = do
  now <- getLocalTime
  let log' = stopNow now log
      is = intervals log'
  let daysSinceMon = fromIntegral $ pred $ fromEnum $ dayOfWeek $ localDay now
      lastWeek = addDays (negate (daysSinceMon + 7)) $ localDay now
      thisWeek = addDays (negate daysSinceMon) $ localDay now
      yesterday = addDays (-1) $ localDay now
      today = localDay now
  printTable $ map (fmap showTotal) $ concat
    [ [HLine]
    , getPeriods log'
    , [ Info False ("Last week", totalTime $ betweenDays lastWeek thisWeek is)
      , Info False ("This week", totalTime $ fromDay thisWeek is)
      , HLine
      , Info False ("Yesterday", totalTime $ betweenDays yesterday today is)
      , Info True  ("Today",     totalTime $ fromDay today is)
      ]
    ]

punch :: (LocalTime -> Punch LocalTime) -> IO ()
punch event = do
  t <- getLocalTime
  punchFile <- getPunchFilePath
  appendFile punchFile $ (++ "\n") $ show $ event t

main :: IO ()
main = do
  setTitle "Punch"
  hSetBuffering stdin NoBuffering
  go
  where
    go = do
      putStrLn "\ESCc"
        -- `clearScreen` inserts newlines instead of actually clearing the
        -- terminal. See this discussion:
        -- https://stackoverflow.com/q/24754406/1105347
      hideCursor
        -- Needed after clearing. See the above link.
      elog <- readLog
      case elog of
        Left msg -> handleError msg
        Right log -> case validLog log of
          Left msg -> handleError msg
          Right _ -> do
            showSummary log
            case log of
              [] -> prompt False
              _ | Stop _ <- last log -> prompt False
                | otherwise -> prompt True

    handleError msg = do
      putStrLn "Error in .punch file:"
      putStrLn $ show msg
      putStrLn ""
      putStrLn "Please edit the file to fix the problem."
      putStrLn ""
      putStrLn "* Press 'q' or ESC to quit."
      putStrLn "* Press any other key to continue."
      c <- getChar
      when (c `elem` "q\ESC") (putStrLn "" >> showCursor >> exitSuccess)
      go

    prompt running = do
      if running
        then do
          setSGR [SetColor Foreground Vivid Green]
          putStr ">> running"
          setSGR [Reset]
        else do
          setSGR [SetColor Foreground Vivid Red]
          putStr ">> stopped"
          setSGR [Reset]
      putStrLn ""
      c <- getChar
      when (c `elem` "q\ESC") (putStrLn "" >> showCursor >> exitSuccess)
      when (c == ' ') $ if running
        then punch Stop
        else punch Start
      go
