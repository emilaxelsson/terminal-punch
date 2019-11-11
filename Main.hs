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

-- | Labels for weeks, starting from this week and going backwards
weekLabels :: [String]
weekLabels =
  "This week" :
  "Last week" : [show (w :: Integer) ++ " weeks ago" | w <- [2 ..]]

-- | Start 'Day' of week, starting from this week and going backwards
weekBeginnings ::
     LocalTime -- ^ Current time
  -> (Day, [Day]) -- ^ (this week, previous weeks)
weekBeginnings now =
  ( addDays (negate daysSinceMon) thisDay
  , [ addDays (negate (daysSinceMon + 7 * weeksAgo)) thisDay
    | weeksAgo <- [1 ..]
    ]
  )
  where
    thisDay = localDay now
    daysSinceMon = fromIntegral $ pred $ fromEnum $ dayOfWeek thisDay

showSummary ::
     Int -- ^ Number of additional weeks to show time for
  -> LOG
  -> IO ()
showSummary extraWeeks log = do
  now <- getLocalTime
  let log' = stopNow now log
      is = intervals log'
      today = localDay now
      yesterday = addDays (-1) today
      (thisWeek, earlierWeeks) = weekBeginnings now
  let weekIntervals =
        fromDay thisWeek is :
        [ betweenDays week next is
        | (week, next) <- zip earlierWeeks (thisWeek : earlierWeeks)
        ]
  printTable $ map (fmap showTotal) $ concat
    [ [HLine]
    , getPeriods log'
    , concat
      [ [ Info False (label, totalTime week)
        | (label, week) <-
            reverse $ take (extraWeeks + 1) $ zip weekLabels weekIntervals
        ]
      , [ HLine
        , Info False ("Yesterday", totalTime $ betweenDays yesterday today is)
        , Info True  ("Today",     totalTime $ fromDay today is)
        ]
      ]
    ]

punch :: (LocalTime -> Punch LocalTime) -> IO ()
punch event = do
  t <- getLocalTime
  punchFile <- getPunchFilePath
  appendFile punchFile $ (++ "\n") $ show $ event t

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay as = Just (last as)

main :: IO ()
main = do
  setTitle "Punch"
  hSetBuffering stdin NoBuffering
  run 1
  where
    run extraWeeks = do
      putStrLn "\ESCc"
        -- `clearScreen` inserts newlines instead of actually clearing the
        -- terminal. See this discussion:
        -- https://stackoverflow.com/q/24754406/1105347
      hideCursor
        -- Needed after clearing. See the above link.
      elog <- readLog
      either handleError (runWithLog extraWeeks) $ do
        log <- elog
        void $ validLog log
        return log

    runWithLog extraWeeks log = do
      showSummary extraWeeks log
      case lastMay $ filter isEvent log of
        Nothing -> prompt False
        Just (Stop _) -> prompt False
        _ -> prompt True

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
      run 1

    prompt running = do
      if running
        then do
          setSGR [SetColor Foreground Vivid Green]
          putStr ">> running"
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
      if c == 'e'
        then run 4
        else run 1
