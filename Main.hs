{-# LANGUAGE Rank2Types #-}

module Main (main) where

import Prelude hiding (log)

import Control.Monad (void, when)
import Data.Coerce (coerce)
import Data.Time
  ( LocalTime
  , NominalDiffTime
  , dayOfWeek
  , getCurrentTime
  , getCurrentTimeZone
  , timeToDaysAndTimeOfDay
  , todHour
  , todMin
  , utcToLocalTime
  , DayOfWeek(..)
  )
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
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hSetBuffering, stdin)

import Punch

getPunchFilePath :: IO FilePath
getPunchFilePath = (</> ".punch") <$> getEnv "HOME"
  -- Can't use `getHomeDirectory` because `directory` requires (via `unix`)
  -- `time <1.9`.

readLog :: IO (Either (PunchError PunchLocalTime) (Log PunchLocalTime))
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

-- | Labels for days, starting from today and going backwards
dayLabels :: LocalTime -> [String]
dayLabels now =
  "Today" :
  "Yesterday" :
  map show lastWeeksDays ++
  [show (d :: Integer) ++ " days ago" | d <- [7 ..]]
  where
    today = dayOfWeek $ dayOf now
    lastWeeksDays
      = drop 2 -- drop today and yesterday
      $ take 7
      $ dropWhile (/= today)
      $ cycle
      $ reverse [Monday .. Sunday]

showSummary ::
     Int -- ^ Number of additional weeks to show time for
  -> Log LocalTime
  -> IO ()
showSummary history log = do
  now <- getLocalTime
  let log'  = stopNow now log
      is    = intervals log'
      weeks = weekIntervals now is
      days  = dayIntervals now is
  printTable $ map (fmap showTotal) $ concat
    [ [HLine]
    , getPeriods log'
    , concat
      [ [ Info False (label, totalTime week)
        | (label, week) <- reverse $ take (history+1) $ zip weekLabels weeks
        ]
      , [ HLine ]
      , [ Info (label == "Today") (label, totalTime day)
        | (label, day) <- reverse $ take (history+1) $ zip (dayLabels now) days
        ]
      ]
    ]

punch :: (forall t. t -> Punch t) -> IO ()
punch event = do
  t <- getLocalTime
  punchFile <- getPunchFilePath
  appendFile punchFile $ (++ "\n") $ show $ event $ PunchLT t

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay as = Just (last as)

main :: IO ()
main = do
  setTitle "Punch"
  hSetBuffering stdin NoBuffering
  run 1
  where
    clear = do
      putStrLn "\ESCc"
        -- `clearScreen` inserts newlines instead of actually clearing the
        -- terminal. See this discussion:
        -- https://stackoverflow.com/q/24754406/1105347
      hideCursor
        -- Needed after clearing. See the above link.

    run history = do
      clear
      elog <- readLog
      either handleError (runWithLog history) $ do
        log <- coerce elog
        void $ validLog log
        return log

    runWithLog history log = do
      clear -- Needed when we skip `run` and jump here directly
      showSummary history log
      case lastMay $ filter isEvent log of
        Nothing -> prompt history log False
        Just (Stop _) -> prompt history log False
        _ -> prompt history log True

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

    prompt history log running = do
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
      case c of
        '+' -> runWithLog (succ history) log
        '-' -> runWithLog (max (pred history) 0) log
        _   -> run history
