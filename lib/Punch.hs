module Punch where

import Prelude hiding (log)

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.List (tails)
import Data.Time
import GHC.Stack (HasCallStack)

oops :: HasCallStack => a
oops = error "internal error"

-- | Time interval denoted by a pair of a start and stop time
--
-- The start time is included in the interval and the stop time is excluded (see
-- 'occursIn').
type Interval time = (time, time)

occursIn :: AbstractTime time => time -> Interval time -> Bool
occursIn t (t1, t2) = t1 <= t && t < t2

class ( Ord time
      , Ord (DayOf time)
      , Enum (DayOf time)
      , Num (MeasuredTime time)
      ) =>
      AbstractTime time
  where
  type DayOf time
  type MeasuredTime time

  dayOf :: time -> DayOf time

  -- | The length of a time interval
  --
  -- If the @start >= stop@, then the interval length is 0.
  intervalLength :: Interval time -> MeasuredTime time

  -- | Start of the day
  midnightOf :: DayOf time -> time

instance AbstractTime LocalTime where
  type DayOf        LocalTime = Day
  type MeasuredTime LocalTime = NominalDiffTime
  dayOf          = localDay
  intervalLength = max 0 . uncurry (flip diffLocalTime)
  midnightOf d   = LocalTime {localDay = d, localTimeOfDay = midnight}

data PunchError time
  = DoubleEvent (Punch time) (Punch time)
  | NonIncreasingTime (Punch time) (Punch time)
  | FormatError String
  deriving (Show)

-- | A punch entry
data Punch time
  = Start time    -- ^ Start an interval
  | Stop time     -- ^ Stop an interval
  | Period String -- ^ Mark the start of a period
  deriving (Eq, Show, Read)

-- | A punch time log
type Log time = [Punch time]

isEvent :: Punch time -> Bool
isEvent (Start _)  = True
isEvent (Stop _)   = True
isEvent (Period _) = False

-- | Check that the log is valid
validLog :: AbstractTime time => [Punch time] -> Either (PunchError time) ()
validLog = go . filter isEvent
  where
    go [] = return ()
    go [_] = return ()
    go (p1 : p2 : log)
      | Start _  <- p1, Start _  <- p2 = Left $ DoubleEvent p1 p2
      | Stop _   <- p1, Stop _   <- p2 = Left $ DoubleEvent p1 p2
      | Start t1 <- p1, Stop t2  <- p2, t2 <= t1 = Left $ NonIncreasingTime p1 p2
      | Stop t1  <- p1, Start t2 <- p2, t2 < t1  = Left $ NonIncreasingTime p1 p2
      | otherwise = go (p2:log)

-- | Convert a valid 'Log' into a sequence of intervals
--
-- Any leading 'Stop' event or trailing 'Start' event will be ignored.
intervals :: Log time -> [Interval time]
intervals = go . filter isEvent
  where
    go []                         = []
    go [Start _]                  = []
    go (Stop _ : log)             = go log
    go (Start t1 : Stop t2 : log) = (t1, t2) : go log
    go _ = oops

fromIntervals :: [Interval time] -> Log time
fromIntervals = concatMap (\(t1, t2) -> [Start t1, Stop t2])

-- | Check that the list of intervals is valid
validIntervals ::
     AbstractTime time => [Interval time] -> Either (PunchError time) ()
validIntervals is = do
  sequence_
    [ unless (t1 < t2) $ Left (NonIncreasingTime (Start t1) (Stop t2))
    | (t1, t2) <- is
    ]
  sequence_
    [ unless (t1 <= t2) $ Left (NonIncreasingTime (Stop t1) (Start t2))
    | ((_, t1), (t2, _)) <- zip is (tail is)
    ]

-- | Keep the intervals that include times on or after the given day
--
-- If the given day starts in the middle of an interval, that interval will be
-- adjusted to start in the beginning of the day.
fromDay :: AbstractTime time => DayOf time -> [Interval time] -> [Interval time]
fromDay day = go
  where
    go [] = []
    go (i@(start, stop) : is)
      | stop <= mid = go is
      | start < mid = (mid, stop) : is
      | otherwise   = i : go is
      where
        mid = midnightOf day

-- | Keep the intervals that include times before the given day
--
-- If the given day starts in the middle of an interval, that interval will be
-- adjusted to end right before the new day.
toDay :: AbstractTime time => DayOf time -> [Interval time] -> [Interval time]
toDay day = go
  where
    go [] = []
    go (i@(start, stop) : is)
      | stop <= mid = i : go is
      | start < mid = [(start, mid)]
      | otherwise   = []
      where
        mid = midnightOf day

-- | Remove the log suffix whose events occur on or after the given day
betweenDays ::
     AbstractTime time
  => DayOf time -- ^ Start day (inclusive)
  -> DayOf time -- ^ End day (exclusive)
  -> [Interval time]
  -> [Interval time]
betweenDays start end = toDay end . fromDay start

-- | Add a 'Stop' event at the current time if the last event in the log is a
-- 'Start'
stopNow ::
     time -- ^ Current time (must be after the last 'Start' time in the log)
  -> Log time
  -> Log time
stopNow now log = case log' of
  Start _:_ -> log ++ [Stop now]
  _ -> log
  where
    log' = dropWhile (not . isEvent) $ reverse log

-- | Measure the total logged time
totalTime ::
     AbstractTime time
  => [Interval time]
  -> MeasuredTime time
totalTime = sum . map intervalLength

-- | List all periods in a log
--
-- Each period runs from the corresponding 'Period' marker to the end of the
-- log.
listPeriods :: Log time -> [(String, Log time)]
listPeriods log = [(p, log') | Period p:log' <- tails log]

-- | Parse a 'Punch' event
parsePunch :: Read time => String -> Either (PunchError time) (Punch time)
parsePunch s = case reads s' of
  [(p, "")] -> return p
  _ -> Left $ FormatError s'
  where
    s' = reverse $ dropWhile isSpace $ reverse s

-- | Remove any suffix starting with "--"
stripComment :: String -> String
stripComment "" = ""
stripComment ('-':'-':_) = ""
stripComment (c:cs) = c : stripComment cs

-- | Parse a 'Log'
parseLog :: Read time => String -> Either (PunchError time) (Log time)
parseLog =
  mapM parsePunch .
  filter (not . null) . map (dropWhile isSpace . stripComment) . lines
