{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Prelude hiding (log)

import Data.List (dropWhileEnd)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
import Text.Read (readEither)

import Punch

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n = reverse . drop n . reverse

-- | Join adjacent intervals
cleanUpIntervals :: AbstractTime time => [Interval time] -> [Interval time]
cleanUpIntervals [] = []
cleanUpIntervals [i] = [i]
cleanUpIntervals (i1:i2:is)
  | snd i1 == fst i2 = cleanUpIntervals ((fst i1, snd i2) : is)
  | otherwise = i1 : cleanUpIntervals (i2:is)

merge ::
     Int   -- ^ Relative frequency of extra elements @1/n@
  -> Gen a -- ^ Extra elements to add
  -> [a]   -- ^ Original list
  -> Gen [a]
merge n ga = go
  where
    genPickExtra = frequency [(1, return True), (n, return False)]

    go [] = do
      pickExtra <- genPickExtra
      if pickExtra then pure <$> ga else return []
    go (a:as) = do
      pickExtra <- genPickExtra
      if pickExtra then (:) <$> ga <*> go (a:as) else (a :) <$> go as

isStart :: Punch time -> Bool
isStart (Start _) = True
isStart _ = False

isStop :: Punch time -> Bool
isStop (Stop _) = True
isStop _ = False

-- | The least significant digit represents time of day. The other digits
-- represent day.
newtype TestDay = TestDay {unTestDay :: Int}
  deriving (Eq, Ord, Show, Read, Enum, Num)

-- | The least significant digit represents time of day. The other digits
-- represent day.
newtype TestTime = TestTime Int
  deriving (Eq, Ord, Show, Read, Num)

unTestTime :: TestTime -> Int
unTestTime (TestTime t) = t

instance AbstractDay TestDay where
  weekDay (TestDay d) = d `mod` 7

instance AbstractTime TestTime where
  type DayOf        TestTime = TestDay
  type MeasuredTime TestTime = TestTime
  dayOf                = TestDay . (`div` 10) . unTestTime
  midnightOf           = TestTime . (* 10) . unTestDay
  intervalLength (t,u) = max 0 (u-t)

instance Arbitrary TestTime where
  arbitrary = do
    NonNegative (Small day) <- arbitrary
    time <- choose (0, 9)
    return $ TestTime (day*10 + time)

newtype Intervals = Intervals {unIntervals :: [Interval TestTime]}
  deriving (Eq)

instance Show Intervals where
  show = show . unIntervals

newtype LOG = LOG {unLOG :: Log TestTime}
  deriving (Eq)

instance Show LOG where
  show = show . unLOG

genTimeStep :: Gen Int
genTimeStep = frequency
  [ (1,) $ choose (6, 30)
  , (8,) $ choose (0, 5)
  ]

intervalsFromSteps ::
     TestTime -- ^ Offset
  -> [MeasuredTime TestTime]
  -> [Interval TestTime]
intervalsFromSteps now (t1:t2:ts') =
  (now + t1, now') : intervalsFromSteps now' ts'
  where
    now' = now + t1 + t2 + 1
intervalsFromSteps _ _ = []

genIntervals :: Gen [Interval TestTime]
genIntervals =
  intervalsFromSteps <$> arbitrary <*> (map TestTime <$> listOf genTimeStep)

instance Arbitrary Intervals where
  arbitrary = Intervals <$> genIntervals
  shrink = map Intervals . shrinkList (const []) . unIntervals

genLog :: Gen (Log TestTime)
genLog = do
  is     <- genIntervals
  trimBeginning <- choose (0, 1)
  trimEnd       <- choose (0, 1)
  let log = drop trimBeginning $ dropFromEnd trimEnd $ fromIntervals is
  merge 8 (Period . pure <$> choose ('A', 'Z')) log

instance Arbitrary LOG where
  arbitrary = LOG <$> genLog
  shrink (LOG log)
    | length log == 2 * (length is) = map (LOG . fromIntervals) $ shrink is
        -- If nothing was lost by calling `intervals`
    | otherwise = [LOG $ fromIntervals is]
    where
      is = intervals log
    -- Not a very efficient way to shrink

chooseDay :: Enum day => (day, day) -> Gen day
chooseDay (from, to) = toEnum <$> choose (fromEnum from, fromEnum to)

genDayFromIntervals :: [Interval TestTime] -> Gen (DayOf TestTime)
genDayFromIntervals is
  | null ts   = dayOf <$> (arbitrary :: Gen TestTime)
  | otherwise = chooseDay (dayOf (minimum ts) - 2, dayOf (maximum ts) + 2)
  where
    ts = map fst is ++ map snd is

genTimeFromIntervals :: [Interval TestTime] -> Gen TestTime
genTimeFromIntervals is = do
  day <- genDayFromIntervals is
  (midnightOf day +) . TestTime <$> choose (0, 9)

genTimeAfterLog :: Log TestTime -> Gen TestTime
genTimeAfterLog log = case reverse $ filter isEvent log of
  Start t : _ -> (t +) <$> (1 +) <$> arbitrary
  Stop t : _ -> (t +) <$> arbitrary
  _ -> arbitrary

noError :: Show err => Either err a -> Bool
noError = either (error . show) (const True)

-- | `cleanUpIntervals` is idempotent
prop_cleanUpIntervals (Intervals is) =
  cleanUpIntervals is' == is'
  where
    is' = cleanUpIntervals is

prop_intervals1 (Intervals is) = noError $ validIntervals is
prop_intervals2 (Intervals is) = intervals (fromIntervals is) == is

prop_intervals3 (LOG log) =
  let log1 = dropWhile isStop $ dropWhileEnd isStart $ filter isEvent log
      log2 = fromIntervals $ intervals log
  in log1 == log2

prop_validLog1 (LOG log) = noError $ validLog log

prop_validLog2 (LOG log) =
  forAll (choose (0, length log)) $ \n -> noError $ validLog $ drop n log

prop_validLog3 (LOG log) =
  forAll (choose (0, length log)) $ \n -> noError $ validLog $ take n log

prop_fromDay1 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    let mid = midnightOf d
    in all (\(start, stop) -> start >= mid && stop > mid) $
         fromDay d is

prop_fromDay2 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    noError $ validIntervals $ fromDay d is

prop_toDay1 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    let mid = midnightOf d
    in all (\(start, stop) -> start < mid && stop <= mid) $
         toDay d is

prop_toDay2 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    noError $ validIntervals $ toDay d is

prop_fromDayToDay1 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    totalTime (toDay d is ++ fromDay d is) == totalTime is

prop_fromDayToDay2 (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    cleanUpIntervals (toDay d is ++ fromDay d is) == cleanUpIntervals is

prop_totalTime (Intervals is) =
  forAll (genDayFromIntervals is) $ \d ->
    totalTime (toDay d is) + totalTime (fromDay d is) == totalTime is

prop_stopNow (LOG log) =
  forAll (genTimeAfterLog log) $ \t ->
    noError $ validLog $ stopNow t log

prop_weekIntervals (Intervals is) =
  forAll (genTimeFromIntervals is) $ \now ->
    let is' = concat $ weekIntervals now is
     in totalTime is' == totalTime is

prop_dayIntervals (Intervals is) =
  forAll (genTimeFromIntervals is) $ \now ->
    let is' = concat $ dayIntervals now is
     in totalTime is' == totalTime is

prop_parseLog (LOG log) =
  either (error . show) id (parseLog $ printLog log) == log
  where
    printLog = unlines . map show



-- Check that the conventional format of the log works (to guard against
-- unexpected regressions in the parser or printer).

timeStamps =
  [ "2020-07-17 16:10:00"
  , "2020-07-17 16:24:18.804513819"
  ]

prop_read_PunchLocalTime =
  all (either error (const True))
    [ readEither time :: Either String PunchLocalTime
    | time <- timeStamps
    ]

prop_show_PunchLocalTime =
  and
    [ show time == s
    | s <- timeStamps
    , let Right time = readEither s :: Either String PunchLocalTime
    ]



--------------------------------------------------------------------------------

return []
runTests = $quickCheckAll

main = do
  passed <- runTests
  if passed then exitSuccess else exitFailure
