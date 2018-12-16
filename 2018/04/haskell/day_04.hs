import           Control.Arrow ((&&&))
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (sort, maximumBy)
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust)
import           Data.Time (UTCTime, defaultTimeLocale, nominalDiffTimeToSeconds,
                            diffUTCTime, formatTime, readPTime)
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (readPrec)

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = "A: " ++ fst res ++ "\nB: " ++ snd res ++ "\n"
  where res = solves cs
        cs = sort . map read . lines $ xs

solves :: [LogEntry] -> (String,String)
solves xs = (,) ansA ansB
  where mms = M.unionsWith combineLogMap $ logMap <$> splitEntry xs
        gs = mkGuard mms

        -- FIXME: rewrite to use Guard (gs) as in ansB
        maxSleepGuard = fromJust $ findMaxValueBy (compare `on` fst) mms
        maxSleepId = fst maxSleepGuard
        maxSleepMinute = fst . fromJust . findMaxValue . snd . snd $ maxSleepGuard
        ansA = show $ maxSleepId * maxSleepMinute

        fmt (Just (a,b), c) = (b,a*c)
        fmt _               = undefined
        ansB = show . snd . maximum . map fmt . filter (isJust . fst)
             $ map ((&&&) (findMaxValue . gMinuteMap) gId) gs

data LogEntry = LogEntry UTCTime LogValue
  deriving (Show, Eq)

splitEntry :: [LogEntry] -> [[LogEntry]]
splitEntry = split (dropInitBlank . keepDelimsL $ whenElt isBeginShiftEntry)

-- FIXME:: gTotalSleep is redundant can be calulate using gMinuteMap
-- TODO: DRY refactor
--       part1 is sum the values on gMinuteMap
--       part2 is finding maximum values on gMinuteMap
--
data Guard = Guard
  { gId :: Int
  , gTotalSleep :: Int
  , gMinuteMap  :: Map Int Int
  } deriving Show

type LogMap = (Int,Map Int Int)

mkGuard :: Map Int (Int, Map Int Int) -> [Guard]
mkGuard = M.foldrWithKey f []
  where f :: (Int -> LogMap -> [Guard] -> [Guard])
        f i (s, m) gs = Guard i s m : gs

sleepTime :: (LogEntry, LogEntry) -> Int
sleepTime (LogEntry t0 _,LogEntry t1 _)
  = round $ nominalDiffTimeToSeconds $ diffUTCTime t1 t0 / 60

listToTuple :: [a] -> (a,a)
listToTuple [x,y] = (x,y)
listToTuple _     = error "listToTuple only support list of two elements"

findMaxValue :: Ord a => Map k a -> Maybe (k,a)
findMaxValue m | M.size m > 0 = Just . head $ M.assocs fnd
               | otherwise    = Nothing
  where fnd = M.filter (== mxv) m
        mxv = maximum m

findMaxValueBy :: Ord a => (a -> a -> Ordering) -> Map k a -> Maybe (k,a)
findMaxValueBy f m | M.size m > 0 = Just . head $ M.assocs fnd
                   | otherwise    = Nothing
  where fnd = M.filter (== mxv) m
        mxv = maximumBy f m

-- $setup
-- >>> l0 = "[1518-11-01 00:00] Guard #10 begins shift"
-- >>> l1 = "[1518-11-01 00:05] falls asleep"
-- >>> l2 = "[1518-11-01 00:25] wakes up"
--
-- >>> ls2 = map read [l2,l1,l0] :: [LogEntry]
-- >>> ls1 = map read [l0,l1,l2] :: [LogEntry]
--

-- | from FallsAsleep to WakeUp, Map each minute to 1 if it is a sleeping time
-- >>> minuteMap (read l1,read l2)
--
minuteMap :: (LogEntry, LogEntry) -> Map Int Int
minuteMap (LogEntry t0 _,LogEntry t1 _)
  = M.fromList bs
  where [m0,m1] = getMin <$> [t0,t1]
        getMin = read . formatTime defaultTimeLocale "%M"
        ms = [m0..pred m1] :: [Int]
        ks = [0..59]
        bs = map (\x -> (x, v x)) ks
        v x = if x `elem` ms then 1 else 0

combineLogMap :: LogMap -> LogMap -> LogMap
combineLogMap (a,ma) (b,mb) = (a+b, M.unionsWith (+) [ma,mb])

-- | Given a BeginShift and one or more pair of (FallsAsleep,WakeUp)
--   return map of guard id, total sleep time and minuteMap (of that set)
logMap :: [LogEntry] -> Map Int LogMap
logMap (LogEntry _ (BeginShift i) : xs)
  = M.singleton i (st, mm)
  where cs = chunksOf 2 xs
        st = sum $ sleepTime . listToTuple <$> cs
        mm = M.unionsWith (+) $ minuteMap . listToTuple <$> cs
logMap _ = undefined

-- |
-- >>> read l0 :: LogEntry
-- LogEntry 1518-11-01 00:00:00 UTC (BeginShift 10)
--
-- >>> read l1 :: LogEntry
-- LogEntry 1518-11-01 00:05:00 UTC FallsAsleep
--
-- >>> read l2 :: LogEntry
-- LogEntry 1518-11-01 00:25:00 UTC WakeUp
--

instance Read LogEntry where
  readPrec = lift rpLogEntry

-- |
-- >>> sort ls1 == sort ls2
-- True
--
instance Ord LogEntry where
  compare (LogEntry a _) (LogEntry b _) = compare a b

rpLogEntry :: ReadP LogEntry
rpLogEntry = do
  t <- rpTime
  _ <- R.char ' '
  LogEntry t <$> rpLogValue

rpTime :: ReadP UTCTime
rpTime = readPTime True defaultTimeLocale "[%Y-%m-%d %H:%M]"

type GuardId = Int
data LogValue = BeginShift GuardId
              | WakeUp
              | FallsAsleep
  deriving (Show,Eq)

instance Read LogValue where
  readPrec = lift rpLogValue

isBeginShiftValue :: LogValue -> Bool
isBeginShiftValue (BeginShift _) = True
isBeginShiftValue _              = False

isBeginShiftEntry :: LogEntry -> Bool
isBeginShiftEntry (LogEntry _ v) = isBeginShiftValue v

rpLogValue :: ReadP LogValue
rpLogValue = rpWakeUp R.+++ rpFallsAsleep R.+++ rpBeginShift

rpBeginShift :: ReadP LogValue
rpBeginShift =
  let number :: ReadP Int
      number = read <$> R.munch1 isDigit
  in do
   _ <- R.string "Guard #"
   i <- number
   _ <- R.string " begins shift"
   return $ BeginShift i

rpWakeUp :: ReadP LogValue
rpWakeUp = do
  _ <- R.string "wakes up"
  return WakeUp

rpFallsAsleep :: ReadP LogValue
rpFallsAsleep = do
  _ <- R.string "falls asleep"
  return FallsAsleep
