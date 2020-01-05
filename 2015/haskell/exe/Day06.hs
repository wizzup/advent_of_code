import           Data.Foldable (foldr', foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as RP
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (readPrec)

import Common (rpInt)

main :: IO ()
main = do
  partOne >>= print
  partTwo >>= print

partOne :: IO Int
partOne = process one read

partTwo :: IO Int
partTwo = process two (mkNewCommand . read)

process :: ([a] -> b) -> (String -> a) -> IO b
process f g = f . map g . lines <$> readFile "../input/06.txt"

one :: [Command] -> Int
one = M.size . M.filter isOn . gs
  where gs = foldl' f startGrid
        f :: LightGrid -> Command -> LightGrid
        f = flip updateGrid

-- |TODO: refactor `one` and `two` to use common functions
--
two :: [NewCommand] -> Int
two = M.foldr' (+) 0  . gs
  where gs = foldl' f startGrid
        f :: LightGrid -> NewCommand -> LightGrid
        f = flip updateGrid2

-- NOTE: Data.HashMap is 2x slower than Data.Map
--       Ord Pos is quicker than hashing Pos?
-- TODO: try using mutable data structure
--       * array from Data.Array
--       * vector from Data.Vector
--       * ST monad
--
type LightGrid = Map Pos Brightness

isOn :: Brightness -> Bool
isOn = (> 0)

mkGrid :: Int -> LightGrid
mkGrid n = M.fromList [((x,y), 0) | x <- [0..n], y <- [0..n]]

-- | initial grid where all lights is off
startGrid :: LightGrid
startGrid = mkGrid 999

-- | update a single light state (0 = off, 1 = 0n) according to command given
updateLight :: Action -> Brightness -> Brightness
updateLight On  _    = 1
updateLight Off _    = 0
updateLight Toggle 1 = 0
updateLight Toggle 0 = 1
updateLight _      _ = 0

-- | Update LightGrid according to command
--
updateGrid :: Command -> LightGrid -> LightGrid
updateGrid (Cmd a r) l = foldr' f l gs
  where gs = getPos r
        f :: Pos -> LightGrid -> LightGrid
        f = M.adjust (updateLight a)

-- | get list of all position within the recangle
-- >>> getPos ((0,0), (2,2))
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
--
getPos :: RectPos -> [Pos]
getPos ((i,j),(k,l)) = [(x,y) | x <- [i..k], y <- [j..l]]

type Pos = (Int, Int)
type RectPos = (Pos, Pos)

data Action = On | Off | Toggle
  deriving Show

-- |
-- >>> read <$> ["turn on", "turn off", "toggle"] :: [Action]
-- [On,Off,Toggle]
--
instance Read Action where
  readPrec = lift rpAction

rpOn :: ReadP Action
rpOn = RP.string "turn on" >> pure On

rpOff :: ReadP Action
rpOff = RP.string "turn off" >> pure Off

rpToggle :: ReadP Action
rpToggle = RP.string "toggle" >> pure Toggle

rpAction :: ReadP Action
rpAction = rpToggle RP.+++ rpOn RP.+++ rpOff

rpPos :: ReadP Pos
rpPos = do
  x <- rpInt
  _ <- RP.char ','
  y <- rpInt
  pure (x,y)

-- | input command (given) consist of an action and a rectangular region
data Command = Cmd Action RectPos
  deriving Show

-- |
-- >>> read "turn on 0,0 through 999,999" :: Command
-- Cmd On ((0,0),(999,999))
-- >>> read "toggle 0,0 through 999,0" :: Command
-- Cmd Toggle ((0,0),(999,0))
-- >>> read "turn off 499,499 through 500,500" :: Command
-- Cmd Off ((499,499),(500,500))
--
instance Read Command where
  readPrec = lift rpCommand

rpCommand :: ReadP Command
rpCommand = do
  a <- rpAction
  RP.skipSpaces
  b <- rpPos
  RP.skipSpaces
  _ <- RP.string "through"
  RP.skipSpaces
  e <- rpPos
  pure $ Cmd a (b,e)


--------------------------------------------------------------------------------
-- | Part two
--------------------------------------------------------------------------------
-- The phrase turn on actually means that you should increase the brightness
-- of those lights by 1.
-- The phrase turn off actually means that you should decrease the brightness
-- of those lights by 1, to a minimum of zero.
-- The phrase toggle actually means that you should increase the brightness
-- of those lights by 2.
--
type Brightness = Int
data NewCommand = NCmd (Brightness -> Brightness) RectPos

instance Show NewCommand where
  show (NCmd f r) = show r ++ " -> " ++ show (f 0)

mkNewCommand :: Command -> NewCommand
mkNewCommand (Cmd Off r)    = NCmd decBright r
mkNewCommand (Cmd On r)     = NCmd incBright r
mkNewCommand (Cmd Toggle r) = NCmd (incBright . incBright) r

decBright :: Brightness -> Brightness
decBright 0 = 0
decBright n = pred n

incBright :: Brightness -> Brightness
incBright = succ

updateGrid2 :: NewCommand -> LightGrid -> LightGrid
updateGrid2 (NCmd bf r) l = foldr' f l gs
  where gs = getPos r
        f :: Pos -> LightGrid -> LightGrid
        f = M.adjust bf

-- | TODO: BONUS, draw LightGrid for easy debuging
--         using text or picture
-- drawGrid :: IO ()
-- drawGrid = undefined

