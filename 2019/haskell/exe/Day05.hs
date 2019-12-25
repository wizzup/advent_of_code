import IntCode

-- 15097178
part_1 :: String -> IO ()
part_1 prog = print $ runForOutput prog [1]

-- 1558663
part_2 :: String -> IO ()
part_2 prog = print $ runForOutput prog [5]

main :: IO ()
main = do
  prog <- getLine

  part_1 prog
  part_2 prog
