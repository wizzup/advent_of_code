import IntCode

main :: IO ()
main = do
  prog <- getLine

  -- 2204990589
  let part_1 = runForOutput prog [1]
  print part_1

  -- -- 50008 (very slow)
  let part_2 = runForOutput prog [2]
  print part_2
