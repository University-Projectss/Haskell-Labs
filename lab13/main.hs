addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = let Just r = mx; Just b = my in Just (r + b)

addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do
  x <- mx
  y <- my
  return (x + y)

cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return (x, y)

prod f xs ys = do
  return [f x y | x <- xs, y <- ys]

myGetLine :: IO String
myGetLine = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- myGetLine
      return (x : xs)

prelNo = sqrt

ioNumber = readLn >>= (\noin -> (putStrLn $ "Intrare\n" ++ show noin) >> let noout = prelNo noin in (putStrLn $ "Iesire") >> print noout)
