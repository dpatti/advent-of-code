import Debug.Trace

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = f x >>= \v -> (v :) <$> mapM' f xs

-- main :: IO ()
-- main = do
--   ints <- mapM' return [1..]
--   print . head $ ints

firstIntMaybeInline :: Maybe Int
firstIntMaybeInline = do
  ints <- mapM' return [1..]
  return . head $ ints

  where
    mapM' f [] = return []
    mapM' f (x:xs) = f x `bindMaybe` \v ->
      (v :) `fmapMaybe` mapM' f xs

    bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
    bindMaybe Nothing _ = Nothing
    bindMaybe (Just v) f = f v

    fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
    fmapMaybe _ Nothing = Nothing
    fmapMaybe f (Just v) = Just (f v)

firstIntMaybe :: Maybe Int
firstIntMaybe = do
  ints <- mapM' return [1..]
  return . head $ ints

firstIntIO :: IO Int
firstIntIO = do
  ints <- mapM' return [1..]
  return . head $ ints

main :: IO ()
main = print firstIntMaybe
