{-# LANGUAGE LambdaCase #-}

import Advent

import Control.Monad.Primitive
import Control.Monad.State

import Data.STRef
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MVector
import Data.Vector.Mutable (MVector)
import qualified Data.Map.Strict as Map

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char

parseMove :: String -> Move
parseMove = parseWith $
  choice [ char 's' *> (Spin     <$> decimal)
         , char 'x' *> (Exchange <$> decimal <* char '/' <*> decimal)
         , char 'p' *> (Partner  <$> anyChar <* char '/' <*> anyChar)
         ]

-- Is there no way to create an MVector from a list?
vectorFromList :: PrimMonad m => [a] -> m (MVector (PrimState m) a)
vectorFromList xs = do
  v <- MVector.new (length xs)
  zipWithM_ (MVector.write v) [0..] xs
  return v

iteri :: PrimMonad m => MVector (PrimState m) a -> (Int -> a -> m ()) -> m ()
iteri vec f =
  forM_ [0 .. MVector.length vec - 1] $ \i -> do
    value <- MVector.read vec i
    f i value

-- I originally planned on trying to make this fast by using mutable data
-- structures, also just because I wanted to try it out. I realized halfway
-- through that repeat states are a normal part of part 2, and it turns out that
-- mutability actually hurt in the end since I needed to keep copying old states
-- onto the current state buffer. Though I don't use this in the final solution,
-- I'm keeping it around because it was my first foray into ST and was
-- interesting to write.
danceST :: String -> Int -> [Move] -> String
-- For some reason, we cannot compose [toList] and [create] here. It results in
-- a type error.
danceST initial times moves = Vector.toList $ Vector.create $ do
  states <- newSTRef Map.empty
  ds <- vectorFromList initial
  replicateM_ times $ do
    -- Check if our state was seen
    prev <- Vector.freeze ds
    result <- Map.lookup prev <$> readSTRef states
    case result of
      Just ds' -> Vector.thaw ds' >>= (`MVector.copy` ds)
      Nothing -> do
        routine ds
        next <- Vector.freeze ds
        modifySTRef states (Map.insert prev next)
        return ()
  return ds

 where
   routine ds =
     forM_ moves $ \case
       Spin amt -> do
         ds' <- MVector.clone ds
         let (left, right) = MVector.splitAt (MVector.length ds' - amt) ds'
         iteri left $ \i -> MVector.write ds (i + amt)
         iteri right $ \i -> MVector.write ds i
       Exchange a b -> MVector.swap ds a b
       Partner a b -> do
         v <- Vector.freeze ds
         let i = fromJust $ Vector.elemIndex a v
         let j = fromJust $ Vector.elemIndex b v
         MVector.swap ds i j


dance :: String -> [Move] -> [String]
dance initial moves =
  map Vector.toList . iterateState Map.empty (Vector.fromList initial) $ \ds -> do
    result <- gets $ Map.lookup ds
    case result of
      Just ds' -> return ds'
      Nothing  -> do
        let ds' = foldl' step ds moves
        modify $ Map.insert ds ds'
        return ds'

 where
   step :: Vector Char -> Move -> Vector Char
   step ds = \case
    Spin amt ->
      let (a, b) = Vector.splitAt (Vector.length ds - amt) ds
       in b Vector.++ a
    Exchange a b -> swapIndex a b ds
    Partner a b  -> swapIndex (pos a ds) (pos b ds) ds

   pos :: Eq a => a -> Vector a -> Int
   pos x = fromJust . Vector.elemIndex x

   swapIndex :: Int -> Int -> Vector a -> Vector a
   swapIndex i j v =
     let a = v Vector.! i
         b = v Vector.! j
      in v Vector.// [(i, b), (j, a)]

main :: IO ()
main = solve parse part1 part2
  where
    oneBillion = 1000000
    parse = map parseMove . splitOn ","
    part1 = (!! 1) . dance characters
    part2 = (!! oneBillion) . dance characters
    characters = ['a'..'p']
