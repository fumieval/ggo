{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import FreeGame
import Linear.Intersection
import qualified Data.BoundingBox as Box
import qualified Data.IntMap as IM
import System.Random.MWC
import System.Random.MWC.Distributions

data Faction = Red | Green deriving (Eq, Ord, Enum, Bounded)

data Role = Triangle | Circle deriving (Eq, Ord, Enum, Bounded)

data Piece = Piece Faction Role

data GameState = GameState
  { _currentFaction :: Faction
  , _currentRole :: Role
  , _vertices :: IM.IntMap Vec2
  , _edges :: [(Int, Int)]
  , _livePieces :: IM.IntMap Piece
  }
makeLenses ''GameState

genVertices :: GenIO -> Int -> Vec2 -> Double -> [Vec2] -> IO [Vec2]
genVertices _ 0 _ _ vs = return vs
genVertices gen n x0 y0 vs = do
  delta <- V2 <$> standard gen <*> standard gen
  let x1 = x0 + delta * V2 240 240
  let y1 = exp (-sum [72 * 72 / qd x1 p | p <- vs])
          * if Box.isInside x1 $ Box (V2 0 0) (V2 1600 900) then 1 else 0
  let alpha = y1 / y0
  k <- uniform gen
  if k < alpha
    then genVertices gen (n - 1) x1 y1 (x1 : vs)
    else genVertices gen n x0 y0 vs

initialState :: IO GameState
initialState = do
  gen <- createSystemRandom
  vs <- IM.fromList
    <$> zip [0..]
    <$> genVertices gen 40 zero 0 []

  let es = take 80 $ go $ map snd $ sortOn fst
        [ (qd p q + d * 1e7, ((i, p), (j, q)))
        | (i, p) <- IM.toList vs
        , (j, q) <- IM.toList $ snd $ IM.split i vs
        , let d = sum $ fmap (notTooClose p q) $ IM.delete i $ IM.delete j vs]

  return $ GameState Red Triangle vs es IM.empty

  where
    notTooClose p q o
      | d < 0 || d > l = 0
      | otherwise = 1 / qd (o - p) (d *^ u)
      where
        l = distance p q
        u = (q - p) ^/ l
        d = dot u (o - p)

    notCrossing p q ((_, r), (_, s)) = isNothing $ intersectSegments p q r s

    go (((i, p), (j, q)) : xs) = (i, j) : go (filter (notCrossing p q) xs)
    go [] = []

cycleEnum :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnum a
  | a == maxBound = minBound
  | otherwise = succ a

factionColor :: Faction -> RGBA Float
factionColor Red = V4 0.9 0.43 0.56 1
factionColor Green = V4 0.6 0.9 0.56 1

roleShape :: Picture2D p => Role -> p ()
roleShape Triangle = polygon [V2 (-16) 12, V2 16 12, V2 0 (-12)]
roleShape Circle = circle 12

main :: IO (Maybe GameState)
main = runGame Windowed (Box (V2 0 0) (V2 1600 900)) $ do

  setFPS 30

  s0 <- liftIO initialState

  flip execStateT s0 $ forever $ do

    whenM (keyDown KeyR) $ liftIO initialState >>= put

    vs <- use vertices
    cs <- use edges

    forM_ cs $ \(i, j) -> color gray $ line [vs IM.! i, vs IM.! j]

    lv <- use livePieces

    whenM mouseDownR $ currentRole %= cycleEnum
    whenM (keyDown KeySpace) $ currentFaction %= cycleEnum
    faction <- use currentFaction
    role <- use currentRole
    p <- mousePosition
    translate p $ color (factionColor faction & _w .~ 0.5) $ roleShape role
    iforM_ vs $ \i v -> do
      translate v $ case lv ^? ix i of
        Just (Piece f r) -> color (factionColor f) $ roleShape r
        Nothing -> do
          color black $ circle 6
          p' <- mousePosition
          whenM mouseButtonL
            $ when (norm p' < 12) $ livePieces . at i ?= Piece faction role

    tick
