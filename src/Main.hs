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

data Role = Eliminator | Therapist | Contestant deriving (Eq, Ord, Enum, Bounded)

data Piece = Piece
    { _pieceFaction :: !Faction
    , _pieceRole :: !Role
    , _pieceHealth :: !Int
    , _pieceTick :: !Int
    } deriving Eq
makeLenses ''Piece

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

  return $ GameState Red Eliminator vs es IM.empty

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
roleShape Eliminator = polygon [V2 (-16) 12, V2 16 12, V2 0 (-12)]
roleShape Therapist = circle 12
roleShape Contestant = polygon [V2 (-12) 12, V2 12 12, V2 12 (-12), V2 (-12) (-12)]

roleHealth :: Role -> Int
roleHealth Eliminator = 3
roleHealth Therapist = 3
roleHealth Contestant = 6

roleTicks :: Role -> Int
roleTicks Eliminator = 300
roleTicks Therapist = 300
roleTicks Contestant = 600

activate :: Faction -> Role -> Piece -> Piece
activate f Eliminator (Piece g r h t) | f /= g = Piece g r (h - 2) t
activate f Therapist (Piece g r h t) | f == g, h < roleHealth r = Piece g r (h + 1) t
activate f Contestant (Piece g r h t) | f /= g = Piece g r h (t + 150)
activate _ _ p = p

isEffective :: Piece -> Piece -> Maybe Faction
isEffective (Piece f r _ _) p
  | activate f r p /= p = Just f
  | otherwise = Nothing

main :: IO (Maybe GameState)
main = runGame Windowed (Box (V2 0 0) (V2 1600 900)) $ do

  setFPS 30

  s0 <- liftIO initialState

  font <- loadFont "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"

  flip execStateT s0 $ forever $ do

    whenM (keyDown KeyR) $ liftIO initialState >>= put

    vs <- use vertices
    cs <- use edges

    lv <- use livePieces

    forM_ cs $ \(i, j) -> let l = line [vs IM.! i, vs IM.! j] in case (lv ^? ix i, lv ^? ix j) of
      (Just p, Just q) -> case (isEffective p q, isEffective q p) of
        (Just f, Just f') -> thickness 5
          $ color (if f == f' then factionColor f else gray) $ color gray l
        (Just f, _) -> thickness 5 $ color (factionColor f) l
        (_, Just f) -> thickness 5 $ color (factionColor f) l
        _ -> color gray l
      _ -> color gray l

    whenM mouseDownR $ currentRole %= cycleEnum
    whenM (keyDown KeySpace) $ currentFaction %= cycleEnum
    faction <- use currentFaction
    role <- use currentRole
    p <- mousePosition
    translate p $ color (factionColor faction & _w .~ 0.5) $ roleShape role
    iforM_ vs $ \i v -> do
      translate v $ case lv ^? ix i of
        Just piece | _pieceHealth piece <= 0 -> livePieces . at i .= Nothing
        Just (Piece f r l t) -> do
          color (factionColor f) $ roleShape r
          translate (V2 12 (-6)) $ color black $ text font 12 $ show l
          color (factionColor f
            & _w .~ if t < 90 then (if even $ t `div` 3 then 1 else 0) else 0.7)
            $ circleOutline (fromIntegral t / 300 * 60 + 12)

          if t >= 0
            then livePieces . ix i . pieceTick -= 1
            else do
              livePieces . ix i . pieceTick += roleTicks role
              forM_ cs $ \(j, k) -> do
                when (i == j) $ livePieces . ix k %= activate f r
                when (i == k) $ livePieces . ix j %= activate f r
        Nothing -> do
          color black $ circle 6
          p' <- mousePosition
          whenM mouseButtonL
            $ when (norm p' < 12) $ do
              livePieces . at i
                ?= Piece faction role (roleHealth role) (roleTicks role)
              currentFaction %= cycleEnum

    tick
