module Linear.Intersection where

import Linear

intersectSegments
    :: V2 Double -> V2 Double
    -> V2 Double -> V2 Double
    -> Maybe (V2 Double)
intersectSegments p q r s
    | Just o <- intersectLines p q r s
    , u <- closestPointOnLine p q o
    , v <- closestPointOnLine r s o
    , u > 1e-12, u < 1 - 1e-12
    , v > 1e-12, v < 1 - 1e-12
    = Just o
    | otherwise = Nothing

closestPointOnLine
    :: V2 Double
    -> V2 Double
    -> V2 Double
    -> Double
closestPointOnLine p q r = (r - p) `dot` (q - p) / qd p q

intersectLines
    :: V2 Double
    -> V2 Double
    -> V2 Double
    -> V2 Double
    -> Maybe (V2 Double)
intersectLines p@(V2 x1 y1) q@(V2 x2 y2) r@(V2 x3 y3) s@(V2 x4 y4)
  | den == 0 = Nothing
  | otherwise = Just $ V2 (cx / den) (cy / den)
  where
    V2 dx12 dy12 = p - q
    V2 dx34 dy34 = r - s

    den = dx12 * dy34 - dy12 * dx34

    det12 = x1*y2 - y1*x2
    det34 = x3*y4 - y3*x4

    cx = det12 * dx34 - dx12 * det34
    cy = det12 * dy34 - dy12 * det34
