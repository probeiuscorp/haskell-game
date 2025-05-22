{-# LANGUAGE BangPatterns #-}

module Game.Casts (scoreStabCast, scoreSlashCast, scoreSweepCast) where

import Game.Prelude
import SDL (unangle, Metric (norm))
import Data.Foldable (Foldable(foldl'))

-- | First position is caster position, then cast pointer position
type ScoreCast = [(World, World)] -> Double

-- | A list of the same values scores a perfect 0.
scoreDeviation :: [Double] -> Double
scoreDeviation = stddev

-- | Effective stab casts keep constant angle from the caster.
scoreStabCast :: ScoreCast
scoreStabCast pts = scoreDeviation $ unangle . uncurry subtract <$> pts

-- | Effective slash casts keep a constant angle between themselves.
-- Weapons vary in their optimal angle.
scoreSlashCast :: Double -> ScoreCast
scoreSlashCast optimalAngle pts = angleScore + deviationScore
  where
    angleScore = abs $ optimalAngle - mean angles
    deviationScore = scoreDeviation angles
    angles = dv $ snd <$> pts
    dv :: [World] -> [Double]
    dv (x:y:xs) = (let d = y - x in if d /= 0 then (unangle d :) else id) $ dv (y : xs)
    dv _ = []

-- | Effective sweep casts keep constant distance from the caster.
scoreSweepCast :: ScoreCast
scoreSweepCast pts = scoreDeviation $ norm . uncurry (-) <$> pts

mean :: Fractional a => [a] -> a
mean = fst . foldl' addElement (0, 0)
  where
    addElement (!m, !n) x = (m + (x-m)/(n+1), n+1)

stddev :: Floating a => [a] -> a
stddev xs = sqrt . mean . fmap ((^ (2 :: Int)) . (-) (mean xs)) $ xs
