{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import qualified Vector as V
import Control.Parallel.Strategies ( using
                                   , parList
                                   , rdeepseq
                                   , rseq
                                   , rpar
                                   , parMap)

import qualified Control.Monad
import qualified System.Environment
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Graphics.Gloss ( display
                      , Display(..)
                      , white
                      , Picture(..))

data Particle = Particle Double V.Vec2 V.Vec2 deriving (Eq, Show, Generic, NFData)

mass :: Particle -> Double
mass (Particle m _ _) = m

pos :: Particle -> V.Vec2
pos (Particle _ p _) = p

vel :: Particle -> V.Vec2
vel (Particle _ _ a) = a

g ::  Double
g = 6.674e-11

e ::  Double
e = 1e-6

force ::  Particle -> Particle -> V.Vec2
force pa pb = V.scale ((g * mass pa * mass pb)/((V.distanceSquared (pos pa) (pos pb) + e) ** 1.5)) (V.sub (pos pb) (pos pa))

forceAll ::  Particle -> [Particle] -> V.Vec2
forceAll p ps = foldl V.add (V.fromTuple (0,0)) (map (force p) (filter (p /=) ps))

applyAcc ::  Double -> V.Vec2 -> Particle -> Particle
applyAcc dt acc part = Particle (mass part) p v
  where
    v = V.add (vel part) (V.scale dt acc)
    p = V.add (pos part) (V.scale dt v  )

updateParticle ::  Double -> [Particle] -> Particle -> Particle
updateParticle dt ps part = applyAcc dt a part
  where f = forceAll part ps
        a = V.scale (1 / mass part) f

updateSystem ::  Double -> [Particle] -> [Particle]
updateSystem dt ps = map (updateParticle dt ps) ps

updateAndPrint :: Int -> Double -> [Particle] -> IO ()
updateAndPrint it dt parts = Control.Monad.when (it > 0) $ do
   let newParts = updateSystem dt parts `using` parList rdeepseq
   putStrLn $ show it ++ ": " ++ show (length $! newParts)
   updateAndPrint (it - 1) dt newParts

solarSystem :: (Eq a, Num a, Show a, Floating a) => [Particle]
solarSystem = let earthMass = 5.972e24
                  earthDist = 1.469e11
                  earthVel  = 29780
                  sunMass   = 1.98855e30
                  mercMass  = 3.3011e23
                  mercDist  = 0.466 * earthDist
                  mercVel   = 47362
                  venusDist = 0.723 * earthDist
                  venusMass = 0.815 * earthMass
                  venusVel  = 35020
                  marsMass  = 0.107 * earthMass
                  marsDist  = 1.52 * earthDist
                  marsVel   = 24077
              in [ Particle earthMass (V.fromTuple (0, earthDist)) (V.fromTuple (earthVel, 0))
                 , Particle sunMass   (V.fromTuple (0, 0))         (V.fromTuple (0       , 0)) ]

genParts :: (Eq a, Num a, Show a, Floating a) =>  Int -> [Particle]
genParts n = [Particle 1 (V.vec2 x y) (V.vec2 0 0) | x <- map fromIntegral [1..n], y <- map fromIntegral [1..n]]

main :: IO ()
main = do
  args <- System.Environment.getArgs
  display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
  if length args /= 2 then
    putStrLn "Usage: grav <iters> <parts>"
  else do
    let iters = read $ head args
    let parts = read $ head $ tail args
    updateAndPrint iters 1e1 (genParts parts)
