-- This module provides a Chromosome (for Genetic Algorithms) based on an
-- Advanced Assembly program.
module AAChromosome where

import AI.GeneticAlgorithm.Simple
import Control.DeepSeq
import Control.Monad.Writer
import Data.Tuple
import Language.AA.AdvancedAssembly
import System.Random

newtype AAProg = AAProg { getAA :: (BitSeries,BitSeries->Double) }

instance NFData RNmspS
instance NFData Primitive
instance NFData AAProg


rdb :: RandomGen g => g -> (Double,g)
rdb g = (min/rng,g')
  where (lo,hi) = genRange g
        (n,g')  = next g
        rng = fromIntegral $ hi-lo
        min = fromIntegral $ n-lo
rmx :: RandomGen g => g -> Int -> (Int,g)
rmx g m = (truncate $ (fromIntegral m)*d,g')
  where (d,g') = rdb g

rbt :: RandomGen g => g -> (Bit,g)
rbt g = (n>=pivot,g')
  where (lo,hi) = genRange g
        (n,g')  = next g
        pivot = (hi-lo)`quot`2

rbts :: RandomGen g => g -> Int -> (BitSeries,g)
rbts g i = swap $ runWriter j
  where j = foldr (>=>) return (replicate i f) $ g
        f :: RandomGen g => g -> Writer BitSeries g
        f a = writer $ fmap (:[]) $ swap $ rbt a

instance Chromosome AAProg where
  crossover = undefined

  mutation g c = (AAProg (b',f),g')
    where (b,f) = getAA c
          (s,g2) = rmx g (length b)
          (a,z) = splitAt s b
          (r,g3) = (rmx g2 10)
          (n,g4) = rbts g3 (r-5)
          (b',g') = if r < 5 then (a++drop r z,g3) else (a++n++z,g4)

  fitness a = f b
    where (b,f) = getAA a
