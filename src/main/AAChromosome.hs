-- This module provides a Chromosome (for Genetic Algorithms) based on an
-- Advanced Assembly program.
module AAChromosome where

import AI.GeneticAlgorithm.Simple
import Control.DeepSeq
import Control.Monad.Writer
import Data.Tuple
import Language.AA.AdvancedAssembly
import System.Random

newtype AAProg = AAProg { getAA :: (DStmt,DStmt->Double) }

bsToDS :: BitSeries -> DStmt
bsToDS = parseST loadStmt

dsToBS :: DStmt -> BitSeries
dsToBS = fst . fst

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

genProg :: RandomGen g => Int -> (DStmt->Double) -> g -> (AAProg,g)
genProg i f g = (AAProg (bsToDS b,f),g')
  where (l,g1) = rmx g i
        (b,g') = rbts g1 l

mutate :: RandomGen g => g -> BitSeries -> (BitSeries,g)
mutate g b = (b',g')
  where (s,g2) = rmx g (length b)
        (a,z) = splitAt s b
        (r,g3) = (rmx g2 26)
        (n,g4) = rbts g3 (r-13)
        (b',g') = if r < 13 then (a++drop r z,g3) else (a++n++z,g4)

{-
combine :: RandomGen g => g -> BitSeries -> BitSeries -> (g,BitSeries)
combine g []     _      = (g,[])
combine g _      []     = (g,[])
combine g a b = fmap ((if c then a' else b')++) (combine g' as bs)
  where (c,g1) = rbt g
        (l,g') = rmx g1 25
        a' = take l a
        b' = take l b
        as = drop l a
        bs = drop l b
-}
combine :: RandomGen g => g -> BitSeries -> BitSeries -> (g,BitSeries)
combine g a b = (g,(take s a)++(drop s b))
  where l = min (length a) (length b)
        (s,g') = rmx g l

instance Chromosome AAProg where
  crossover g (AAProg (d,f)) (AAProg (e,h)) = ([d'],g')
    where a = dsToBS d
          b = dsToBS e
          (g1,a') = combine g a b
          (g',b') = combine g1 a b
          d' = AAProg (bsToDS a',f)
          e' = AAProg (bsToDS b',h)
  
  mutation g (AAProg (d,f)) = (AAProg (bsToDS b,f),g')
    where (b,g') = mutate g $ dsToBS d

  fitness a = f b
    where (b,f) = getAA a
