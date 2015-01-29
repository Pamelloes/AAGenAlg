{-
Copyright (c) 2015, Joshua Brot

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Joshua Brot nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
-- This module contains a primitive implementation of a Genetic Algorithm
module GenAlg where

import Data.Monoid
import Control.Monad
import Control.Monad.State
import System.Random

-- A class that can be modified by genetic algorithms
class Chromosome a where
  -- A modified version of the Chromosome
  -- In general, a relatively small modification, but large modifications are
  -- not forbidden.
  mutate :: RandomGen g => g -> a -> (a,g)
  -- Combine the two given Chromosomes into a third.
  crossover :: RandomGen g => g -> a -> a -> (a,g)

runGA :: (RandomGen g,Chromosome a) => (g -> (a,g)) -> ([a] -> Either a [a])->g
                                    -> Int -> Double -> (a,g)
runGA n f g c m = go $ zeroGeneration g n c
  where go (a,g) = case (ng g a) of
                     (Left  a', g') -> (a',g')
                     (Right as, g') -> go (as,g')
        ng g a = let (a',g') = nextGeneration n m g a in (f a',g')

zeroGeneration :: (RandomGen g) => g -> (g -> (a,g)) -> Int -> ([a],g)
zeroGeneration g f i = (fmap fst t,snd$last t)
  where s = tail $ iterate (f . snd) (undefined,g)
        t = take i s

nextGeneration :: (RandomGen g,Chromosome a) => (g -> (a,g)) -> Double -> g
                                             -> [a] -> ([a],g)
nextGeneration f c g a = mutate'' c g' (k++n++r)
  where s = length a
        (n,g1) = zeroGeneration g f (s`quot`5)
        ct     = take (7*s`quot`10) $ crossover' g1 a 
        r      = fmap fst ct
        g'     = snd $ last ct
        d      = s - (7*s`quot`10) - (s`quot`5)
        k      = take d a

mutate'' :: (RandomGen g,Chromosome a) => Double -> g -> [a] -> ([a],g)
mutate'' c g a = (runState . mapM state) (fmap (mutate' c) a) $ g

mutate' :: (RandomGen g,Chromosome a) => Double -> a -> g -> (a,g)
mutate' c a g = if c>r then mutate g' a else (a,g')
  where (r,g') = randomR (0.0,1.0) g

crossover' :: (RandomGen g,Chromosome a) => g -> [a] -> [(a,g)]
crossover' g a = mconcat $ tail $ scanr ((.last).next a) [(undefined,g)] [2,3..]
  where next :: (RandomGen g,Chromosome a) => [a] -> Int -> (a,g) -> [(a,g)]
        next a i (_,g) = tail $ scanl (f$a!!i) (undefined,g) (take i a)
        f a b c = crossover (snd$b) a c
