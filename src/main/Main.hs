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
-- This is the main file for AAGenAlg
module Main where

import AAChromosome
import AI.GeneticAlgorithm.Simple
import Control.Exception
import Control.Monad.Catch.Pure
import Control.Monad.Writer
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Language.AA.AdvancedAssembly
import System.Random

handleIO :: DataType -> WriterT String Catch DataType                            
handleIO a@(_,BString s) = lg a s                                  
  where lg :: DataType -> BitSeries -> WriterT String Catch DataType             
        lg a [] = return a
        lg d s = do
          let (a,b) = splitAt 8 s
          let c = chr (fromIntegral $ bsToInt a)                        
          tell [c]
          lg d b
handleIO d = handleIO (cstring d)      

up :: Word8 -> [Bit]                                                             
up w = [h,g,f,e,d,c,b,a]                                                         
  where z a = a /= 0                                                             
        a=z$w.&.0x1                                                              
        b=z$w.&.0x2                                                              
        c=z$w.&.0x4                                                              
        d=z$w.&.0x8                                                              
        e=z$w.&.0x10                                                             
        f=z$w.&.0x20                                                             
        g=z$w.&.0x40                                                             
        h=z$w.&.0x80 

diff :: [Word8] -> [Word8] -> Int
diff [] b = (-1) * (length b)
diff a [] = (-1) * (length a)
diff (a:as) (b:bs) = d + (diff as bs)
  where d = foldr (+) 0 $ zipWith (\a b->if a==b then 1 else 0) (up a) (up b)

runCond :: Int -> State (WriterT String Catch) 
               -> WriterT String Catch (State (WriterT String Catch))
runCond i (S (a,(b,(c,_))))
  | i > 0 = return $ S (a,(b,(c,runCond (i-1))))
  | otherwise = lift $ throwM (ErrorCall "Too long!")

fit :: BitSeries -> Double
fit b = if fail then -1000
  else if succ then 10001
    else (fromIntegral df)
  where c = runCatch $ runWriterT $ runProgram' (runCond 20) handleIO b
        fail = case (c) of { Left _ -> True; Right _ -> False }
        Right ((s,dt),st) = c
        desired= "Hello World!"
        succ   = st==desired
        df     = diff (B.unpack $ C.pack desired) 
                      (B.unpack $ C.pack st)

finish :: AAProg -> Int -> IO Bool
finish c i = do
  let score = fitness c
  putStrLn $ "Generation: " ++ show i
  putStrLn $ "Score: " ++ show score
  return $ score > 10000

main :: IO ()
main = do
  AAProg (p,_) <- runGAIO 50 0.7 (genProg 50 fit) finish
  print $ "Program: " ++ show p
