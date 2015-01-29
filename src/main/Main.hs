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

import qualified GenAlg as Z

import AAChromosome
import AI.GeneticAlgorithm.Simple
import Control.Exception (ErrorCall (ErrorCall))
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

runCond :: Int -> State (WriterT String Catch) 
               -> WriterT String Catch (State (WriterT String Catch))
runCond i (S (a,(b,(c,_))))
  | i > 0 = return $ S (a,(b,(c,runCond (i-1))))
  | otherwise = lift $ throwM (ErrorCall "Too long!")

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

dist :: Eq a => [a] -> [a] -> Int
dist a b = last (if lab == 0 then mainDiag
  else if lab > 0 then lowers !! (lab - 1)
       else{- < 0 -}   uppers !! (-1 - lab))
  where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
        uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
        lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
        eachDiag a [] diags = []
        eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag 
                                             : eachDiag a bs diags
          where nextDiag = head (tail diags)
        oneDiag a b diagAbove diagBelow = thisdiag
          where doDiag [] b nw n w = []
                doDiag a [] nw n w = []
                doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n)
                                                        (tail w))
                  where me = if ach == bch then nw 
                             else 1 + min3 (head w) nw (head n)
                firstelt = 1 + head diagBelow
                thisdiag = firstelt : doDiag a b firstelt diagAbove 
                                             (tail diagBelow)
        lab = length a - length b
        min3 x y z = if x < y then x else min y z

diff :: [Word8] -> [Word8] -> Int
diff a b = dist (ur a) (ur b)
  where ur s = mconcat $ fmap up s

runProg :: (Monad m) => (State m -> m (State m))                             
             -> (DataType -> m DataType) -> DStmt -> m (State m, DataType)   
runProg j i d = evaluate f s                                                 
  where p = fst$fst d
        f = snd d
        s = S ([],(defaultNamespace p,(i,j)))  

fit :: DStmt -> Double
fit b = if fail then -1000
  else if succ then 10001 else 100/(fromIntegral df)
  where c = runCatch $ runWriterT $ runProg (runCond 50) handleIO b
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
  putStrLn $ "Score: " ++ show score ++ "( "++ show (100/score) ++ " )"
  return $ score > 10000

main :: IO ()
main = do
  AAProg (p,_) <- runGAIO 5 1.0 (genProg 200 fit) finish
  print $ "Program: " ++ show p
