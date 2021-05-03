{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module StateWalk where

import System.IO
import Map 
import Data.Complex
import Control.Monad       (liftM, ap,mapM,join)
import Data.Ratio
import qualified AsMonad as AM 
import StateMtwo
import Control.Monad.State
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

type QDist  = AsMonDist(Complex Float)
type CDist = AsMonDist(Float) 

stateInitCondQMap :: (Num a,Ord a) =>StateM2 (QDist) a
stateInitCondQMap = StateT $ func4( fromListAM[((0,H),1 :: Complex Float)],  fromListAM[((0,T),1 :: Complex Float)])

sqrRoot = 1/sqrt(2) :: Complex Float 
stateHadamardCoinMap :: (Ord a, Num a) => a -> StateM2 (QDist) a 
stateHadamardCoinMap a = StateT $ func4 (fromListAM[((a-1,H),sqrRoot),((a+1,T),sqrRoot)],fromListAM[((a-1,H),sqrRoot),((a+1,T),-sqrRoot)])

stateQuantumWalkNMap :: (Ord a,Num a) => Int -> StateM2 (QDist) a -> StateM2 (QDist) a
stateQuantumWalkNMap (0) state = state
stateQuantumWalkNMap n state =  stateQuantumWalkNMap (n-1) (state >>= stateHadamardCoinMap)

probsState :: (Ord a, Num a) => Int -> StateM2 (CDist) a 
probsState n = mapStateT getProbsAM $ stateQuantumWalkNMap n stateInitCondQMap

bla = runStateT ( probsState 10) H

stateWalkProbs :: (Ord a, Num a) => Int -> CDist a
stateWalkProbs n = evalStateT(probsState n) T 

walkToList :: (Ord a, Num a,Ord k) => AsMonDist a k -> [(k,a)] 
walkToList a = toListAM a 

stateExample5 = stateWalkProbs 20
listExample :: [(Int,Float)]
listExample = walkToList(stateExample5)

plotStateWalk  =
   toFile def "stateWalk.svg" $ do
   layout_title .= "Amplitude Modulation"
   setColors [opaque blue, opaque red]
   plot (line "am" [listExample])
   plot (points "am points" (listExample))

-- Different coin

sqrRootIm = 0 :+ 1/sqrt(2) :: Complex Float 
stateYCoinMap :: (Ord a, Num a) => a -> StateM2 (QDist) a 
stateYCoinMap a = StateT $ func4 (fromListAM[((a-1,H),sqrRoot),((a+1,T),sqrRootIm)],fromListAM[((a-1,H),sqrRootIm),((a+1,T),sqrRoot)])

stateQuantumWalkYMap :: (Ord a,Num a) => Int -> StateM2 (QDist) a -> StateM2 (QDist) a
stateQuantumWalkYMap (0) state = state
stateQuantumWalkYMap n state =  stateQuantumWalkNMap (n-1) (state >>= stateYCoinMap)

probsStateY :: (Ord a, Num a) => Int -> StateM2 (CDist) a 
probsStateY n = mapStateT getProbsAM $ stateQuantumWalkYMap n stateInitCondQMap

stateWalkYProbs :: (Ord a, Num a) => Int -> CDist a
stateWalkYProbs n = evalStateT(probsStateY n) T 

stateExampleY = stateWalkYProbs 20
listExample2 :: [(Int,Float)]
listExample2 = walkToList(stateExampleY)

example2 n = runStateT $ stateQuantumWalkYMap n stateInitCondQMap  

plotStateWalkY  =
   toFile def "stateWalkY.svg" $ do
   layout_title .= "Amplitude Modulation"
   setColors [opaque blue, opaque red]
   plot (line "am" [listExample2])
   plot (points "am points" (listExample2))

-- Cube
stateInitCubeCondQMap :: StateM3 (QDist) (Bool,Bool,Bool) 
stateInitCubeCondQMap = StateT $ func4Cube(x,y,z) where 
   x = fromListAM[((((False,False,False),One)),1 :: Complex Float)]  
   y = fromListAM[((((False,False,False),Two)),1 :: Complex Float)]  
   z = fromListAM[((((False,False,False),Three)),1 :: Complex Float)]  

coinAmplitude = 1/sqrt(3) :+ 0 :: Complex Float
d=3
w = mkPolar 1 (2*pi/d)
stateCubeCoinMap :: (Bool,Bool,Bool) -> StateM3 (QDist) (Bool,Bool,Bool) 
stateCubeCoinMap (x,y,z) = StateT $ func4Cube (x',y',z') where
  x' = fromListAM[(((not(x),y,z),One),coinAmplitude),(((x,not(y),z),Two),coinAmplitude),(((x,y,not(z)),Three),coinAmplitude)]
  y' = fromListAM[(((not(x),y,z),One),coinAmplitude),(((x,not(y),z),Two),coinAmplitude*w),(((x,y,not(z)),Three),coinAmplitude*(w*w))]
  z' = fromListAM[(((not(x),y,z),One),coinAmplitude),(((x,not(y),z),Two),coinAmplitude*(w*w)),(((x,y,not(z)),Three),coinAmplitude*w)]

stateQuantumWalkCubeMap ::  Int -> StateM3 (QDist) (Bool,Bool,Bool) -> StateM3 (QDist) (Bool,Bool,Bool) 
stateQuantumWalkCubeMap (0) state = state
stateQuantumWalkCubeMap n state =  stateQuantumWalkCubeMap (n-1) (state >>= stateCubeCoinMap)

probsStateCube :: Int -> StateM3 (CDist) (Bool,Bool,Bool) 
probsStateCube n = mapStateT getProbsAM $ stateQuantumWalkCubeMap n stateInitCubeCondQMap

stateWalkCubeProbs ::  Int -> CDist (Bool,Bool,Bool) 
stateWalkCubeProbs n = evalStateT(probsStateCube n) One 

--TODO: Fazer histograma para walk classica cubo e walk quantica cubo.
--TODO: Fazer grover coin e dft.

convFunc False = 0
convFunc True = 1

