{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module SetExample where

import qualified Data.Set as S
import qualified Set as SM 

--Data.Set examples
exampleSet1 = S.fromList [1,2,3]
exampleSet2 = S.fromList [4,5,6]
exampleUnion = S.union exampleSet1 exampleSet2
exampleDifference = S.difference (S.insert 5 exampleSet1) exampleSet2
exampleIntersection = S.intersection (S.insert 5 exampleSet1) (S.insert 3 exampleSet2)

exampleSetList :: [S.Set Int]
exampleSetList = [exampleSet1, exampleSet2]
exampleSetListUnion = S.unions exampleSetList

exampleSingleton :: S.Set Int
exampleSingleton = S.singleton 1
exampleEmpty = S.empty

exampleDelete = S.delete 3 exampleSet2
exampleInsert = S.insert 7 exampleSet2

--SetMonad Examples
--exampleSM1 = SM.fromList[1,2,3]
--exampleSM2 = SM.fromList [4,5,6]
--exampleSMUnion = SM.union exampleSM1 exampleSM2
--exampleSMDifference = SM.difference (SM.insert 5 exampleSM1) exampleSM2
--exampleSMIntersection = SM.intersection (SM.insert 5 exampleSM1) (SM.insert 3 exampleSM2)
--
--exampleSMList :: [SM.SetMon Int]
--exampleSMList = [exampleSM1, exampleSM2]
--exampleSMListUnion = SM.unions exampleSMList
--
--exampleSMSingleton :: SM.SetMon Int
--exampleSMSingleton = SM.singleton 1
--exampleSMEmpty = SM.empty
--
--exampleSMDelete = SM.delete 3 exampleSM2
--exampleSMInsert = SM.insert 7 exampleSM2
