{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module SetExample where

import qualified Data.Set as S

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
