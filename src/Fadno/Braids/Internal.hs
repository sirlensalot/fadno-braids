{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fadno.Braids.Internal
    (
     -- * Generators
     Gen(..),gPos,gPol
     ,Polarity(..),power,complement
     -- * Representations
     ,Braid(..)
     ,Artin(..),aGens
     ,MultiGen(..),Step(..),mSteps
    ,insertWithS,insertS,lookupS,deleteS,stepGens,stepToGens,gensToStep
    ,DimBraid(..),dim,dBraid,dSteps,dStrands
     -- * Strands and loops
    ,Strand(..),strand,strand',strands,sSteps,sLast
    ,Loop(..),toLoops,lStrands
     -- * Moves/isotopy
    ,Move(..),inverse,moveH,moveW,Loc(..),lx,ly
    ) where

import Control.Lens hiding (Empty)
import Numeric.Natural
import Control.Arrow

-- | Braid generator "power", as (i + 1) "over/under" i.
-- O[ver] == power 1 (i + 1 "over" i)
-- U[nder] = power -1 (i + 1 "under" i)
data Polarity = U | O deriving (Eq,Show,Enum,Ord)


-- | Polarity to signum or "power" in literature.
power :: Integral a => Polarity -> a
power O = 1
power U = -1

complement :: Polarity -> Polarity
complement O = U
complement U = O




-- | Braid generator pairing position (absolute or relative)
-- and polarity.
data Gen a = Gen { _gPos :: a, _gPol :: Polarity }
    deriving (Eq,Functor,Ord)
instance (Show a) => Show (Gen a) where
    show (Gen a pol) = "Gen " ++ show a ++ " " ++ show pol
makeLenses ''Gen


-- | Braid as "Artin generators" (one-at-a-time).
newtype Artin a = Artin { _aGens :: [Gen a] }
    deriving (Eq,Show,Monoid,Functor)
instance Foldable Artin where
    foldMap f = foldMap f . map _gPos . _aGens
makeLenses ''Artin

-- | Braid "step" of many-at-a-time generators.
-- Absolute-head-offset-tail structure disallows
-- invalid adjacent generators.
-- Example: 'Step (Gen 1 U) [Gen 0 O]' translates to [s1,s3^-1].
data Step a =
    Empty |
    Step {
      -- ^ Absolute-indexed "top" generator
      _sHead :: Gen a
      -- ^ (offset + 2)-indexed tail generators.
    , _sOffsets :: [Gen Natural]
    } deriving (Eq)
makeLenses ''Step
instance Show a => Show (Step a) where
    show Empty = "Empty"
    show (Step h os) = "Step (" ++ show h ++ ") " ++ show os



-- | Insert a gen at absolute index into a 'Step'.
-- Ignores invalid indices, uses function with new, old value
-- for update.
insertWithS :: forall a . Integral a => (Polarity -> Polarity -> Polarity) -> Gen a -> Step a -> Step a
insertWithS _ g Empty = Step g []
insertWithS f (Gen k p) s@(Step (Gen hi hp) sgs)
    | invalid hi = s
    | k < hi = Step (Gen k p) (Gen (fromIntegral $ hi - k - 2) hp:sgs)
    | k == hi = set (sHead.gPol) (f p hp) s
    | otherwise = set sOffsets (ins hi sgs) s
    where invalid i = k + 1 == i || i + 1 == k
          ins :: a -> [Gen Natural] -> [Gen Natural]
          ins i [] = [Gen (fromIntegral $ k - i - 2) p]
          ins i gss@(g@(Gen gi gp):gs)
              | invalid i' = gss
              | k < i' = Gen (fromIntegral $ k - i - 2) p:
                         Gen (fromIntegral $ i' - k - 2) gp:gs
              | k == i' = set gPol (f p gp) g:gs
              | otherwise = g:ins i' gs
              where i' = i + fromIntegral gi + 2

-- | Insert a gen at absolute index into a 'Step'.
-- Ignores invalid indices, overwrites on update.
insertS :: Integral a => Gen a -> Step a -> Step a
insertS = insertWithS const

-- | Lookup by absolute index in a 'Step'.
lookupS :: Integral a => a -> Step a -> Maybe Polarity
lookupS k = lkp . stepToGens where
    lkp [] = Nothing
    lkp (Gen a p:gs) | k == a = Just p
                     | otherwise = lkp gs

-- | Delete/clear a gen at absolute index.
deleteS :: Integral a => a -> Step a -> Step a
deleteS a = gensToStep . del . stepToGens where
    del [] = []
    del (g@(Gen i _):gs) | a == i = gs
                         | otherwise = g:del gs



-- | translate 'Step' to absolute-indexed gens.
stepToGens :: Integral a => Step a -> [Gen a]
stepToGens Empty = []
stepToGens (Step h gs) = reverse $ foldl conv [h] gs
    where conv rs@(Gen p' _:_) (Gen p e) = Gen (fromIntegral p + p' + 2) e:rs
          conv _ _ = error "c'est impossible"

-- | translate absolute-indexed gens to 'Step'.
-- Drops invalid values.
gensToStep :: (Integral a) => [Gen a] -> Step a
gensToStep = foldl (flip insertS) Empty

-- | Iso for valid constructions.
stepGens :: Integral a => Iso' (Step a) [Gen a]
stepGens = iso stepToGens gensToStep


invertS :: Integral a => a -> Step a -> Step a
invertS maxV = foldl (flip insertS) Empty . invGens . stepToGens
    where invGens = over (traverse.gPos) (maxV -)





type instance Index (Step a) = a
type instance IxValue (Step a) = Polarity
instance Integral a => Ixed (Step a) where
  ix k f m = case lookupS k m of
     Just v  -> f v <&> \v' -> insertS (Gen k v') m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Integral a => Monoid (Step a) where
    mempty = Empty
    a `mappend` b = foldl ins a (stepToGens b)
        where ins s g = insertWithS (flip const) g s




-- | Steps of many-at-a-time generators.
newtype MultiGen a = MultiGen { _mSteps :: [Step a] }
    deriving (Eq,Monoid)
instance (Show a) => Show (MultiGen a) where show (MultiGen s) = "MultiGen " ++ show s
makeLenses ''MultiGen


-- | Braid with explicit dimensions (mainly for empty steps/strands)
data DimBraid b a =
    DimBraid { _dBraid :: b a, _dSteps :: Int, _dStrands :: a }
    deriving (Eq,Show)
instance (Monoid (b a), Integral a) => Monoid (DimBraid b a) where
    mempty = DimBraid mempty 0 0
    (DimBraid b1 x1 y1) `mappend` (DimBraid b2 x2 y2) =
        DimBraid (b1 `mappend` b2) (max x1 x2) (y1 + y2)
makeLenses ''DimBraid

dim :: (Braid b a) => b a -> DimBraid b a
dim b = DimBraid b (stepCount b) (strandCount b)




-- | Braid representations.
class (Integral b, Monoid (a b)) => Braid (a :: * -> *) b where

    {-# MINIMAL toGens,minIndex,maxIndex,invert #-}

    -- | "Length", number of "steps"/columns/artin generators.
    stepCount :: a b -> Int
    -- | "N", braid group index, number of strands/rows/"i"s.
    strandCount :: a b -> b
    -- | Common format is a series of "steps" of absolute-indexed generators.
    toGens :: a b -> [[Gen b]]
    -- | Minimum index (i) value
    minIndex :: a b -> b
    -- | Maximum index (i) value. Note this means values of (i+1) obtain, per generators.
    maxIndex :: a b -> b
    -- | Invert indices
    invert :: a b -> a b
    -- | convert to single-gen
    toArtin :: a b -> Artin b
    -- | convert to multi-gen
    toMultiGen :: a b -> MultiGen b

    strandCount a = (maxIndex a + 2) - minIndex a

    stepCount = length . toGens -- inefficient

    toArtin = Artin . concat . toGens

    toMultiGen = MultiGen . map gensToStep . toGens




instance Integral a => Braid (Artin) a where
    toGens = map return . _aGens
    stepCount = length . _aGens
    minIndex (Artin []) = 0
    minIndex b = minimum b
    maxIndex (Artin []) = 0
    maxIndex b = maximum b
    invert b = over (aGens.traverse.gPos) (maxIndex b -) b
    toArtin = id

instance Integral a => Braid (MultiGen) a where
    toGens = map stepToGens . _mSteps
    stepCount = length . _mSteps
    minIndex = minimum . map _gPos . concat . toGens
    maxIndex = maximum . map _gPos . concat . toGens
    invert b = over (mSteps.traverse) (invertS $ maxIndex b) b
    toMultiGen = id

instance (Integral a, Braid b a) => Braid (DimBraid b) a where
    toGens b = gs ++ pad where
        gs = toGens $ _dBraid b
        pad = replicate (stepCount b - length gs) []
    stepCount b = max (_dSteps b) (stepCount $ _dBraid b)
    strandCount b = max (_dStrands b) (strandCount $ _dBraid b)
    minIndex = minIndex . _dBraid
    maxIndex b = minIndex b + strandCount b - 2
    invert = over dBraid invert


-- | Concrete braid strand presentation as values delimited
-- by polarities.
data Strand a = Strand { _sSteps :: [(a,Polarity)], _sLast :: a }
              deriving (Eq,Show)
makeLenses ''Strand
instance Functor Strand where
    fmap f (Strand ss l) = Strand (map (first f) ss) (f l)
instance Foldable Strand where
    foldMap f (Strand ss l) = foldMap f (map fst ss ++ [l])
instance Traversable Strand where
    traverse f (Strand ss l) =
        Strand <$> traverse (\(a,p)->(,) <$> f a <*> pure p) ss <*> f l




-- | Extract a single strand from a braid.
strand :: (Integral a, Braid b a) => a -> b a -> Strand a
strand a = strand' a . toGens

-- | Strand from gen matrix.
strand' :: Integral a => a -> [[Gen a]] -> Strand a
strand' a = foldl srch (Strand [] a) where
    srch (Strand ss l) gs = case lkp l gs of
                              Just (n,p) -> Strand (ss ++ [(l,p)]) n
                              Nothing -> Strand (ss ++ [(l,O)]) l
    lkp _ [] = Nothing
    lkp l (Gen i p:gs) | l == i = Just (succ l,complement p)
                         | l == succ i = Just (pred l,p)
                         | otherwise = lkp l gs

-- | Extract all strands from a braid.
strands :: (Integral a, Braid b a) => b a -> [Strand a]
strands b = map (`strand'` toGens b) [minIndex b..succ $ maxIndex b]

-- | Capture strands into a loop, where '_sLast' of one strand
-- is the first value of the next.
-- Foldable instance ignores "last" values of strands (since they will equal the next head).
newtype Loop a = Loop { _lStrands :: [Strand a] }
            deriving (Eq,Show,Monoid,Functor)
makeLenses ''Loop

instance Foldable Loop where
    foldMap f = foldMap f . toListOf (lStrands.traverse.sSteps.traverse._1)



-- | Find loops in strands.
toLoops :: (Eq a,Show a) => [Strand a] -> [Loop a]
toLoops [] = []
toLoops sa = recurL [] sa where
    shead = fst . head . _sSteps
    findTail s = (==shead (head s)) . _sLast
    recurL ls [] = ls
    recurL ls (a:as) = recurS [a] as
        where recurS s ss =
                  case filter (findTail s) ss of
                    [] -> recurL (Loop s:ls) ss
                    [t] -> recurS (t:s) (filter (not . findTail s) ss)
                    ts -> error $ "More than one strand found with same tail: " ++ show ts


-- | A la Reidemeister.
data Move b i = Move (b i) (b i)
    deriving (Eq,Show)
instance Field1 (Move b i) (Move b i) (b i) (b i) where
    _1 f (Move a b) = (`Move` b) <$> f a
instance Field2 (Move b i) (Move b i) (b i) (b i) where
    _2 f (Move a b) = Move a <$> f b

-- | Flip a move
inverse :: Move b i -> Move b i
inverse (Move a b) = Move b a

-- | Move "height" or strand count
moveH :: Braid a i => Move a i -> i
moveH (Move m1 m2) = max (strandCount m1) $ strandCount m2
-- | Move "width" or step count
moveW :: Braid a i => Move a i -> Int
moveW (Move m1 m2) = max (stepCount m1) $ stepCount m2

-- Coordinate in braid.
data Loc a = Loc { _lx :: Int, _ly :: a } deriving (Eq,Show,Ord)
makeLenses ''Loc
instance Field1 (Loc a) (Loc a) Int Int where
    _1 f (Loc a b) = (`Loc` b) <$> f a
instance Field2 (Loc a) (Loc a) a a where
    _2 f (Loc a b) = Loc a <$> f b
