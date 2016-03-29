{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Braids represented as Haskell types with support for generation and transformations.
--
-- = Braid Typeclass
--
-- `Braid b a` is a typeclass over the braid rep itself and its value type. Since a goal of this library is to use braids for non-mathematical purposes (ie music composition), a Braid can be indexed over any `Integral` type, to support braids representing pitch values in a register for instance.
--
-- = Generators
--
-- All braids are represented using Artin generators as `Gen`, with `Polarity` defining the "power" of a generator as `O`ver or `U`nder.
--
-- Generator indexes differ from the literature in that they are generally *0-indexed* whereas Artin generators are 1-indexed. However, again these braids can represent other ranges of numbers as branch indexes.
--
-- = Braid instances
--
-- `Artin` creates canonical, "one-at-a-time", generator braids.
--
-- `MultiGen` creates "compressed", "many-at-a-time" braids.
--
-- `DimBraid` is for creating "padded" braids, since generators cannot express the absence of a cross.
--
-- = Braid builders
--
-- `bandGen` creates Birman/Ko/Lee-style band generators. In addition, stylized braid builders like `buildBraid` and `terraceBraid` are offered.
--
-- = Transformations/Moves
--
-- In addition to operations like `merge` etc, the type `Move` represents Reidemeister-type isotopy moves. `makeTree` unfolds a potentially-infinite tree representing all possible applications of a move.
--
-- = Graphics
--
-- `drawBraid` and `drawStrands` allow drawings of braids, admitting extra functions for colorizing etc.
--
module Fadno.Braids
    (
     module Fadno.Braids.Internal
    -- * Braid find/merge/clear
     ,find,find',merge,mergeAt,mergeAt',clear,clearMerge
    -- * Isotopy/Reidemeister moves
      ,reidemeister2,reidemeister3,findMoves,applyMove,moves,makeTree
    -- * Band generators
     ,bandGen
    ,module Fadno.Braids.Graphics
    ) where
import Control.Lens hiding (op,(#),Empty)
import Fadno.Braids.Internal
import Fadno.Braids.Graphics
import Data.Tree






-- | Birman, Ko, Lee "band generators" (sigma-s-t)
bandGen :: Integral a => a -> a -> Artin a
bandGen s t | s >= t = error "invalid, s >= t"
            | otherwise = Artin $
                          map (`Gen` O) (reverse [s + 1 .. t - 1]) ++
                          [Gen s O] ++
                          map (`Gen` U) [s + 1 .. t - 1]


-- | Merge one braid into another at offsets.
mergeAt :: (Integral a, Braid b a) => Int -> a -> b a -> b a -> MultiGen a
mergeAt x y ba bb = mergeAt' x y (toGens ba) (toGens bb)

-- | Matrix version.
mergeAt' :: forall a . (Integral a) => Int -> a -> [[Gen a]] -> [[Gen a]] -> MultiGen a
mergeAt' x y ba bb = MultiGen $ loop (offset ba) bb
    where loop :: [[Gen a]] -> [[Gen a]] -> [Step a]
          loop [] [] = []
          loop (a:as) [] = gensToStep a:loop as []
          loop [] (b:bs) = gensToStep b:loop [] bs
          loop (a:as) (b:bs) = foldl (flip insertS) (gensToStep a) b:loop as bs
          offset gens = replicate x [] ++ offsetGenVals y gens

offsetGenVals :: Integral a => a -> [[Gen a]] -> [[Gen a]]
offsetGenVals y = map (map (fmap (+y)))

normalGenVals :: Integral a => [[Gen a]] -> [[Gen a]]
normalGenVals gs = offsetGenVals (- (minimum . map _gPos $ concat gs)) gs

-- | Rectangular gen eraser.
clear :: Integral a => Int -> a -> Int -> a -> [[Gen a]] -> [[Gen a]]
clear x y w h = clrx 0 where
    clrx _ [] = []
    clrx xi (s:ss) = (if xi >= x && xi < x + w then clry s else s):clrx (succ xi) ss
    clry [] = []
    clry (g@(Gen i _):gs) | i >= y && i < y + h = clry gs
                          | otherwise = g:clry gs


-- | Merge one braid into another.
merge :: forall b a . (Integral a, Braid b a) => b a -> b a -> MultiGen a
merge = mergeAt 0 0

_bands :: MultiGen Int
_bands = mergeAt 5 5 (bandGen 0 4) (bandGen 0 9)


_testpath :: FilePath
_testpath = "output/test.png"

-- renderBraid :: Braid b a => Int -> [BraidDrawF a] -> FilePath -> b a -> IO ()

_drawLoops,_drawStrands :: (Show a, Integral a, Braid b a) => b a -> IO ()
_drawLoops = renderBraid 400 [colorLoops] _testpath
_drawStrands = renderBraid 400 [colorStrands] _testpath




-- | Reidemeister move 2, [s1,s1^-1] === flat
reidemeister2 :: Integral a => Move Artin a
reidemeister2 = Move (Artin [Gen 0 O,Gen 0 U]) (Artin [])

-- | Reidemeister move 3, [s1,s2,s1^-1] === [s2^-1,s1,s2], and inverse polarity.
-- Rule: a pattern of [(i,p),(i',p),(i,^p)] moves to [(i',^p),(i,p),(i',p)],
-- where i' = i `op` i where op is plus or minus; with the reversed lists too.
reidemeister3 :: Integral a => [Move Artin a]
reidemeister3 = [ mk (zipWith Gen is ps) | ps <- [[U,U,O],[O,O,U]], is <- [[0,1,0],[1,0,1]]]
    where mk gs = Move (Artin gs) $
                        Artin $ over (traverse.gPos)
                                  (\i -> if i == 0 then 1 else 0) (reverse gs)

_drawReid3s :: IO ()
_drawReid3s = renderBraids 80 [colorStrands] "output/reid3.png" $
             map (\(Move a b) -> [a,b]) (reidemeister3 :: [Move Artin Int])


-- drawMove (last reidemeister3) bands
_drawMove :: (Integral i, Braid a i, Braid b i) => Move a i -> b i -> IO ()
_drawMove m b = renderBraids 80 [colorStrands] "output/move.png"
               [toMultiGen b:map toMultiGen [view _1 m, view _2 m],
                --[toMultiGen b],
                map (\l -> applyMove m l b) (findMoves m b)]




-- | Find all locations of a sub-braid within a braid.
find :: (Integral i, Braid a i, Braid b i) => a i -> b i -> [Loc i]
find ba = find' (normalGenVals $ toGens ba) (stepCount ba) (strandCount ba)

-- | Find all locations of a sub-braid within a braid, matrix version.
find' :: (Integral i, Braid b i) => [[Gen i]] -> Int -> i -> b i -> [Loc i]
find' ba w h bb = [ Loc x y | x <- [0 .. stepCount bb] ,
                    y <- [minIndex bb .. maxIndex bb] ,
                    test x y ]
    where gba = normalGenVals ba
          gbb = toGens bb
          test x y = gbb == toGens (clearMerge gba x y w h gbb)

-- clear generators and merge. TODO: doesn't clear adjacent gens.
clearMerge :: Integral a =>
     [[Gen a]] -> Int -> a -> Int -> a -> [[Gen a]] -> MultiGen a
clearMerge ba x y w h = mergeAt' x y ba . clear x y w h

-- | Locates all move location in a braid.
findMoves :: (Integral i, Braid a i, Braid b i) => Move a i -> b i -> [Loc i]
findMoves m@(Move m1 _) = find' (toGens m1) (moveW m) (moveH m)


-- | Apply a move at a location.
applyMove :: (Integral i, Braid a i, Braid b i) => Move a i -> Loc i -> b i -> MultiGen i
applyMove m@(Move _ m2) (Loc x y)  =
    clearMerge (toGens m2) x y (moveW m) (moveH m) . toGens

-- | Test a collection of moves against a braid and pair results with location.
moves :: (Integral i, Braid a i, Braid b i) => [Move a i] -> b i -> [(Move a i,[Loc i])]
moves mvs target = filter (not . null . snd) $ map (\m -> (m, findMoves m target)) mvs


-- | Unfold a tree of all possible move applications on a braid.
-- A permutation is the permuted braid + the [(move,loc)]s that got us there.
-- Thus the root is (original braid, []); children are [(b1,(move,loc))].
makeTree :: (Integral i, Braid a i, Braid b i) =>
            [Move a i] -> b i -> Tree (MultiGen i,[(Move a i,Loc i)])
makeTree mvs org = unfoldTree go (toMultiGen org,[]) where
    go n@(seed,path) = (n,concatMap (gen seed path) $ moves mvs seed)
    gen target path (mv,locs) = map (\l -> (applyMove mv l target,(mv,l):path)) locs


-- let t = makeTree reidemeister3 _bands
-- let t = makeTree (reidemeister3 ++ map inverse reidemeister3) _bands
-- renderBraids 100 [colorStrands] "allmoves.png" $ nub $ map (return.fst) $ flatten t

zipTail :: (a -> a -> c) -> [a] -> [c]
zipTail f = zipWith f <*> tail
