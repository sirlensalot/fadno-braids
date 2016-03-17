{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fadno.Braids.Graphics
    (
     renderBraid, BraidDrawF,
     renderStrand, StrandDrawF,
     colorStrands,colorLoops,
     gridStrand,renderBraids
    ) where

import Fadno.Braids.Internal
import Diagrams.Prelude hiding (Index,index,width,height,over,lw,Loop)
import Diagrams.Backend.Rasterific
import qualified Data.List as L
import Data.Maybe


--
-- Braid Drawing
--

_aBraid :: Artin Integer
_aBraid = Artin [Gen 0 O,Gen 5 U,Gen 3 O, Gen 2 U,Gen 4 O]
_aStrand :: Strand Integer
_aStrand = head $ strands _aBraid

_testpath :: FilePath
_testpath = "output/test.png"

_testRenderB :: IO ()
_testRenderB = renderBraid 40 [colorStrands] _testpath _aBraid
_testRenderS :: IO ()
_testRenderS = renderStrand 40 [] _testpath crimson _aStrand

_testRendRast :: Diagram B -> IO ()
_testRendRast = renderRast _testpath 1000

renderBraids :: Braid b a => Int -> [BraidDrawF a] -> FilePath -> [[b a]] -> IO ()
renderBraids stepWidth drawFs fpath bs =
    renderRast fpath
               (stepWidth * maxWidth * maxCols)
               (bg white $ frame 0.2 $
                vcat $ map (hcat . map drawB) bs)
    where
      maxCols = maximum $ fmap length bs
      maxWidth = maximum $ fmap stepCount (concat bs)
      drawB = frame 0.8 . (`drawBraid` drawFs)

-- | Draw a braid with specified stepWidth and draw decorators.
renderBraid :: Braid b a => Int -> [BraidDrawF a] -> FilePath -> b a -> IO ()
renderBraid stepWidth drawFs fpath b =
    renderRast fpath (stepWidth * stepCount b) (bg white $ frame 0.4 $ drawBraid b drawFs)

-- | Draw a strand with specified stepWidth, color, and draw decorators.
renderStrand :: Integral a => Int -> [StrandDrawF a] -> FilePath -> Colour Double -> Strand a -> IO ()
renderStrand sw drawFs fp color s@(Strand ss _l) =
    renderRast fp (sw * (length ss + 1))
                   (bg white $ frame 0.4 $
                    runFs drawFs $ lwO 5 $ lc color $
                    drawStrand s)
        where runFs = foldl1 (.) . map ($ s)

renderRast :: FilePath -> Int -> Diagram B -> IO ()
renderRast fpath imgWidth = renderRasterific fpath (mkWidth (fromIntegral imgWidth))

colors :: (Ord a, Floating a) => [Colour a]
colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen, darkkhaki]

type Strand' a = [(a,Polarity,a)]

toStrand' :: Strand a -> Strand' a
toStrand' (Strand [] _) = []
toStrand' (Strand ss l) = zipWith (\(a,p) n -> (a,p,n)) ss (tail (map fst ss) ++ [l])

drawStrand :: Integral a => Strand a -> Diagram B
drawStrand s = foldMap (cap . fromVertices) $ foldl rs [[firstp (head ss)]] $
                  zip [(0 :: Int)..] ss
    where
      ss = toStrand' s
      cap = lineCap LineCapButt
      firstp (y,_,_) = p2 (0,fromIntegral y)
      rs [] _ = error "No Pants!"
      rs (ps:pss) (x,(y,p,y')) | p == U = [pt 1,pt 0.6]:(pt 0.4:ps):pss
                            | otherwise       = (pt 1:ps):pss
          where pt = warpPt x y y'

warpPt :: Integral a => Int -> a -> a -> Double -> P2 Double
warpPt x y y' k = p2 (fromIntegral x + k, fromIntegral y `delt` k)
    where delt | y > y' = (-)
               | y < y' = (+)
               | otherwise = const


type BraidDrawF a = [Strand a] -> [Diagram B] -> [Diagram B]
type StrandDrawF a = Strand a -> Diagram B -> Diagram B

-- | Color a braid's strands separately.
colorStrands :: BraidDrawF a
colorStrands _ = zipWith lc colors

drawBraid :: Integral a => Braid b a => b a -> [BraidDrawF a] -> Diagram B
drawBraid b fs = mconcat $ runFs fs $ map (lwO 10 . drawStrand) ss
    where runFs = foldl1 (.) . map ($ ss)
          ss = strands b

-- | Color a braid's loops, such that looped strands have the same color.
colorLoops :: forall a . (Eq a,Show a) => BraidDrawF a
colorLoops ss = zipWith bs ss
    where loops = toLoops ss
          bs :: Strand a -> Diagram B -> Diagram B
          bs s = lc (colors !! seqidx)
              where seqidx = fromMaybe (error "invalid braid, strand not in seqs") $
                             L.findIndex (elem s . _lStrands) loops

{-
labelIndex :: BraidDrawF
labelIndex (Braid _ _ _ idx _) = zipWith f (zip idx [0..]) where
    f :: (Int,Int) -> Diagram B -> Diagram B
    f (v,i) = (<> alignedText 1 0.5 (show v) #
                  fontSize (local 0.35) #
                  moveTo (p2 (-0.3,fromIntegral i)))

labelStrand :: StrandDrawF
labelStrand (Strand _ idx) dia = foldl f dia (zip idx [(0 :: Int)..])
    where f d (v,i) = d <> alignedText 1 0.5 (show v) #
                      fontSize (local 0.35) #
                      moveTo (p2 (-0.3,fromIntegral i))
-}

gridStrand :: Integral a => StrandDrawF a
gridStrand s dia = (foldMap yl [0..fromIntegral yd] <>
                                 foldMap xl [0..xd])
                                 # lc lightgrey `beneath` dia
    where yl,xl :: Int -> Diagram B
          yl i = fromVertices [dp2 (0::Int,i), dp2 (xd,i)]
          xl i = fromVertices [dp2 (i,0::Int), dp2 (i,yd)]
          yd = maximum s - minimum s
          xd = length (_sSteps s)

dp2 :: (Integral a, Integral a1, Num n) => (a, a1) -> P2 n
dp2 (a,b) = p2 (fromIntegral a, fromIntegral b)

{-
strandVertex :: StrandDrawF
strandVertex (Strand ss _) dia = foldMap f (zip ss [(0 :: Int)..]) <>
                                 lastDot (last ss) # lwO 2 <> dia
    where f ((v,w),i) | weft w /= UNDER = vdot i v
                      | otherwise = mempty
          vdot :: Int -> Int -> Diagram B
          vdot i v = moveTo (dp2 (i,v)) $ fc black $ circle 0.1
          lastDot (v,w) = vdot (length ss) (warpf (warp w) v 1)

drawTerraceCol :: Int -> StrandDrawF
drawTerraceCol col (Strand ss idx) dia =
    dia <> mconcat (zipWith lc colors $ map (lwO 2 . step) $
           terracedCol False (length idx) (ss !! col))
    where step :: StrandStep -> Diagram B
          step (y,w) | weft w == UNDER = fromVertices [dp2(col,y),pt 0.4] <>
                                             fromVertices [pt 0.6, pt 1]
                         | otherwise       = fromVertices [dp2(col,y),pt 1]
                         where pt = warpPt col y w

-}
