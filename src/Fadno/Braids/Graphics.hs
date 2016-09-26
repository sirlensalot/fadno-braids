{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- | Diagrams for braids.
module Fadno.Braids.Graphics
    (
     DrawConf(..),
     renderBraid, BraidDrawF,
     renderStrand, StrandDrawF,
     colorStrands,colorLoops,
     gridStrand,renderBraids,
     randomBraid
    ) where

import Fadno.Braids.Internal
import Diagrams.Prelude hiding (Index,index,width,height,over,lw,Loop)
import Diagrams.Backend.Rasterific
import qualified Data.List as L
import Data.Maybe

import System.Random
import Numeric.Natural
import Control.Monad

-- | Configure braid/strand drawing.
data DrawConf = DrawConf {
      -- | step width (and height)
      stepWidth :: Int
      -- | strand line width
    , strandWidth :: Double
      -- | value > 0 and <= 1 specifying gap for "under" strands
    , crossGap :: Double
}
instance Default DrawConf where def = DrawConf 40 10 0.2


-- | Draw rows and columns of braids with specified stepWidth and draw decorators.
renderBraids :: Braid b a => DrawConf -> [BraidDrawF a] -> FilePath -> [[b a]] -> IO ()
renderBraids dc drawFs fpath bs =
    renderRast fpath
               (stepWidth dc * maxWidth * maxCols)
               (reflectY $ bg white $ frame 0.2 $
                vcat $ map (hcat . map drawB) bs)
    where
      maxCols = maximum $ fmap length bs
      maxWidth = maximum $ fmap stepCount (concat bs)
      drawB = frame 0.8 . drawBraid dc drawFs

-- | Draw a braid with specified stepWidth and draw decorators.
renderBraid :: (Braid b a) => DrawConf -> [BraidDrawF a] -> FilePath -> b a -> IO ()
renderBraid dc drawFs fpath b =
    renderRast fpath (stepWidth dc * stepCount b) (reflectY $ bg white $ frame 0.4 $ drawBraid dc drawFs b)

-- | Draw a strand with specified stepWidth, color, and draw decorators.
renderStrand :: Integral a => DrawConf -> [StrandDrawF a] -> FilePath -> Colour Double -> Strand a -> IO ()
renderStrand dc drawFs fp color s@(Strand ss _l) =
    renderRast fp (stepWidth dc * (length ss + 1))
                   (reflectY $ bg white $ frame 0.4 $
                    runFs drawFs s $ lwO 5 $ lc color $
                    drawStrand dc s)

renderRast :: FilePath -> Int -> Diagram B -> IO ()
renderRast fpath imgWidth = renderRasterific fpath (mkWidth (fromIntegral imgWidth))

drawBraid :: (Integral a,Braid b a) => DrawConf -> [BraidDrawF a] -> b a -> Diagram B
drawBraid dc fs b = mconcat $ runFs fs ss $ map (lwO (strandWidth dc) . drawStrand dc) ss
    where ss = strands b

drawStrand :: Integral a => DrawConf -> Strand a -> Diagram B
drawStrand dc s = foldMap (cap . fromVertices) $ foldl rs [[firstp (head ss)]] $
                  zip [(0 :: Int)..] ss
    where
      ss = toStrand' s
      cap = lineCap LineCapButt
      firstp (y,_,_) = p2 (0,fromIntegral y)
      margin = (1 - crossGap dc) / 2
      rs [] _ = error "no strands"
      rs (ps:pss) (x,(y,p,y')) | p == U = [pt 1,pt (1 - margin)]:(pt margin:ps):pss
                            | otherwise       = (pt 1:ps):pss
          where pt = warpPt x y y'

warpPt :: Integral a => Int -> a -> a -> Double -> P2 Double
warpPt x y y' k = p2 (fromIntegral x + k, fromIntegral y `delt` k)
    where delt | y > y' = (-)
               | y < y' = (+)
               | otherwise = const


-- | Modify braid drawing, with strand data and diagrams.
type BraidDrawF a = [Strand a] -> [Diagram B] -> [Diagram B]
-- | Modify a single-strand drawing, with strand and diagram.
type StrandDrawF a = Strand a -> Diagram B -> Diagram B

-- | Color a braid's strands separately.
colorStrands :: BraidDrawF a
colorStrands _ = zipWith lc colors

-- | Color a braid's loops, such that looped strands have the same color.
colorLoops :: forall a . (Eq a,Show a) => BraidDrawF a
colorLoops ss = zipWith bs ss
    where loops = toLoops ss
          bs :: Strand a -> Diagram B -> Diagram B
          bs s = lc (colors !! seqidx)
              where seqidx = fromMaybe (error "invalid braid, strand not in seqs") $
                             L.findIndex (elem s . _lStrands) loops


-- | Draw a grid behind a single strand.
gridStrand :: Integral a => StrandDrawF a
gridStrand s dia = (foldMap yl [0..fromIntegral yd] <>
                                 foldMap xl [0..xd])
                                 # lc lightgrey `beneath` dia
    where yl,xl :: Int -> Diagram B
          yl i = fromVertices [dp2 (0::Int,i), dp2 (xd,i)]
          xl i = fromVertices [dp2 (i,0::Int), dp2 (i,yd)]
          yd = maximum s - minimum s
          xd = length (_sWeaves s)



colors :: (Ord a, Floating a) => [Colour a]
colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen, darkkhaki]

type Strand' a = [(a,Polarity,a)]

toStrand' :: Strand a -> Strand' a
toStrand' (Strand [] _) = []
toStrand' (Strand ss l) = zipWith (\(a,p) n -> (a,p,n)) ss (tail (map fst ss) ++ [l])


runFs :: [b -> a -> a] -> b -> a -> a
runFs [] _ = id
runFs fs' ss = foldl1 (.) . map ($ ss) $ fs'


dp2 :: (Integral a, Integral a1, Num n) => (a, a1) -> P2 n
dp2 (a,b) = p2 (fromIntegral a, fromIntegral b)



-- | Create a roughly square braid with specified strand count.
randomBraid :: Int -> IO (MultiGen Int)
randomBraid stepcount = MultiGen <$> forM [1..stepcount] (\_ -> randomStep stepcount)

randomStep :: Int -> IO (Step Int)
randomStep stepcount = do
  let r a b = randomRIO (a,b)
      rp = (\b -> if b then O else U) <$> r True False
  mk1 <- Gen <$> r 0 (stepcount `div` 10) <*> rp
  let mkSs :: Natural -> [Gen Natural] -> IO [Gen Natural]
      mkSs p ss | p < fromIntegral stepcount = do
                    s <- Gen <$> (fromIntegral <$> r (0 :: Int) steprange) <*> rp
                    mkSs (p + _gPos s + 2) (s:ss)
                | otherwise = return ss
      steprange = if heur == 0 then 1 else heur
      heur = stepcount `div` 10
  mss <- mkSs (fromIntegral $ _gPos mk1) []
  return $ Step mk1 mss


_aBraid :: Artin Integer
_aBraid = Artin [Gen 0 O,Gen 5 U,Gen 3 O, Gen 2 U,Gen 4 O]
_aStrand :: Strand Integer
_aStrand = head $ strands _aBraid

_testpath :: FilePath
_testpath = "output/test.png"

_testRenderB :: IO ()
_testRenderB = renderBraid def [colorStrands] _testpath _aBraid
_testRenderS :: IO ()
_testRenderS = renderStrand def [] _testpath crimson _aStrand

_testRendRast :: Diagram B -> IO ()
_testRendRast = renderRast _testpath 1000
