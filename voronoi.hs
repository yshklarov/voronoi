import System.Random
import Data.List
import Control.Applicative
import Control.Monad
import Graphics.GD
import Data.Function
import System.Environment
import System.Exit


type Polygon = [Point]
type Site = (Point, Color)


imgsize = (600, 400)
numpoints = 20
sitecolor = (rgb 0 0 0)  -- black
outlinecolor = (rgb 255 0 0)  -- red
sitediam = 3
outlinediam = 5


main = do
    args <- getArgs
    let norm = if args == [] then norm2
               else if head args == "i" then normI
               else normL $ read $ head args
    sites <- randomSites numpoints
    im <- newImage imgsize
    plotRegions im (closest norm sites)
    plotSites im sites
    savePngFile "voronoi.png" im
  where
    closest norm sites query =
       fst $ minimumBy (compare `on` snd)
                       (map (\(p, c) -> ((p, c), norm (p `sub` query))) sites)


-- Plotting functions

plotRegions :: Image -> (Point -> Site) -> IO ()
plotRegions im closestTo = do
    mapM (\(p, c) -> setPixel p c im) cpoints
    return ()
  where
    points = [(x, y) | x <- [0..fst imgsize - 1], y <- [0..snd imgsize - 1]]
    cpoints = map (\p -> (p, snd (closestTo p))) points


plotSites :: Image -> [Site] -> IO ()
plotSites im sites = do
    mapM (\(p, c) -> makeDot p) sites
    return ()
  where
    makeDot p = do
      antiAliased (drawFilledEllipse p (outlinediam, outlinediam)) outlinecolor im
      antiAliased (drawFilledEllipse p (sitediam, sitediam)) sitecolor im


-- Point-generating functions

randomList :: Random a => a -> a -> IO [a]
randomList a b = randomRs (a,b) <$> newStdGen

randomSites :: Int -> IO [Site]
randomSites n = do
     xs <- randomList 0 (fst imgsize - 1)
     ys <- randomList 0 (snd imgsize - 1)
     cs <- randomList 0 255
     return $ take n $ zip (zip xs ys) (map (uncurry3 rgb) (trips cs))


-- Math-related functions

-- l2 (Euclidian) norm
norm2 :: Point -> Double
norm2 p = sqrt (fromIntegral (dx^2 + dy^2))
  where (dx, dy) = p

-- Other norms. Eg. l1 is the "Manhattan" norm.
normL :: Double -> Point -> Double
normL n p = (((abs dx) ** n) + ((abs dy) ** n)) ** (1 / n)
  where (dxi, dyi) = p
        (dx, dy) = (fromIntegral dxi, fromIntegral dyi)

-- l-inf. norm
normI :: Point -> Double
normI p = fromIntegral $ max (abs dx) (abs dy)
  where (dx, dy) = p

sub :: Point -> Point -> Point
(m, n) `sub` (p, q) = (m-p, n-q)


-- Utility functions

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs _ = []

trips :: [a] -> [(a, a, a)]
trips (x:y:z:xs) = (x, y, z) : trips xs
trips _ = []

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

--minimumBy :: Ord a => [b] -> (b -> a) -> b
--minimumBy f [] = error "minimumBy: empty list"
--minimumBy f (x:[]) = x
--minimumBy f (x:y:xs) = if ((f x) < (f y))
--                           then minimumBy f x:xs
--                           else minimumBy f y:xs