-- | Based on <https://hackage.haskell.org/package/delaunay-0.1.0.2>
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BrainWall.Graphics.Delaunay
    ( Triangle (..)
    , triangulate
    , containsPoint
    , delaunay
    ) where

import qualified BrainWall.Box       as Box
import           BrainWall.Triangle
import           BrainWall.V2
import           Data.Function       (on)
import           Data.Hashable       (Hashable (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HSet
import           Data.List
import           Data.Maybe          (fromJust)
import           GHC.Generics        (Generic)

delaunay :: (Hashable a, Integral a, Ord a) => [Triangle a] -> [Triangle a]
delaunay base = nubBy ((==) `on` tri) $ triangulationToTris $ makeDelaunay
    (foldl' (\acc x -> insertTriangle acc x) emptyTriangulation base)
    base
  where
    tri (Triangle p q r) = sort [p, q, r]

-- | Generate the Delaunay triangulation of a set of points
triangulate :: (Hashable a, Integral a) => [V2 a] -> [Triangle a]
triangulate [] = []
triangulate pts' =
  case pts of
    (_:_:_:_) -> triangulationToTris $ removeHelperPoints pts trig
    _tooFew   -> []
  where trig = addPoints (baseTriangulation pts) pts
        pts  = nub pts'

----------- Types -----------

type Pt = V2

-- a type for unordered pairs
data UPair a = UPair !a !a deriving (Eq, Generic, Ord)

instance Hashable a => Hashable (UPair a)

mkUPair :: (Ord a) => a -> a -> UPair a
mkUPair a b = if a <= b then UPair a b else UPair b a

-- `other x y` returns Nothing if x is not part of y and `Just b` where a,b are part of a and x == a
other :: (Eq a) => a -> UPair a -> Maybe a
other x (UPair a b) = if x == a then Just b else if x == b then Just a else Nothing

pElem :: (Eq a) => a -> UPair a -> Bool
pElem x p = case other x p of
  Just _ -> True ; _ -> False

type Triangulation a = HashMap (Pt a) (HashSet (UPair (Pt a)))


----------- Toplevel helpers -----------

-- remove points that are in the triangulation only due to the baseTriangulation
removeHelperPoints :: (Eq a, Hashable a) => [Pt a] -> Triangulation a -> Triangulation a
removeHelperPoints pts trig = removeHelperPoints' ((HMap.keys trig) \\ pts) trig
  where
    removeHelperPoints' [] trig' = trig'
    removeHelperPoints' (p:ps) trig' = case HMap.lookup p trig' of
      Just nbors -> removeHelperPoints' ps $
                   HMap.delete p $
                   HSet.foldl'
                   (\trig'' (UPair nbor1 nbor2) -> HMap.adjust (HSet.filter (not . (p `pElem`))) nbor1 $
                                                  HMap.adjust (HSet.filter (not . (p `pElem`))) nbor2 $
                                                  trig'')
                   trig' nbors
      Nothing -> removeHelperPoints' ps trig' -- shouldn't happen



-- build a triangulation that covers all the points
baseTriangulation :: (Hashable a, Num a, Ord a)=> [Pt a] -> Triangulation a
baseTriangulation pts = foldl' insertTriangle emptyTriangulation [Triangle p1 p2 p3, Triangle p2 p3 p4]
  where p1 = V2 xMin yMin
        p2 = V2 xMin yMax
        p3 = V2 xMax yMin
        p4 = V2 xMax yMax
        -- note: p1 <= p2 <= p3 <= p4.
        (Box.Box (V2 xMin' yMin') (V2 xMax' yMax')) =
            case foldMap (Just . Box.fromV2) pts of
                Nothing -> error "baseTriangulation: empty points"
                Just b  -> b
        [xMin,yMin] = map (\x -> x - 1) [xMin',yMin']
        [xMax,yMax] = map (\x -> x + 1) [xMax',yMax']


triangulationToTris :: (Eq a, Hashable a) => Triangulation a -> [Triangle a]
triangulationToTris trig = concatMap (\(p1,nPts) -> map (\(UPair p2 p3) -> Triangle p1 p2 p3) (HSet.toList nPts)) ptsWithNeighbors
  where
    pts = HMap.keys trig
    ptsWithNeighbors = map (\pt -> (pt, trig HMap.! pt)) pts


emptyTriangulation :: Triangulation a
emptyTriangulation = HMap.empty

insertTriangle :: (Hashable a, Ord a) => Triangulation a -> Triangle a -> Triangulation a
insertTriangle !nbors (Triangle p1' p2' p3') = newTriangulation
  where newTriangulation = foldl (\nbrs (pt,rst) -> HMap.insertWith HSet.union pt rst nbrs) nbors
                           [(p1,HSet.singleton $ mkUPair p2 p3),
                            (p2,HSet.singleton $ mkUPair p1 p3),
                            (p3,HSet.singleton $ mkUPair p1 p2)]
        [p1,p2,p3] = sort $ [p1', p2', p3']

deleteTriangle :: (Hashable a, Ord a) => Triangulation a -> Triangle a -> Triangulation a
deleteTriangle !trig (Triangle p1 p2 p3) = HMap.adjust (HSet.delete $ mkUPair p2 p3) p1 $
                                  HMap.adjust (HSet.delete $ mkUPair p1 p3) p2 $
                                  HMap.adjust (HSet.delete $ mkUPair p1 p2) p3 $
                                  trig


----------- Helpers -----------

dist :: Integral a => Pt a -> Pt a -> Double
dist p1 p2 = sqrt . fromIntegral $ squaredDistance p1 p2

angle :: Integral a => Pt a -> Pt a -> Pt a -> Double
angle !pA !pC !pB = if gamma > pi then pi - gamma else gamma
  where
    gamma = abs $ acos ((a*a + b*b - c*c) / (2 * a * b))
    a = dist pC pB
    b = dist pA pC
    c = dist pA pB

-- based on barycentric coordinates
-- from http://www.blackpawn.com/texts/pointinpoly/default.html
-- note: this may have a tendency to numerical problems. If possible try to refactor so that there is less multiplication
containsPoint :: (Fractional a, Ord a) => Triangle a -> Pt a -> Bool
containsPoint (Triangle p1 p2 p3) pt = u >= 0 && v >= 0 && u + v <= 1
  where v0 = p3 .-. p1
        v1 = p2 .-. p1
        v2 = pt .-. p1
        dot00 = dotProduct v0 v0
        dot01 = dotProduct v0 v1
        dot02 = dotProduct v0 v2
        dot11 = dotProduct v1 v1
        dot12 = dotProduct v1 v2

        denom = (dot00 * dot11 - dot01 * dot01)
        u = (dot11 * dot02 - dot01 * dot12) / denom
        v = (dot00 * dot12 - dot01 * dot02) / denom

-- note: in practice, this only yields a little benefit
fastContainsPoint :: Integral a => Triangle a -> Pt a -> Bool
fastContainsPoint tri@(Triangle (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) pt@(V2 x y) =
  case (min (min x1 x2) x3,max (max x1 x2) x3,min (min y1 y2) y3,max (max y1 y2) y3) of
    (xMin,xMax,yMin,yMax) -> xMin <= x && x <= xMax && yMin <= y && y <= yMax &&
        fmap toRational tri `containsPoint` fmap toRational pt

----------- Triangulation -----------

-- pulls the neighboring triangles out of a triangulation.
-- each of the resuling triangles is of the form (a,b,c) where a and c are in common with the
-- original triangle
neighbors :: (Eq a, Hashable a) => Triangulation a -> Triangle a -> [(Pt a,Triangle a)]
neighbors trig (Triangle p1' p2' p3') = findNeighbors p1' (p2',p3') ++ findNeighbors p2' (p1',p3') ++ findNeighbors p3' (p1',p2')
  where findNeighbors p1 (p2,p3) = HSet.toList $ HSet.map fromJust $ HSet.delete Nothing $
                                   HSet.map (\pr -> case (other p2 pr, other p3 pr) of
                                                (Just p3'',Nothing) | p3 /= p3'' -> Just (p3,Triangle p1 p3'' p2)
                                                (Nothing,Just p2'') | p2 /= p2'' -> Just (p2,Triangle p1 p2'' p3)
                                                (_      ,       _)            -> Nothing)
                                   (trig HMap.! p1)

addPoints :: (Hashable a, Integral a) => Triangulation a -> [Pt a] -> Triangulation a
addPoints trig []        = trig
addPoints !trig (pt:pts) = addPoints (addPoint trig pt) pts

addPoint :: (Hashable a, Integral a) => Triangulation a -> Pt a -> Triangulation a
addPoint trig pt = makeDelaunay
                   trig'
                   splittedTris
  where
    potentialTris = triangulationToTris trig
    tris = filter (`fastContainsPoint` pt) potentialTris
    tri = head tris
    (trig',(t1,t2,t3)) = splitTriangle trig tri pt
    splittedTris = [t1,t2,t3]

splitTriangle :: (Hashable a, Ord a) => Triangulation a -> Triangle a -> Pt a -> (Triangulation a, (Triangle a, Triangle a,Triangle a))
splitTriangle trig (Triangle p1 p2 p3) pt = (trig',(t1,t2,t3))
  where
    -- note: p1 <= p2 <= p3
    trig' = foldl' insertTriangle (deleteTriangle trig (Triangle p1 p2 p3)) [t1,t2,t3]
    t1 = Triangle p1 p2 pt
    t2 = Triangle p1 p3 pt
    t3 = Triangle p2 p3 pt

----------- Delaunay -----------

makeDelaunay :: forall a. (Hashable a, Integral a) => Triangulation a -> [Triangle a] -> Triangulation a
makeDelaunay trig [] = trig
makeDelaunay triangulation (t:ts) =
  case makeDelaunay' triangulation (neighbors triangulation t) of
    Just (trig',insTr,delTr) -> makeDelaunay trig' (foldr (:) (foldl' (flip delete) ts delTr) insTr)
    Nothing -> makeDelaunay triangulation ts
    where
      -- takes a triangle and its neighbors and checks if any edge of the triangle should be flipped
      -- if so, returns the triangulation with the triangle of interest flipped
      makeDelaunay' :: Triangulation a -> [(Pt a, Triangle a)] -> Maybe (Triangulation a, [Triangle a], [Triangle a])
      makeDelaunay' trig nbors = case filter (\(p3, Triangle p1' p3' p2') -> shouldFlip (p1', p2') p3' p3) nbors of
        []                     -> Nothing
        ((p3, Triangle p1' p3' p2'):_) -> Just (flipEdge trig (p1', p2') p3' p3)

-- takes two triangles and checks whether they should be flipped.
-- the first two points are the common points, the latter two those not in the other triangle
shouldFlip :: Integral a => (Pt a, Pt a) -> Pt a -> Pt a -> Bool
shouldFlip (p1, p2) p3 p3' = angle p1 p3 p2 + angle p1 p3' p2 > pi &&
                             angle p3 p1 p3' + angle p3 p2 p3' <= pi

flipEdge :: (Hashable a, Ord a) => Triangulation a -> (Pt a, Pt a) -> Pt a -> Pt a -> (Triangulation a, [Triangle a], [Triangle a])
flipEdge trig (a,b) c c' = (foldl' insertTriangle (foldl' deleteTriangle trig delTr) insTr, insTr, delTr)
  where insTr = [Triangle c a c',Triangle c b c']
        delTr = [Triangle a b c, Triangle a b c']
