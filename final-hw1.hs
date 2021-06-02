{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Array (Array, Ix)
import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Ix as Ix (inRange)

class Indexable c where
  type Index c

  get :: c a -> Index c -> Maybe a

  getUnsafe :: c a -> Index c -> a
  getUnsafe cls c = getUnsafeHelper (get cls c)
    where
      getUnsafeHelper :: Maybe a -> a
      getUnsafeHelper (Just a) = a
      getUnsafeHelper Nothing = error "Out of boundary"

  getIthUnsafe :: c a -> Int -> a
  getIthUnsafe cls i = getUnsafe cls $ (indices cls) !! i

  update :: c a -> Index c -> a -> Maybe (c a)

  updateUnsafe :: c a -> Index c -> a -> c a
  updateUnsafe cls c a = updateUnsafeHelper (update cls c a)
    where
      updateUnsafeHelper :: Maybe a -> a
      updateUnsafeHelper (Just a) = a
      updateUnsafeHelper Nothing = error "Out of boundary"

  indices :: c a -> [Index c]

instance Indexable [] where
  type Index [] = Int

  get = getHelper 0
    where
      getHelper acc [] _ = Nothing
      getHelper acc (x : xs) ind = if acc == ind then Just x else getHelper (acc + 1) xs ind

  update [] _ _ = Nothing
  update x n newVal = Just (updateHelper x n newVal)
    where
      updateHelper (x : xs) n newVal
        | n == 0 = newVal : xs
        | otherwise = x : updateHelper xs (n - 1) newVal

  indices xs = [0 .. length xs - 1]

instance Ix ind => Indexable (Array ind) where
  type Index (Array ind) = ind

  get arr ind = if Ix.inRange (A.bounds arr) ind then Just (arr A.! ind) else Nothing
  update arr ind newVal = if Ix.inRange (A.bounds arr) ind then Just (arr A.// [(ind, newVal)]) else Nothing
  indices = A.indices

newtype Seq a = Seq (Array Int a) deriving (Show)

instance Indexable Seq where
  type Index Seq = Int

  get (Seq arr) ind = if Ix.inRange (A.bounds arr) ind then Just (arr A.! ind) else Nothing

  update (Seq arr) ind newVal = if Ix.inRange (A.bounds arr) ind then Just (Seq $ arr A.// [(ind, newVal)]) else Nothing

  indices (Seq arr) = A.indices arr

class SymbolReadable a where
  toObj :: Char -> a

data DNA = A | T | G | C
  deriving (Eq, Show, Ord)

instance Scorable DNA where
  score x y = if x == y then 5 else -4

data RNA = A' | U' | G' | C'
  deriving (Eq, Show, Ord)

instance SymbolReadable DNA where
  toObj 'A' = A
  toObj 'T' = T
  toObj 'G' = G
  toObj 'C' = C

instance SymbolReadable RNA where
  toObj 'A' = A'
  toObj 'U' = U'
  toObj 'G' = G'
  toObj 'C' = C'

toSeq :: SymbolReadable a => String -> Seq a
toSeq str =
  Seq $ A.listArray (0, length str - 1) $ toObj <$> str

class Scorable a where
  score :: a -> a -> Float

data Atom = Atom {coords :: (Float, Float, Float), chemType :: String, name :: String} deriving (Show)

data AminoAcid = AminoAcid {atomSet :: [Atom], acidName :: String} deriving (Show)

instance Scorable AminoAcid where
  score a a' = if acidName a == acidName a' then 5 else -4

newtype MatrixU a = MatrixU {mat :: Array (Int, Int) a}

createMatrix :: Int -> Int -> a -> MatrixU a
createMatrix rowsN colsN val =
  MatrixU $
    A.array (head allIndices, last allIndices) indicesToVals
  where
    allIndices = [(i, j) | i <- [0 .. rowsN - 1], j <- [0 .. colsN - 1]]
    indicesToVals = zip allIndices (repeat val)

updateIndexM :: MatrixU a -> (Int, Int) -> a -> MatrixU a
updateIndexM (MatrixU m) ind val = MatrixU $ updateIndex m ind val

updateIndex :: Ix ind => Array ind a -> ind -> a -> Array ind a
updateIndex arr ind val = arr A.// [(ind, val)]

scoreForIndex ::
  MatrixU Float ->
  Float ->
  (Int, Int) ->
  Float ->
  Float
scoreForIndex (MatrixU m) gapCost (i, j) matchScore
  | i == 0 && j == 0 = 0
  | i == 0 = fromIntegral j * gapCost
  | j == 0 = fromIntegral i * gapCost
  | otherwise = maximum [insCost, delCost, subCost]
  where
    insCost = m A.! (i - 1, j) + gapCost
    delCost = m A.! (i, j - 1) + gapCost
    subCost = m A.! (i - 1, j - 1) + matchScore

traceBack :: Scorable a => MatrixU Float -> Float -> (Int, Int) -> (Seq a, Seq a) -> Alignment
traceBack _ _ (0, 0) _ = Alignment []
traceBack m g (i, 0) p = Alignment $ Ins:(alignment $ traceBack m g (i - 1, 0) p)
traceBack m g (0, j) p = Alignment $ Del:(alignment $ traceBack m g (0, j - 1) p)
traceBack m gapCost (i, j) p@(s1, s2) = Alignment $ step:(alignment $ traceBack m gapCost (i - di, j - dj) p)
  where
    (step, di, dj) = snd $ maximum
      [(if (((mat m) A.! (i, j)) - ((mat m) A.! (i - di, j - dj))) == x then 1 else 0, (step, di, dj))
      | (step, di, dj, x) <- [(Ins, 1, 0, gapCost), (Del, 0, 1, gapCost), (Match, 1, 1, score (getIthUnsafe s1 (i - 1)) (getIthUnsafe s2 (j - 1)))]]

data CharAlignment = Ins | Del | Match
  deriving (Ord, Eq)

instance Show CharAlignment where
  show Ins      = "^"
  show Del      = "-"
  show Match    = "|"

newtype Alignment = Alignment { alignment :: [CharAlignment] }

instance Show Alignment where
  show (Alignment a) = concat $ map show a

data Trie a = Trie [Int] (Map a (Trie a))
  deriving Show

buildTrie :: forall a. (Eq a, Ord a) => Array Int (Seq a) -> Trie a
buildTrie sequences = buildTrie' (A.assocs sequences) $ Trie [] M.empty
  where
    buildTrie' :: [(Int, Seq a)] -> Trie a -> Trie a
    buildTrie' [] trie = trie
    buildTrie' ((i, seq):seqs) trie =
      buildTrie' seqs $ addSeqToTrie i (map (getIthUnsafe seq) $ indices seq) trie

    addSeqToTrie :: Int -> [a] -> Trie a -> Trie a
    addSeqToTrie seqN [] (Trie endHere children) = Trie (seqN:endHere) children
    addSeqToTrie seqN (c:s) (Trie endHere children) =
      let child = addSeqToTrie seqN s $ M.findWithDefault (Trie [] M.empty) c children
       in Trie endHere $ M.insert c child children

align'' :: forall a. (Eq a, Ord a, Show a, Scorable a) => Seq a -> Array Int (Seq a) -> Array Int Alignment
align'' query seqs = A.array (A.bounds seqs) $ go initialMatrix 0 trie
  where
    gapCost :: Float
    gapCost = -1

    trie = buildTrie seqs

    qIndices = indices query

    nRows = (maximum $ map (length . indices) $ A.elems seqs) + 1
    nCols = 1 + length qIndices

    fillRow :: MatrixU Float -> Int -> [Float] -> MatrixU Float
    fillRow matrix i values = goFill matrix 0 values
      where
        goFill m j [] = m
        goFill m j (val:vals) =
          if j == nCols
            then m
            else goFill (updateIndexM m (i, j) val) (j + 1) vals
    fillRow' :: MatrixU Float -> Int -> a -> MatrixU Float
    fillRow' matrix h c = goFill matrix 0
      where
        goFill m i =
          if i == nCols
            then m
            else goFill (updateIndexM m (h + 1, i)
                         (scoreForIndex m gapCost (h + 1, i)
                         $ if i > 0 then score (getIthUnsafe query $ i - 1) c else 0))
                         (i + 1)

    initialMatrix =
      let emptyMatrix = createMatrix nRows nCols 0
       in fillRow emptyMatrix 0 [i * gapCost | i <- [0..]]

    go :: MatrixU Float -> Int -> Trie a -> [(Int, Alignment)]
    go m h (Trie endHere children) =
      endHereAlignments ++ concat
        [go (fillRow' m h c) (h + 1) child
        | (c, child) <- M.toList children]
      where
        endHereAlignments =
          case endHere of
            [] -> []
            ss@(s:_) ->
              let al = traceBack m gapCost (h, nCols - 1) (seqs A.! s, query)
               in zip endHere $ repeat (Alignment $ reverse $ alignment al)

test :: IO ()
test = do
  let aligns = align'' (toSeq "TACCG" :: Seq DNA)
        (A.listArray (0, 5) $ map toSeq ["AC", "TAC", "CG", "ACT", "C", "TCT"])
  putStrLn $ intercalate "\n" $ map show $ A.elems aligns
