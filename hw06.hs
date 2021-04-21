-- С помощью таких конструкций можно включать разные расширения языка.
-- Поподробнее с тем, что это такое, мы познакомимся чуть позже.
-- Но, как вы могли догадаться, для выполнения домашки нам понадобились какие-то расширения!
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Array (Array, Ix)
import qualified Data.Array as A ((!), (//), array, bounds, indices, listArray)
import qualified Data.Ix as Ix (inRange)

-- | Класс типов 'Indexable' характеризует структуры данных,
--   к элементам которых можно обращаться по индексу.
--
--   Важно, что характеризует он именно какую-то __структуру__ данных @c@.
--   @c@ может быть 'List', 'Matrix' или 'Array ind' (у 'Array' два типовых
--   параметра — тип индекса, т.е. @ind@, и тип значений в массиве).
--
class Indexable c where
  -- | Специальный синтаксис для определения каких-то типов,
  --   связанных с @c@ и необходимых для того, чтобы он мог быть представителем данного класса типов.
  --   Тут мы говорим, что c @c@ связан какой-то тип, являющийся в нём индексом.
  --
  --   Это нужно для того, чтобы зафиксировать разные индексы для разных структур данных.
  --   Понятно, что для 'List' индексом является 'Int'.
  --   При этом для 'Matrix' индексом является уже пара интов: (Int, Int).
  --   Для 'Array ind' индексом неожиданным образом является @ind@!
  --
  --   Тип @Index c@ называется __ассоциированным__ типом для типа @c@.
  --
  type Index c

  -- | Достаёт из структуры @c a@ значение по индексу @Index c@.
  --   Если индекс находится за границами структуры, то возвращает Nothing.
  --
  --   Смотрите, как круто! Запись 'Index c' подразумевает, что вторым аргументом
  --   в @get@ был передан тип, соответствующий индексу @c@.
  --   То есть при задании класса типов у нас есть возможность абстрагироваться от того,
  --   что это за конкретный индекс: 'Int', (Int, Int) или что-то другое...
  --
  get :: c a -> Index c -> Maybe a

  -- | То же самое, что и @get@. Но если индекс находится за границами,
  --   то падает с ошибкой.
  --
  getUnsafe :: c a -> Index c -> a
  getUnsafe cls c = getUnsafeHelper (get cls c)
    where
      getUnsafeHelper :: Maybe a -> a
      getUnsafeHelper (Just a) = a
      getUnsafeHelper Nothing = error "Out of boundary"

  getIthUnsafe :: c a -> Int -> a
  getIthUnsafe cls i = getUnsafe cls $ (indices cls) !! i
  -- | Заменяет в структуре @c a@ значение по индексу @Index c@ на @a@.
  --   Возвращает изменённую структуру.
  --   Если индекс находится за границами структуры, то возвращает Nothing.
  --
  --   ВАЖНО: функции, которые каким-либо образом меняют объекты в Хаскеле,
  --          возвращают __новые__ версии этих объектов, содержащие изменения.
  --          При этом те объекты, которые мы меняли изначально, никак не
  --          поменяются, потому что у нас в языке всё неизменяемое.
  --
  update :: c a -> Index c -> a -> Maybe (c a)

  -- | То же самое, что и @update@. Но если индекс находится за границами,
  --   то падает с ошибкой.
  --
  updateUnsafe :: c a -> Index c -> a -> c a
  updateUnsafe cls c a = updateUnsafeHelper (update cls c a)
    where
      updateUnsafeHelper :: Maybe a -> a
      updateUnsafeHelper (Just a) = a
      updateUnsafeHelper Nothing = error "Out of boundary"

  -- | Возвращает список всех индексов в структуре @c a@.
  --   Порядок — от первого индекса до последнего.
  --
  indices :: c a -> [Index c]

-- 1. Добавьте в класс типов 'Indexable' "реализации по умолчанию" (см. лекцию)
--    для функций @getUnsafe@ и @updateUnsafe@. (0.1 б)

-- 2. Сделайте 'List' (который []), 'Array' и 'Matrix' представителями
--    класса типов 'Indexable'. (0.5 б)

-- | Пример того, как задать ассоциированный тип 'Index'
--   для конкретного типа 'List'.
--

instance Indexable [] where
    type Index [] = Int

    get = getHelper 0
      where
        getHelper acc [] _ = Nothing
        getHelper acc (x:xs) ind = if acc == ind then Just x else getHelper (acc + 1) xs ind

    update [] _ _ = Nothing
    update x n newVal = Just (updateHelper x n newVal)
      where
        updateHelper (x:xs) n newVal
          | n == 0 = newVal:xs
          | otherwise = x:updateHelper xs (n - 1) newVal

    indices xs = [0 .. length xs - 1]

-- | Пример того, как задать ассоциированный тип 'Index'
--   для конкретного типа 'Array ind'.
--

instance Ix ind => Indexable (Array ind) where
  type Index (Array ind) = ind

  get arr ind = if Ix.inRange (A.bounds arr) ind then Just (arr A.! ind) else Nothing
  update arr ind newVal = if Ix.inRange (A.bounds arr) ind then Just (arr A.// [(ind, newVal)]) else Nothing
  indices = A.indices

-- 3. Определите новый тип 'Seq a', который является обёрткой над 'Array Int a'.
--    Сделайте его представителем класса типов 'Indexable'. (0.25 б)

newtype Seq a = Seq (Array Int a) deriving (Show)

instance Indexable Seq where
  type Index Seq = Int

  get (Seq arr) ind = if Ix.inRange (A.bounds arr) ind then Just (arr A.! ind) else Nothing
  update (Seq arr) ind newVal = if Ix.inRange (A.bounds arr) ind then Just (Seq $ arr A.// [(ind, newVal)]) else Nothing
  indices (Seq arr) = A.indices arr

-- 4. Перепешите функцию @align@ с практики так, чтобы она выравнивала
--    друг на друга две любые 'Indexable'-структуры данных. (0.25 б)

align' :: Indexable i => Scorable a => i a -> i a -> Float
align' seq1 seq2 = getAlignmentScore $ fillAlignmentMatrix seq1 seq2

getAlignmentScore :: MatrixU Float -> Float
getAlignmentScore (MatrixU scoreMatrix) = scoreMatrix A.! (snd $ A.bounds scoreMatrix)

fillAlignmentMatrix :: Indexable i => Scorable a => i a -> i a -> MatrixU Float
fillAlignmentMatrix seq1 seq2 = scoreMatrix
  where
    lenSeq1' = length $ indices seq1
    lenSeq2' = length $ indices seq2
    lenSeq1 = lenSeq1' + 1
    lenSeq2 = lenSeq2' + 1
    initScoreMatrix = createMatrix lenSeq1 lenSeq2 0
    matIndices = A.indices (mat initScoreMatrix)
    scoreMatrix = recursiveUpdate initScoreMatrix matIndices
    recursiveUpdate :: MatrixU Float -> [(Int, Int)] -> MatrixU Float
    recursiveUpdate acc [] = acc
    recursiveUpdate acc (x : xs) = recursiveUpdate updatedAcc xs
      where
        updatedAcc :: MatrixU Float
        updatedAcc = scoreForIndex seq1 seq2 acc x (indices seq1) (indices seq2)

scoreForIndex ::
  Scorable a =>
  Indexable i =>
  i a ->
  i a ->
  MatrixU Float ->
  (Int, Int) ->
  [Index i] ->
  [Index i] ->
  MatrixU Float
scoreForIndex seq1 seq2 scoreMat@(MatrixU m) ind@(i, j) indx1 indx2
  | i == 0 && j == 0 = updateIndexM scoreMat ind 0
  | i == 0 = updateIndexM scoreMat ind (fromIntegral j * gapCost)
  | j == 0 = updateIndexM scoreMat ind (fromIntegral i * gapCost)
  | otherwise = updateIndexM scoreMat ind (maximum [insCost, delCost, subCost])
  where
    insCost = m A.! (i - 1, j) + gapCost
    delCost = m A.! (i, j - 1) + gapCost
    subCost = m A.! (i - 1, j - 1) + score (getUnsafe seq1 (indx1 !! (i - 1))) (getUnsafe seq2 (indx2 !! (j - 1)))
    gapCost :: Float
    gapCost = -1

------------------------------------------------------------------------------------

-- Ура! Теперь мы можем выравнивать [DNA] на [DNA], Seq RNA на Seq RNA и так далее!
-- И всё это без переписывания функции @align@: она по умолчанию работает для всех
-- 'Indexable'-структур данных, наполненных значениями 'Scorable'-типа.
--
-- Мы можем даже выравнивать Matrix DNA на Matrix DNA, но зачем...

------------------------------------------------------------------------------------

-- 5. Перепишите функцию @align@ так, чтобы она выдавала не только скор выравнивания,
--    но и сам результат выравнивания. (2 б)

align :: (Scorable a, Indexable c) => c a -> c a -> (Float, ([Maybe a], [Maybe a]))
align seq1 seq2 = (alignmentScore, alignment)
  where
    alignmentMatrixU@(MatrixU m) = fillAlignmentMatrix seq1 seq2
    alignmentScore = getAlignmentScore alignmentMatrixU

    alignment = traceBack (snd $ A.bounds m) ([], [])

    traceBack (0, 0) res = res
    traceBack (i, 0) (s1, s2) = traceBack (i - 1, 0) ((Just (getIthUnsafe seq1 (i - 1)) : s1), (Nothing : s2))
    traceBack (0, j) (s1, s2) = traceBack (0, j - 1) ((Nothing : s1), (Just (getIthUnsafe seq2 (j - 1)) : s2))
    traceBack (i, j) (s1, s2) = traceBack (i - di, j - dj) ((stepI : s1), (stepJ : s2))
      where
        (di, dj) = snd $ maximum [(m A.! (i - di, j - dj), (di, dj)) | (di, dj) <- [(1, 0), (0, 1), (1, 1)]]
        [stepI, stepJ] = [if d == 0 then Nothing else Just (seq `getIthUnsafe` (k - 1))
                         | (d, seq, k) <- [(di, seq1, i), (dj, seq2, j)]]

-- 6.1. Задайте класс типов 'SymbolReadable', в котором есть функция превращения
--      буквы в какой-то объект.
--      Определите инстансы 'SymbolReadable' для 'DNA' и 'RNA'. (0.1 б)

class SymbolReadable a where
  toObj :: Char -> a

data DNA = A | T | G | C
  deriving (Eq, Show)

data RNA = A' | U' | G' | C'
  deriving (Eq, Show)

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

instance Scorable DNA where
    score A A = 5
    score T T = 5
    score G G = 5
    score C C = 5
    score _ _ = -4

-- 6.2 Реализуйте функцию 'toSeq' прнимающую строку, а возвращающую 'Seq' элементов типа,
--     являющегося представителем класса типов 'SymbolReadable'. (0.1 б)

toSeq :: SymbolReadable a => String -> Seq a
toSeq str =
  Seq $ A.listArray (0, length str -1) $ toObj <$> str

dnaSeq_1 :: Seq DNA
dnaSeq_1 = toSeq "ATCGTACGTACG"

dnaSeq_2 :: Seq DNA
dnaSeq_2 = toSeq "ATGCTAGCTGATGCAT"

-- 7. Задайте класс типов 'WithCoords'. У его представителей должны быть определены функции:
--    1. getCoords — получуает по объекту список его координат, т.е. [Seq (Float, Float)]
--    2. setCoords — принимает объект и список координат, а возвращает объект, в котором все соответствующие
--                   координаты заменены на координаты из списка
--    (0.25 б)

class WithCoords a where
  getCoords :: a -> [(Float, Float, Float)]
  setCoords :: a -> [(Float, Float, Float)] -> a

-- 8. Определите тип данных 'Atom'. У него должны быть поля, соотвестствующие его координатам
--    в пространстве, типу химического элемента и названию атома.
--    Сделайте 'Atom' представителем класса типов 'WithCoords'. (0.25 б)

data Atom = Atom {coords :: (Float, Float, Float), chemType :: String, name :: String} deriving (Show)

instance WithCoords Atom where
  getCoords a = [coords a]
  setCoords a [coord] = Atom coord (chemType a) (name a)

-- 9. Определите тип данных 'AminoAcid'. У него должны быть поля, соответствующие
--    набору атомов, входящих в состав аминокислоты, и однобуквенному названию аминокислоты.
--    Сделайте 'AminoAcid' представителями классов типов 'WithCoords' и 'Scorable'. (0.5 б)

class Scorable a where
  score :: a -> a -> Float

data AminoAcid = AminoAcid {atomSet :: [Atom], acidName :: String} deriving (Show)

instance WithCoords AminoAcid where
  getCoords a = map coords (atomSet a)

  setCoords a coords = AminoAcid res (acidName a)
    where
      helper :: WithCoords a => [a] -> [(Float, Float, Float)] -> [a] -> [a]
      helper (a : as) (c : cs) acc = helper as cs (setCoords a [c] : acc)
      helper [] [] acc = acc
      res = reverse $ helper (atomSet a) coords []

instance Scorable AminoAcid where
  score a a' = if acidName a == acidName a' then 5 else -4

------------------------------------------------------------------------------------

-- Обратите внимание! Теперь мы получили возможность выранивать аминокислотные
-- последовательности друг на друга, ничего не меняя в функции @align@.

------------------------------------------------------------------------------------

-- 10. Сделайте так, чтобы (Indexable c => c AminoAcid) был представителем класса типов 'WithCoords'.
--     Явно писать инстанс для 'Indexable c => c AminoAcid' нельзя! (1 б)

instance (Indexable c, WithCoords (Index c)) => WithCoords (c AminoAcid) where
  getCoords c = concatMap getCoords (indices c)
  setCoords c coords =
    foldl (\ac e -> updateUnsafe ac e (setCoords (getUnsafe ac e) coords)) c (indices c)

-- 11. Реализуйте функцию 'rotate', которая принимает матрицу
--     вращения и объект типа, являющегося инстансом 'WithCoords', а возвращает
--     версию исходного объекта, в которой все координаты повёрнуты. (1 б)

type Matrix = ((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))

newCoords :: Matrix -> (Float, Float, Float) -> (Float, Float, Float)
newCoords ((a00, a01, a02), (a10, a11, a12), (a20, a21, a22)) (x, y, z) = (a00 * x + a01 * y + a02 * z, a10 * x + a11 * y + a12 * z, a20 * x + a21 * y + a22 * z)

rotate :: WithCoords a => Matrix -> a -> a
rotate matrix a = setCoords a (map (newCoords matrix) (getCoords a))

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

------------------------------------------------------------------------------------

-- Обратите внимание! Теперь мы получили возможность вращать в пространстве:
-- атомы, аминокислоты, любые последовательности аминокислот.

------------------------------------------------------------------------------------