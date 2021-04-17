-- В Хаскеле есть тип данных 'Map k v', сопостовляющий каждому ключу 
-- типа @k@ какое-то конкретное значение типа @v@.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (find, toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Float (int2Float)
import Data.Array

-- 1. Реализуйте исключительно с помощью свёрток:

-- | 1.1. Функция, суммирующая квадраты элементов списка (0.1 б)
--
sumOfSquares :: Num a => [a] -> a
sumOfSquares = foldl' (\thunk elem -> thunk + elem ^ 2) 0

-- | 1.2. Функция разворачивания списка (0.25 б)
--
reverse' :: [a] -> [a]
reverse' = foldl' (\xs x -> x:xs) []

-- | 1.3. Функция, которая достаёт из списка элемент по индексу (0.25 б)
--

getByIndex :: [a] -> Int -> Maybe a
getByIndex list index = snd $ foldl' (\a@(curr, ac) e -> if curr == index then (curr + 1, Just e) else (curr + 1, ac)) (0, Nothing) list

-- | Тип данных "Студент"
--
data Student 
  = Student 
      { name  :: String -- имя студента
      , grade :: Int    -- оценка студента по нашему предмету
      }

-- | Тип данных "Информация о студентах курса"
--
data StudentsLog
  = StudentsLog
      { studentNames :: [String]  -- список имён студентов
      , worstGrade   :: Maybe Int -- наименьшапя оценка по курсу
      , bestGrade    :: Maybe Int -- наибольшая оценка по курсу
      } deriving (Show)

-- | 1.4. Функция, которая по списку студентов курса рассчитывает информацию по курсу (0.5 б)
--

calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog = foldl' f (StudentsLog [] Nothing Nothing)
  where
    f StudentsLog {..} Student {..} =
      let wg = maybe grade (minimum grade) worstGrade
          bg = maybe grade (maximum grade) bestGrade
       in StudentsLog (studentNames ++ [name]) (Just wg) (Just bg)
    minimum x y = if x < y then x else y
    maximum x y = if x < y then y else x

-- | 1.5. Функция, которая вставляет в уже отсортированный список элементов
--        новый элемент на такую позицию, что все элементы левее будут меньше или
--        равны нового элемента, а все элементы справа будут строго больше (1 б)
--

insert :: Ord a => [a] -> a -> [a]
insert xs x = foldr aux ini xs False
  where
    aux y f done
      | done || x > y = y : f done
      | otherwise = x : y : f True
    ini True = []
    ini _ = [x]

-- | 1.6. Сортировка вставками. В реализации можно использовать функцию @insert@, 
--        если вы её реализовали (0.5 б)
--

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' insert []

-- | 1.7. Функция zip. В секции where дана подсказка — вид инициального значения (1 б)
--

zip' :: [a] -> [b] -> [(a, b)]
zip' = foldr f ini
  where
    ini :: [b] -> [(a, b)]
    ini _ = []
    f x xs (y : ys) = (x, y) : xs ys
    f _ _ [] = []

-- | 2. Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--      @calculateStudentsLog'@, которая делает то же самое, что и @calculateStudentsLog@.
--      В реализации нужно использовать то, что 'StudentsLog' — моноид. (0.5 б)
--

instance Semigroup StudentsLog where
  s1 <> s2 =
    StudentsLog
      (studentNames s1 <> studentNames s2)
      (worst (worstGrade s1) (worstGrade s2))
      (best (bestGrade s1) (bestGrade s2))

worst :: Maybe Int -> Maybe Int -> Maybe Int
worst (Just x) (Just y) = Just $ min x y
worst Nothing (Just y) = Just y
worst (Just x) Nothing = Just x
worst Nothing Nothing = Nothing

best :: Maybe Int -> Maybe Int -> Maybe Int
best (Just x) (Just y) = Just $ max x y
best Nothing (Just y) = Just y
best (Just x) Nothing = Just x
best Nothing Nothing = Nothing

instance Monoid StudentsLog where
  mempty = StudentsLog [] Nothing Nothing

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' =
  foldl (\acc s -> acc <> toStudentLog s) mempty
  where
    toStudentLog Student {..} = StudentsLog [name] (Just grade) (Just grade)

-- | Хорошо знакомый нам тип данных "Дерево"
--
data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- 3. Сделайте 'Tree' представителем класса типов 'Foldable' (1 б)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f n = foldr (\e a -> mappend (f e) a) mempty n
  foldr f acc Leaf = acc
  foldr f acc (Node x t) = foldr (flip (foldr f)) (f x acc) t

-- | Тип данных "Яблоко"
--
data Apple
  = Apple
      { color  :: String -- цвет яблока
      , weight :: Float  -- вес яблока
      }
  deriving (Eq, Show)

instance Ord Apple where
  compare app1 app2 = compare (weight app1) (weight app2)
  app1 <= app2 = (weight app1) <= (weight app2)

-- | 4. С помощью функйций из 'Data.Foldable' реализуйте следующие функции:

-- | 4.1. Проверка, что все яблоки в дереве имеют вес, который находится
--        в заданном диапазоне весов (0.1 б)
--

applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (ls, rs) = all (\Apple {..} -> weight >= ls && weight <= rs) tree

-- | 4.2. Находит яблоко с наибольшим весом (0.1 б)
--

heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple t = Just . maximum $ t

-- | 4.3 Находит яблоко с цветом из заданного списка цветов и весом,
--       находящимся в заданном диапазоне весов (0.1 б)
--

thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple Leaf _ _ = Nothing
thisApple app colors (l, r) = find (\Apple {..} -> elem color colors && int2Float l <= weight && weight <= int2Float r) app

-- | 4.4 Считает сумму весов всех яблок в дереве.
--       В реализации нужно использовать 'Data.Foldable.sum' (0.25 б)
--

sumOfApples :: Tree Apple -> Float
sumOfApples t = sum $ weight <$> toList t

-- | Корзинка с яблоками.
--   Важно, что яблоки в корзинке расфасованы по цветам.
--   Для каждого цвета яблоки упорядочены по весу
--
newtype Basket = Basket { apples :: Map String [Apple] }
  deriving (Eq, Show)

-- | 5. Реализуйте с помощью свёртки дерева функцию, которая соберёт 
--      по дереву яблок корзинку с яблоками.
--      В 'Data.Map.Strict' вы найдёте функции, которые помогут вам
--      инициализировать и модифицировать мапу (0.5 б)
--

collectBasket :: Tree Apple -> Basket
collectBasket =
  Basket . foldl f mempty
  where
    f acc e = case Map.lookup (color e) acc of
      Just x -> Map.update (Just . flip insert e) (color e) acc
      Nothing -> Map.insert (color e) [e] acc

-- | Двоичная куча (https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча)
--
data BinaryHeap a 
  = BinNode 
      { val   :: a
      , left  :: BinaryHeap a
      , right :: BinaryHeap a
      } 
  | BinLeaf
  deriving (Eq, Show)

-- | 6.1. Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0.5 б)
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf
siftDown node =
    let (BinNode v l r) = node
        in
    case (l, r) of
      (BinNode left_Val left_L left_R, BinNode right_Val right_L right_R) -> if left_Val > right_Val && left_Val > v then BinNode left_Val new_L r else if right_Val > left_Val && right_Val > v then BinNode right_Val l new_R else node
        where
          new_R = siftDown $ BinNode v right_L right_R
          new_L = siftDown $ BinNode v left_L left_R
      (BinLeaf, BinLeaf) -> node
      (BinNode left_Val left_L left_R, BinLeaf) -> if v < left_Val then BinNode left_Val new_L BinLeaf else node
        where
          new_L = siftDown $ BinNode v left_L left_R
      (BinLeaf, BinNode right_Val right_L right_R) -> if v < right_Val then BinNode right_Val BinLeaf new_R else node
        where
          new_R = siftDown $ BinNode v right_L right_R

-- | 6.2. Реализуйте с помощью свёртки (которая уже написана в коде) 
--        функцию buildHeap, которая за __линейное время__ конструирует 
--        на основе спиcка элементов бинарную кучу.
--        Соответствующий алогритм описан в статье на вики (ссылка выше).
--        Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!) (1 б)
buildHeap  :: forall a. Ord a => [a] -> BinaryHeap a
buildHeap l = foldr makeHeap array [1 .. div (length l) 2] ! 1
  where
    array :: Array Int (BinaryHeap a)
    array = listArray (1, (length l)) (map (\x -> BinNode x BinLeaf BinLeaf) l)
    makeHeap :: Int -> Array Int (BinaryHeap a) -> Array Int (BinaryHeap a)
    makeHeap i array = array // remove
      where
        left_i = 2 * i
        right_i = left_i + 1
        cur_vl = val (array ! i)
        left_heap :: BinaryHeap a
        left_heap = array ! left_i
        right_heap :: BinaryHeap a
        right_heap = if right_i > (length l) then BinLeaf else array ! right_i
        new_heap :: BinaryHeap a
        new_heap = siftDown (BinNode cur_vl left_heap right_heap)
        remove :: [(Int, BinaryHeap a)]
        remove = if right_i > (length l) then [(i, new_heap), (left_i, BinLeaf)] else [(i, new_heap), (left_i, BinLeaf), (right_i, BinLeaf)]