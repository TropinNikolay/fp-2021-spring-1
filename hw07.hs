-- В Хаскеле есть тип данных 'Map k v', сопостовляющий каждому ключу 
-- типа @k@ какое-то конкретное значение типа @v@.
import Data.Map.Strict (Map)
import Data.List (foldl')

-- 1. Реализуйте исключительно с помощью свёрток:

-- | 1.1. Функция, суммирующая квадраты элементов списка (0.1 б)
--
sumOfSquares :: Num a => [a] -> a
sumOfSquares = foldr (\thunk elem -> thunk + elem ^ 2) 0

-- | 1.2. Функция разворачивания списка (0.25 б)
--
reverse' :: [a] -> [a]
reverse' = foldl' (\xs x -> x:xs) []

-- | 1.3. Функция, которая достаёт из списка элемент по индексу (0.25 б)
--

--getByIndex :: [a] -> Int -> a
--getByIndex = foldl' ()

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
      }

-- | 1.4. Функция, которая по списку студентов курса рассчитывает информацию по курсу (0.5 б)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog = undefined

-- | 1.5. Функция, которая вставляет в уже отсортированный список элементов
--        новый элемент на такую позицию, что все элементы левее будут меньше или
--        равны нового элемента, а все элементы справа будут строго больше (1 б)
--
insert :: Ord a => [a] -> a -> [a]
insert = undefined

-- | 1.6. Сортировка вставками. В реализации можно использовать функцию @insert@, 
--        если вы её реализовали (0.5 б)
--
insertionSort :: Ord a => [a] -> [a]
insertionSort = undefined

-- | 1.7. Функция zip. В секции where дана подсказка — вид инициального значения (1 б)
--
zip' :: [a] -> [b] -> [(a, b)]
zip' = undefined
  where
    ini :: [b] -> [(a, b)]
    ini _ = []

-- | 2. Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--      @calculateStudentsLog'@, которая делает то же самое, что и @calculateStudentsLog@.
--      В реализации нужно использовать то, что 'StudentsLog' — моноид. (0.5 б)
--
calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = undefined

-- | Хорошо знакомый нам тип данных "Дерево"
--
data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- 3. Сделайте 'Tree' представителем класса типов 'Foldable' (1 б)

-- | Тип данных "Яблоко"
--
data Apple
  = Apple
      { color  :: String -- цвет яблока
      , weight :: Float  -- вес яблока
      }
  deriving (Eq, Show)

-- | 4. С помощью функйций из 'Data.Foldable' реализуйте следующие функции:

-- | 4.1. Проверка, что все яблоки в дереве имеют вес, который находится 
--        в заданном диапазоне весов (0.1 б)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange = undefined

-- | 4.2. Находит яблоко с наибольшим весом (0.1 б)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple = undefined

-- | 4.3 Находит яблоко с цветом из заданного списка цветов и весом,
--       находящимся в заданном диапазоне весов (0.1 б)
--
thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple = undefined


-- | 4.4 Считает сумму весов всех яблок в дереве.
--       В реализации нужно использовать 'Data.Foldable.sum' (0.25 б)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = undefined

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

--collectBasket :: Apple Tree -> Basket
--collectBasket = undefined

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
--      
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown = undefined

-- | 6.2. Реализуйте с помощью свёртки (которая уже написана в коде) 
--        функцию buildHeap, которая за __линейное время__ конструирует 
--        на основе спиcка элементов бинарную кучу.
--        Соответствующий алогритм описан в статье на вики (ссылка выше).
--        Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!) (1 б)
--       
buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap l = foldr undefined undefined l