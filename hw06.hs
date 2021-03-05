-- С помощью таких конструкций можно включать разные расширения языка.
-- Поподробнее с тем, что это такое, мы познакомимся чуть позже.
-- Но, как вы могли догадаться, для выполнения домашки нам понадобились какие-то расширения!
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

import           Data.Array      (Array, Ix)
import qualified Data.Ix as Ix (inRange)
import qualified Data.Array as A (array, bounds, indices, listArray, (!), (//))

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

data Seq a = Array Int a

--instance Indexable Seq where
--  get arr ix
--    | Ix.inRange (A.bounds arr) ix = Just (arr A.! ix)
--    | otherwise = Nothing
--
--  update = undefined
--  indices = undefined

-- 4. Перепешите функцию @align@ с практики так, чтобы она выравнивала
--    друг на друга две любые 'Indexable'-структуры данных. (0.25 б)

------------------------------------------------------------------------------------

-- Ура! Теперь мы можем выравнивать [DNA] на [DNA], Seq RNA на Seq RNA и так далее!
-- И всё это без переписывания функции @align@: она по умолчанию работает для всех
-- 'Indexable'-структур данных, наполненных значениями 'Scorable'-типа.
--
-- Мы можем даже выравнивать Matrix DNA на Matrix DNA, но зачем...

------------------------------------------------------------------------------------

-- 5. Перепишите функцию @align@ так, чтобы она выдавала не только скор выравнивания,
--    но и сам результат выравнивания. (2 б)

-- 6.1. Задайте класс типов 'SymbolReadable', в котором есть функция превращения 
--      буквы в какой-то объект.
--      Определите инстансы 'SymbolReadable' для 'DNA' и 'RNA'. (0.1 б)

-- 6.2 Реализуйте функцию 'toSeq' прнимающую строку, а возвращающую 'Seq' элементов типа,
--     являющегося представителем класса типов 'SymbolReadable'. (0.1 б)

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

data Atom = Atom {coords :: [(Float, Float, Float)], chemType :: String, name :: String}

instance WithCoords Atom where
  getCoords a = coords a
  setCoords a coord = Atom coord (chemType a) (name a)

-- 9. Определите тип данных 'AminoAcid'. У него должны быть поля, соответствующие 
--    набору атомов, входящих в состав аминокислоты, и однобуквенному названию аминокислоты.
--    Сделайте 'AminoAcid' представителями классов типов 'WithCoords' и 'Scorable'. (0.5 б)

class Scorable a where
    score :: a -> a -> Float

data AminoAcid = AminoAcid {atomSet :: [Atom], acidName :: String}

instance WithCoords AminoAcid  where
    getCoords a = getCoordsHelper (map coords (atomSet a))
      where
        getCoordsHelper :: [[a]] -> [a]
        getCoordsHelper [a] = a

    setCoords a cords = AminoAcid (helper cords (atomSet a)) (acidName a)
      where
        helper :: [(Float,Float,Float)] -> [Atom] -> [Atom]
        helper (cord:cs) (atom:as) = (setCoords atom [cord]):helper cs as

instance Scorable AminoAcid where
    score a a' = if acidName a == acidName a' then 5 else -4

------------------------------------------------------------------------------------

-- Обратите внимание! Теперь мы получили возможность выранивать аминокислотные
-- последовательности друг на друга, ничего не меняя в функции @align@.

------------------------------------------------------------------------------------

-- 10. Сделайте так, чтобы (Indexable c => c AminoAcid) был представителем класса типов 'WithCoords'.
--     Явно писать инстанс для 'Indexable c => c AminoAcid' нельзя! (1 б)

-- 11. Реализуйте функцию 'rotate', которая принимает матрицу
--     вращения и объект типа, являющегося инстансом 'WithCoords', а возвращает
--     версию исходного объекта, в которой все координаты повёрнуты. (1 б)

type Matrix = ((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))

newCoords :: Matrix -> (Float, Float, Float) -> (Float, Float, Float)
newCoords ((a00, a01, a02), (a10, a11, a12), (a20, a21, a22)) (x, y, z) = (a00 * x + a01 * y + a02 * z, a10 * x + a11 * y + a12 * z, a20 * x + a21 * y + a22 * z)

rotate :: WithCoords a => Matrix -> a -> a
rotate matrix a = setCoords a (map (newCoords matrix) (getCoords a))

------------------------------------------------------------------------------------

-- Обратите внимание! Теперь мы получили возможность вращать в пространстве:
-- атомы, аминокислоты, любые последовательности аминокислот.

------------------------------------------------------------------------------------