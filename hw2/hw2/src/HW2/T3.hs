module HW2.T3
  ( epart
  , mcat
  ) where

fromMaybe :: Monoid a => Maybe a -> a -> a
fromMaybe Nothing a  = a
fromMaybe (Just x) a = a <> x

mcat :: Monoid a => [Maybe a] -> a
mcat list = fromMaybe (mconcat list) mempty

fromRight :: Monoid b => Either a b -> b
fromRight (Left _)  = mempty
fromRight (Right r) = r

fromLeft :: Monoid a => Either a b -> a
fromLeft (Right _) = mempty
fromLeft (Left l)  = l

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart list = (foldMap fromLeft list, foldMap fromRight list)
