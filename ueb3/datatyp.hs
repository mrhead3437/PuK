data IntervalMap k v = IntervalMap v [(k,v)] deriving (Show)

singleton :: v ->  IntervalMap k v
singleton v = IntervalMap v []

(!) :: Ord k => IntervalMap k v -> k -> v
(!) (IntervalMap v []) _ = v
(!) (IntervalMap v xs) k | null checker = v
                          | otherwise = snd $ last checker
        where
          checker = takeWhile (\x -> fst x <= k) xs

insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert begin end value (IntervalMap d []) = IntervalMap d [(begin, value), (end, d)]
insert begin end value (IntervalMap d xs) | begin > fst (head xs) = IntervalMap d $ [head xs, (begin, value), (end, d)] ++ xs
                                          | otherwise = IntervalMap d $ [(begin, value), (end, d)] ++ xs

instance Functor (IntervalMap k) where
    fmap func (IntervalMap v k) = IntervalMap (func v) [(a,func val) | (a,val) <- k]
    (<$) val (IntervalMap v k) = IntervalMap val ([(a,val) | (a,_) <- k])

instance Ord k => Applicative (IntervalMap k) where
    pure d = IntervalMap d []
    (<*>) (IntervalMap v vs) (IntervalMap w ws) = IntervalMap (v w)  ([(start,(v w))|(start,value) <- ws])

a = singleton 'a' :: IntervalMap Int Char
b = insert 10 20 'b' a
c = insert 9 21 'c' b
d = insert 5 15 'd' c
e = insert 14 22 'e' d
f = insert 10 19 'f' e
g = fmap fromEnum f
h = "Hello" <$ g

-- läuft nicht
i = insert 5 10 110 $ insert 10 15 90 $ singleton 100 :: IntervalMap Int Int
j = insert 5 10 (-) $ insert 10 15 (*) $ singleton (+) :: IntervalMap Int (Int -> Int -> Int)
k = insert 3 18 2 $ singleton 10 :: IntervalMap Int Int
l = j <*> i <*> k