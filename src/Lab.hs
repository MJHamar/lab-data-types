--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Data types                                                            --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

data IntPos = IntPos Int Int

instance Eq IntPos where
    (IntPos x0 y0) == (IntPos x1 y1) = (x0 == x1) && (y0 == y1)

instance Show IntPos where
    show (IntPos x y) = "IntPos " ++ (show x) ++ " " ++ (show y)

zeroPos :: IntPos
zeroPos = IntPos 0 0

x :: IntPos -> Int
x (IntPos x0 y0) = x0

y :: IntPos -> Int
y (IntPos x0 y0) = y0

--------------------------------------------------------------------------------

data Pos a = Pos a a

instance Eq a => Eq (Pos a) where
    (Pos x0 y0) == (Pos x1 y1) = (x0 == x1) && (y0 == y1)

instance Show a => Show (Pos a) where
    show (Pos x y) = "Pos " ++ (show x) ++ " " ++ (show y)

zero :: Num a => Pos a
zero = Pos 0 0

left :: Pos a -> a
left (Pos x y) = x

top :: Pos a -> a
top (Pos x y) = y

--------------------------------------------------------------------------------

data DocumentItem = ListItem (Int -> String)

doc :: [DocumentItem]
doc =
    [ ListItem (\n -> show n ++ ". An item")
    , ListItem (\n -> concat ["I am item #", show n])
    , ListItem (\n -> concat ["There. Are. ", show n, ". Items." ])
    ]

render :: [DocumentItem] -> Int -> String
render     [] _ = ""
render (x:xs) i = ((f x) i) ++ render xs (i+1)
  where
    f (ListItem g) = g

--------------------------------------------------------------------------------
