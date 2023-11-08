-- Author: Abu Huraira

import CodeWorld

data Line a = Line [a] a [a]
  deriving (Show) -- required to enable printing (for finite lines)


integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- Exercise 1.1
cutLine :: Int -> Line a -> Line a
cutLine n (Line left x right) = Line (take n left) x (take n right)


-- Exercise 1.2
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generateLeft f x) x (generateRight g x)
  where
    generateLeft :: (a -> Maybe a) -> a -> [a]
    generateLeft f x = case f x of
      Just y -> y : generateLeft f y
      Nothing -> []

    generateRight :: (a -> Maybe a) -> a -> [a]
    generateRight g x = case g x of
      Just y -> y : generateRight g y
      Nothing -> []


-- Exercise 1.3
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line left x right) = Line (map f left) (f x) (map f right)


-- Exercise 1.4
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line left1 focus1 right1) (Line left2 focus2 right2) =
    Line (zip left1 left2) (focus1, focus2) (zip right1 right2)

zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line left1 focus1 right1) (Line left2 focus2 right2) =
    Line (zipWith f left1 left2) (f focus1 focus2) (zipWith f right1 right2)


data Cell = Alive | Dead
  deriving (Show, Eq)

-- Exercise 1.5
rule30 :: Line Cell -> Cell
rule30 (Line (left:_) Alive (right:_)) = case (left, right) of
    (Alive, Alive) -> Dead
    (Alive, Dead)  -> Dead
    (Dead, Alive)  -> Dead
    (Dead, Dead)   -> Alive
rule30 (Line [] Alive (right:_)) = case right of
    Alive -> Dead
    Dead  -> Alive
rule30 (Line [] Dead (right:_)) = case right of
    Alive -> Alive
    Dead  -> Dead
rule30 (Line (left:_) Alive []) = case left of
    Alive -> Dead
    Dead  -> Alive
rule30 (Line (left:_) Dead []) = case left of
    Alive -> Alive
    Dead  -> Dead


-- Exercise 1.6
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing -- Can't shift left if there are no elements to the left
shiftLeft (Line (l:ls) focus rs) = Just (Line ls l (focus:rs))

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing -- Can't shift right if there are no elements to the right
shiftRight (Line ls focus (r:rs)) = Just (Line (focus:ls) r rs)


-- Exercise 1.7
lineShifts :: Line a -> Line (Line a)
lineShifts line = Line (generateShifts line)
                     line
                     (generateShifts (reverseLine line))
  where
    generateShifts :: Line a -> [Line a]
    generateShifts l = iterateM shiftRight l

    iterateM :: (a -> Maybe a) -> a -> [a]
    iterateM f x = x : case f x of
      Just y -> iterateM f y
      Nothing -> []

    reverseLine :: Line a -> Line a
    reverseLine (Line left focus right) = Line right focus left
    

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


-- Exercise 1.8
renderLine :: Line Picture -> Picture
renderLine (Line left focus right) = translated (-fromIntegral (length left)) 0 (renderLine' (Line left focus right))
  where
    renderLine' :: Line Picture -> Picture
    renderLine' (Line [] focus []) = focus
    renderLine' (Line [] focus (r:rs)) = pictures [focus, translated 1 0 (renderLine' (Line [] r rs))]
    renderLine' (Line (l:ls) focus []) = pictures [translated (-1) 0 (renderLine' (Line ls l [])), focus]
    renderLine' (Line (l:ls) focus (r:rs)) = pictures [translated (-1) 0 (renderLine' (Line ls l [])), focus, translated 1 0 (renderLine' (Line [] r rs))]


data Space a = Space (Line (Line a)) 
  deriving (Show)


-- TODO: Exercise 1.9

main :: IO ()
main = return ()