module CounterMatch.Parity where

data Parity = BOTTOM| EVEN| ODD| TOP deriving (Eq, Show)

instance Ord Parity where
    BOTTOM <= _ = True
    _ <= TOP = True
    _ <= _ = False




join:: Parity->Parity->Parity
join a b
    | a == BOTTOM = b
    | b == BOTTOM = a
    | a == b = a
    | otherwise = TOP

is_zero:: Parity->Parity
is_zero BOTTOM = BOTTOM
is_zero ODD = BOTTOM
is_zero _ = EVEN


not_zero:: Parity->Parity
not_zero a = id a

incr:: Parity->Parity
incr EVEN = ODD
incr ODD = EVEN
incr a = a


decr = incr