import Numeric

--gz :: [a] -> Int
--digit :: a -> Int
--di :: (a, b) -> Int

--acc :: RealFloat a => [a] -> Int

-- k =  0,5 * cw * L * Dichte

m = 200
k = 2.5

g = 9.81

dt = 0.1 * acc

f v = m * g - k * v ^ 2
a f = f / m
vn a va = a * dt + va
sn sa a va = sa + 0.5 * a * dt ^ 2 + va * dt

newt t = t + dt
newv v s = vn (a (f v)) (v)
news s v = sn (s) (a (f v)) (v)

step t v s
    | (a(f v)) > (0.5 * acc) = step (newt t) (newv v s) (news s v)
--  | v < 2.0                = step (newt t) (newv v s) (news s v)
    | otherwise = (t,v,s)


acc = ex (gz [m,k,g])

gz xs = minimum (map (digit) xs)

digit a = di (floatToDigits 10 (abs a))

di (xs, x) = length xs

ex a = 0.1 ^ a
