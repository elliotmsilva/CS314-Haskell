module HW2 where


--poly is a wrapper for a list of elements of type a
newtype Poly a = P [a] deriving (Show, Eq)

--unwraps a poly to its coefficient list
unP (P a) = a

type Polynomial = Poly Double

-- represent a polynomial as a list of coefficients
-- P [1,2,3] represents \x -> 1 + 2*x + 3*x^2
--
-- for uniqueness, we require the last element of the list be non-zero

validPoly :: (Num a, Eq a) => Poly a -> Bool
validPoly (P []) = True
validPoly (P cs) = last cs /= 0


-- Example. negating a polynomial means negating all the coefficients
negatePoly :: Num a => Poly a -> Poly a
negatePoly (P cs) = P (map negate cs)

-- to resolve, replace the definition above with
--    negatePoly (P cs) = P (map negate cs)
-- or
--    negatePoly = P . map negate . unP


{------------------------------------------------------------------------------------}


-- 1. Multiply a polynomial by a scalar. 
-- 
-- for all polynomials p, scalars s, and x, 
--     s * applyPoly p x == applyPoly (scale s p) x
--
-- n.b., scale 0 p == P []

scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale 0 _             = (P [])
scale scalar (P poly) = P ( map (*scalar) poly )

--map :: (a -> b) -> [a] -> [b]
--multiplies the scalar to each element in the poly list


{------------------------------------------------------------------------------------}


-- 2. Compute polynomial's value at a point.
-- Equivalently, convert a polynomial to the function it represents.

applyPoly :: (Num a, Eq a) => Poly a -> a -> a
applyPoly (P []) _   = 0
applyPoly (P poly) x = sum ( zipWith (*) poly (map (\n -> x^n) [0..]) )
--                                 a-b-c  [a]         [b]


--zipWith :: (a->b->c) mutiply -> [a] elements of poly -> [b] with exponents (map powers of x) -> [c]

--sums the list made with zipwith
--zips the poly list by multiplying coefficients of poly with increasing powers of x starting from 0
--maps x to the power of 0, 1, 2... so on


{------------------------------------------------------------------------------------}


-- 3. Compute the derivative of a polynomial.
--
-- n.b., deriv(c * x^n) = c * n * x^(n-1) when n > 0, and 0 otherwise
--
-- ghci> deriv (P [0,1,1,1])
-- (P [1,2,3])
-- ghci> deriv (P [10,20])
-- P [20]

deriv :: (Num a, Eq a) => Poly a -> Poly a
deriv (P [])     = (P [])
deriv (P (p:ps)) = P ( zipWith (*) ps (map fromIntegral [1..]) )
--                         a->b->c [a]        [b]

--derivative will always remove the head, because the derivative of a constant is 0
--zip the tail of the poly list an increasing list representing the powers of x
--in order to compute c*n*x^(n-1)

--map fromIntegral to convert [1..] to same type as coefficients since just doing
--zipwith ... [1..] didnt work bc of typematching


{------------------------------------------------------------------------------------}


-- 4. Add two polynomials
--
-- n.b., addPoly p (negatePoly p) == P []

addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly (P []) (P [])   = (P [])
addPoly (P poly) (P []) = (P poly)
addPoly (P []) (P poly) = (P poly)

addPoly (P (a:as)) (P (b:bs)) = 
   let result = unP (addPoly (P as) (P bs))
       summed = (a+b) : result
   in P (dropTrailingZeros summed)
   where
      dropTrailingZeros [] = []
      dropTrailingZeros cs
         | last cs == 0 = dropTrailingZeros (init cs)
         | otherwise    = cs
--add the heads of both polys, then recursively add the tails
--need to unwrap them first in order to cons the elements tgt, 
--then wrap the entire result as a poly

{------------------------------------------------------------------------------------}


-- 5. Multiply two polynomials
--
-- ghci> multPoly (P [1,1]) (P [-1,1])
-- (1+x) * (-1+x) = -1 + 0x + 1x^2
-- P [-1,0,1]

-- ghci> multPoly (P [0,1]) (P [1,2,3,4])
-- x * (1 + 2x + 3x^2 + 4x^3) = 0 + 1x + 2x^2 + 3x^3 + 4x^4
-- P [0,1,2,3,4]  

multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly (P []) _         = (P [])
multPoly _ (P [])         = (P [])
multPoly (P (p:ps)) (P q) = addPoly (scale p (P q)) (shiftOver (multPoly (P ps) (P q)))
   where 
      shiftOver :: (Num a) => Poly a -> Poly a
      shiftOver (P poly) = P (0 : poly)
      --prepend polynomial with a 0 to multiply everything by x

--scale the second poly by the head of the first poly, then recursively mult the tail of the first
--poly with the second poly, then shift the result over by one (multiply by x) and add it to the first result
--this keeps the powers of x consistent during multiplication

{------------------------------------------------------------------------------------}


-- We can use x and the instance defined below to create expressions
-- more convienently;
--
-- ghci> (x + 1) * (x - 1)
-- P [-1,0,1]
-- ghci> deriv (x^2 - x)
-- P [-1,2]

x :: (Num a) => Poly a
x = P [0,1]

instance (Eq a, Num a) => Num (Poly a) where
   (+) = addPoly
   (*) = multPoly
   negate = negatePoly
   
   fromInteger 0 = P []
   fromInteger n = P [fromInteger n]
   
   abs = error "abs undefined for Poly"
   signum = error "signum undefined for Poly"
