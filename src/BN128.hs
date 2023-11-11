module BN128 where

{-|
Pairing function for Elliptic Curve BN128 is implemented.  Specifications for curve are
taken from https://github.com/ethereum/EIPs/blob/master/EIPS/eip-197.md .
|-}


-----------------------------------------------------------------------
---------------------------   Parameters   ----------------------------
-----------------------------------------------------------------------

-- | Field order for BN128
fieldOrder :: Integer
fieldOrder = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47


-- | Group order (for elliptic curve) is given by
-- r_EC = 21888242871839275222246405745257275088548364400416034343698204186575808495617
-- 'r_sign' gives even/odd sign of group order
r_sign :: Integer  -- (-1)^r_EC
r_sign = -1

-- | Binary representation of r_EC  [b_0, b_1, ..., b_t]  (b_t == 1)
bits_r_EC :: [Integer]
bits_r_EC = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,1,0,1,0,0,1,1,1,0,1,1,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,1,1,1,1,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,0,1,0,0,0,0,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,0,0,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1]


-----------------------------------------------------------------------
---------------------   Tower of Field Extensions   -------------------
-----------------------------------------------------------------------

newtype Fp1 = Fp1 {t0 :: Integer} deriving (Eq, Show)
data Fp2    = Fp2 {u0 :: Fp1, u1 :: Fp1} deriving (Eq, Show)
data Fp6    = Fp6 {v0 :: Fp2, v1 :: Fp2, v2 :: Fp2} deriving (Eq, Show)
data Fp12   = Fp12 {w0 :: Fp6, w1 :: Fp6} deriving (Eq, Show)

-- | Field extensions belong to class 'Field'
class (Num a, Eq a) => Field a where
  nrp :: a       -- Non-reducible polynomial
  inv :: a -> a  -- Multiplicative inverse


-- Fp1 is the'standard' field of integers modulo p
instance Num Fp1 where

  (+) (Fp1 a0) (Fp1 b0) = Fp1 $ (a0 + b0) `mod` fieldOrder

  (-) (Fp1 a0) (Fp1 b0) = Fp1 $ (a0 - b0) `mod` fieldOrder

  (*) (Fp1 a0) (Fp1 b0) = Fp1 $ (a0 * b0) `mod` fieldOrder

  fromInteger a0 = Fp1 $ a0 `mod` fieldOrder

  abs = undefined

  signum = undefined


instance Field Fp1 where

  nrp = Fp1 1

  inv (Fp1 n) = if mod n fieldOrder == 0
    then error "tried to divide by zero"
    else fromInteger $ mod t fieldOrder
         where
           (_, _, t) = euclides fieldOrder n


-- | Extended Euclides' algorithm
euclides :: Integer -> Integer -> (Integer, Integer, Integer)
euclides x y = if r < 0 then (negate r, negate s, negate t) else (r, s, t)
  where
    (r, s, t) = go (x, 1, 0) (y, 0, 1)
    go (r0, s0, t0) (r1, s1, t1)
      | r1 == 0   = (r0, s0, t0)
      | otherwise = let
             (q, r2) = divMod r0 r1
             s2      = s0 - q * s1
             t2      = t0 - q * t1
          in
             go (r1, s1, t1) (r2, s2, t2)


-- Fp2 = Fp1[u] / (u^2 + 1)
instance Num Fp2 where

  (+) (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 (a0 + b0) (a1 + b1)

  (-) (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 (a0 - b0) (a1 - b1)

  (*) (Fp2 a0 a1) (Fp2 b0 b1) = Fp2 (a0 * b0 - a1 * b1) (a1 * b0 + a0 * b1)

  fromInteger a0 = Fp2 (fromInteger a0) 0

  abs = undefined

  signum = undefined


instance Field Fp2 where

  nrp = Fp2 9 1

  inv (Fp2 a0 a1) = Fp2 (a0 * factor) (-a1 * factor)
    where
      factor = inv (a1 * a1 + a0 * a0)


-- Fp6 = Fp2[v] / (v^3 - ξ) where ξ = u + 9
instance Num Fp6 where

  (+) (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = Fp6 (a0 + b0) (a1 + b1) (a2 + b2)

  (-) (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = Fp6 (a0 - b0) (a1 - b1) (a2 - b2)

  (*) (Fp6 a0 a1 a2) (Fp6 b0 b1 b2) = Fp6 c0 c1 c2
    where
      c0 = a0 * b0 + (a2 * b1 + a1 * b2) * nrp
      c1 = a1 * b0 + a0 * b1 + a2 * b2 * nrp
      c2 = a2 * b0 + a1 * b1 + a0 * b2

  fromInteger a0 = Fp6 (fromInteger a0) 0 0

  abs = undefined

  signum = undefined


instance Field Fp6 where

  nrp = Fp6 0 1 0

  inv (Fp6 a0 a1 a2) = Fp6 c0 c1 c2
    where
      t0 = a0 * a0 * a0
      t1 = (fromInteger 3) * a0 * a1 * a2 * nrp
      t2 = a1 * a1 * a1 * nrp
      t3 = a2 * a2 * a2 * (Fp2 80 18)
      factor = inv $ t0 - t1 + t2 + t3
      c0 = (a0 * a0 - a1 * a2 * nrp) * factor
      c1 = (-a0 * a1 + a2 * a2 * nrp) * factor
      c2 = (a1 * a1 - a0 * a2) * factor

-- Fp12 = Fp6[w] / (w^2 - γ) where γ = v
instance Num Fp12 where

  (+) (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 (a0 + b0) (a1 + b1)

  (-) (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 (a0 - b0) (a1 - b1)

  (*) (Fp12 a0 a1) (Fp12 b0 b1) = Fp12 (a0 * b0 + a1 * b1 * nrp) (a1 * b0 + a0 * b1)

  fromInteger a0 = Fp12 (fromInteger a0) 0

  abs = undefined

  signum = undefined


instance Field Fp12 where

  nrp = Fp12 1 0

  inv (Fp12 a0 a1) = Fp12 (a0 * factor) (negate $ a1 * factor)
    where
      factor = inv $ a0 * a0 - a1 * a1 * nrp


-----------------------------------------------------------------------
-------------------------   Elliptic Curves   -------------------------
-----------------------------------------------------------------------

-- | Elliptic curve points (using affine coordinates)
data EllipticCurve a = EC {ax :: a, ay :: a} | Infty
  deriving (Eq, Show)

-- Group multiplication (or "sum") on a elliptic curve
instance Field a => Semigroup (EllipticCurve a) where
  (<>) Infty p = p
  (<>) p Infty = p
  (<>) (EC x1 y1) (EC x2 y2)
    | x1 == x2 && y1 == (negate y2) = Infty
    | x1 == x2 && y1 == y2          = let
           (x, y) = (x1, y1)
           m  = (fromInteger 3) * (x * x) * (inv $ (fromInteger 2) * y)
           x' = m * m - (fromInteger 2) * x
           y' = m * (x - x') - y
       in
           EC x' y'
    | not (x1 == x2)                = let
           m  = (y2 - y1) * inv (x2 - x1)
           x3 = m * m - x1 - x2
           y3 = m * (x1 - x3) - y1
       in
           EC x3 y3


-- | Standard generator point for G1.
g1Gen :: EllipticCurve Fp1
g1Gen = EC (Fp1 1) (Fp1 2)


-- | Standard generator point for G2.
g2Gen :: EllipticCurve Fp2
g2Gen = EC (Fp2 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
                0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2)
           (Fp2 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
                0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b)


-- BN128 curve must satisfy E1: y^2 = x^3 + 3 and E2: y^2 = x^3 + 3/(u+9)
isOnCurve :: Field a => EllipticCurve a -> Bool
isOnCurve Infty    = True
isOnCurve (EC x y) = y^2 == x^3 + (fromInteger 3) * inv nrp


-----------------------------------------------------------------------
-----------------------------   Pairing   -----------------------------
-----------------------------------------------------------------------

-- | Untwist of point on E2 for pairing calculation
untwist :: EllipticCurve Fp2 -> EllipticCurve Fp12
untwist Infty      = Infty
untwist (EC x2 y2) = EC x12 y12
  where
    cubicRoot = Fp6 0 1 0
    x12 = (Fp12 (Fp6 x2 0 0) 0) * (Fp12 cubicRoot 0)
    y12 = (Fp12 (Fp6 y2 0 0) 0) * (Fp12 0 cubicRoot)


-- | Miller's algorithm
miller :: [Integer] -> EllipticCurve Fp12 -> EllipticCurve Fp12 -> Fp12
miller bits p q
  | p == Infty || q == Infty || p == q = fromInteger r_sign
  | otherwise = millerGeneric bits p q

-- | Miller's algorithm: generic case
millerGeneric :: [Integer] -> EllipticCurve Fp12 -> EllipticCurve Fp12 -> Fp12
millerGeneric bits (EC xP yP) (EC xQ yQ) = g1 * (inv g2)
  where
    g1                 = g1' * (xQ - xT)
    (g1', g2, xT, _yT) = foldr comb (one, one, xP, yP) bits'
    one                = fromInteger 1
    comb               = millerComb (EC xP yP) (EC xQ yQ)
    bits'              = init bits
    
-- | Accumulator function for Miller's algorithm
millerComb :: EllipticCurve Fp12 -> EllipticCurve Fp12                            -- point parameters
              -> Integer -> (Fp12, Fp12, Fp12, Fp12) -> (Fp12, Fp12, Fp12, Fp12)  -- accumulator function
millerComb (EC xP yP) (EC xQ yQ) b (f1, f2, x, y) = 
  let m   = ((fromInteger 3) * x * x) * (inv ((fromInteger 2) * y))
      f1' = f1 * f1 * (yQ - y - m * (xQ - x))
      f2' = f2 * f2 * (xQ + (fromInteger 2) * x - m * m)
      x'  = m * m - (fromInteger 2) * x
      y'  = negate y - m * (x' - x)
  in  if b == 0 || x' - xP == fromInteger 0
         then (f1', f2', x', y')
         else if b == 1
                 then let m'   = (y' - yP) * (inv (x' - xP))
                          f1'' = f1' * (yQ - y' - m' * (xQ - x'))
                          f2'' = f2' * (xQ + (xP + x') - m' * m')
                          x''  = m' * m' - x' - xP
                          y''  = negate y' - m' * (x'' - x')
                      in  (f1'', f2'', x'', y'')
                 else error "not binary"


-- | Embed into elliptic curve over field extension
embed :: EllipticCurve Fp1 -> EllipticCurve Fp12
embed Infty = Infty
embed (EC (Fp1 n) (Fp1 m)) = EC (fromInteger n) (fromInteger m)


type G1 = EllipticCurve Fp1  -- elliptic curve over Fp1
type G2 = EllipticCurve Fp2  -- elliptic curve over Fp2


-- | Pairing function
pairing :: G1 -> G2 -> Fp12
pairing Infty _ = 1
pairing _ Infty = 1
pairing p1 q2   = (fromInteger r_sign) * f_P_Q * (inv f_Q_P)
  where
    f_P_Q   = miller' (embed p1) (untwist q2)
    f_Q_P   = miller' (untwist q2) (embed p1)
    miller' = miller bits_r_EC


-----------------------------------------------------------------------
-------------------------   Exponentiation   --------------------------
-----------------------------------------------------------------------

-- | Binary representation of a non-negative integer. Note that 'bits n' never
-- has a '0' head.
bits :: Integer -> [Integer]
bits n = go n []
  where
    go m bits
      | m == 0    = bits
      | otherwise = let (q, r) = divMod m 2 in
          go q (r : bits)

-- | Effiicient exponentiation on elliptic curve
ecExp :: Field a => EllipticCurve a -> Integer -> EllipticCurve a
ecExp Infty _ = Infty
ecExp p n     = foldr (expComb p) Infty (reverse $ bits n)
  where
    expComb p b q = let q' = q <> q in
      if b == 0 then q' else p <> q'
