module BLS12381 where

{-|
Pairing function for Elliptic Curve BLS12-381 is implemented.
|-}


-----------------------------------------------------------------------
---------------------------   Parameters   ----------------------------
-----------------------------------------------------------------------

-- | Field order for BN128
fieldOrder :: Integer
fieldOrder = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab


-- | Group order (for elliptic curve) is given by
-- r_EC = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
-- 'r_sign' gives even/odd sign of group order
r_sign :: Integer  -- (-1)^r_EC
r_sign = -1

-- | Binary representation of r_EC  [b_0, b_1, ..., b_t]  (b_t == 1)
bits_r_EC :: [Integer]
bits_r_EC = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,0,1,1,1,1,0,1,1,1,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,1,0,1,1,0,1,1,1,1,1,0,0,1,1,1]


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

  nrp = Fp2 1 1

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
      t1 = 3 * a0 * a1 * a2 * nrp
      t2 = a1 * a1 * a1 * nrp
      t3 = a2 * a2 * a2 * (Fp2 0 2)
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
           m  = 3 * (x * x) * (inv $ 2 * y)
           x' = m * m - 2 * x
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
g1Gen = EC (Fp1 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb)
           (Fp1 0x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1)


-- | Standard generator point for G2.
g2Gen :: EllipticCurve Fp2
g2Gen = EC (Fp2 0x024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
                0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
           (Fp2 0x0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
                0x0606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)


-- BN128 curve must satisfy E1: y^2 = x^3 + 4 and E2: y^2 = x^3 + 4(u+1)
isOnCurve :: Field a => EllipticCurve a -> Bool
isOnCurve Infty    = True
isOnCurve (EC x y) = y^2 == x^3 + 4 * nrp


-----------------------------------------------------------------------
-----------------------------   Pairing   -----------------------------
-----------------------------------------------------------------------

-- | Untwist of point on E2 for pairing calculation
untwist :: EllipticCurve Fp2 -> EllipticCurve Fp12
untwist Infty      = Infty
untwist (EC x2 y2) = EC x12 y12
  where
    cubicRoot = Fp6 0 1 0
    x12 = (Fp12 (Fp6 x2 0 0) 0) * inv (Fp12 cubicRoot 0)
    y12 = (Fp12 (Fp6 y2 0 0) 0) * inv (Fp12 0 cubicRoot)


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
  let m   = (3 * x * x) * (inv (2 * y))
      f1' = f1 * f1 * (yQ - y - m * (xQ - x))
      f2' = f2 * f2 * (xQ + 2 * x - m * m)
      x'  = m * m - 2 * x
      y'  = negate y - m * (x' - x)
  in  if b == 0 || x' - xP == 0
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
