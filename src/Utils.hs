module Utils where

----- Utility FUNCTIONS -----

-- | To binary representation [b_0, b_1, ..., b_(t-1)] of 'n', with b_t == 1
binaryDigits :: Integer -> [Integer]
binaryDigits 0 = [0]
binaryDigits n = reverse $ go n []
  where
    go 0 bits = bits
    go m bits = let (q, r) = divMod m 2
                in  go q (r : bits)

-- | Gives (-1)^n
signEvenOdd :: Integer -> Integer
signEvenOdd n = 1 - 2 * (mod n 2)

-- | From binary representation '[b0, b1, b2, ..., bt]'
fromBinary :: [Integer] -> Integer
fromBinary bits = fst $ foldl (\(a, d) bit -> (a + d * bit, 2 * d)) (0, 1) bits

-- | Checks proper binary representation of 'r_EC'
checkBinary :: Integer -> [Integer] -> Bool
checkBinary r bits = last bits == 1 && r == fromBinary bits

