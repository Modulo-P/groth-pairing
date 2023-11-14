{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import BN128 (EllipticCurve, g1Gen,g2Gen, ecExp, fieldOrder, Fp1, Fp2,pairing)
import Test.QuickCheck (Arbitrary(..), oneof, elements, stdArgs, quickCheck, verboseCheck, withMaxSuccess)


instance Arbitrary (EllipticCurve Fp1) where
    arbitrary = do
        --a <- elements [1..fieldOrder]
        a <- elements [1..100000] 
        return $ ecExp g1Gen a

instance Arbitrary (EllipticCurve Fp2) where
    arbitrary = do
        --a <- elements [1..fieldOrder]
        a <- elements [1..100000] 
        return $ ecExp g2Gen a

--pairing (p1 <> p2) q == pairing p1 q * pairing p2 q
--pairing p (q1 <> q2) == pairing p q1 * pairing p q2

bilinearProp :: EllipticCurve Fp1 -> EllipticCurve Fp1 -> EllipticCurve Fp2 -> Bool
bilinearProp p1 p2 q  = (pairing (p1 <> p2) q == pairing p1 q * pairing p2 q) 

bilinearProp2 :: EllipticCurve Fp1 -> EllipticCurve Fp2 -> EllipticCurve Fp2 -> Bool
bilinearProp2 p q1 q2  = pairing p (q1 <> q2) == pairing p q1 * pairing p q2


main :: IO ()
main = do 
    verboseCheck ( withMaxSuccess 200 bilinearProp)
    verboseCheck ( withMaxSuccess 200 bilinearProp2)
