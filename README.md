# groth-pairing

Testing "pairing function" in the context of Elliptic Curves constructed using a tower of field extensions.

Elliptic curves:

- BN128 (file `src/BN128.hs`)
- BLS12-381 (file `src/BLS12381.hs`)

## Pairing function

Let `G1` be the elliptic curve over `Fp1` (the base field) and `G2` the elliptic curve over `Fp2` (the first extension).  Then

```haskell
pairing :: G1 -> G2 -> Fp12
```

where `Fp12` is the extended field according to the curve's embedding degree 'k = 12' ("top floor" in the tower).

## Points on G1 and G2

A point in G1 is constructed with `ecExp g1Gen n`:

```haskell
ghci> :t ecExp g1Gen
ecExp g1Gen :: Integer -> G1
```

Likewise, a point in G2 is constructed with `ecExp g2Gen n`.

Here `n` is an integer between 0 and q, with

- For BN128:
```
q = 21888242871839275222246405745257275088548364400416034343698204186575808495617
```

- For BLS12-381:
```
q = 52435875175126190479447740508185965837690552500527637822603658699938581184513
```

Function `ecExp` is an efficient implementation of the exponential function on an elliptic curve, so that e.g. `ecExp p 5` is effectively equivalent to `p <> p <> p <> p <> p`.

As a check, one can test that a constructed point `p` is on the elliptic curve with `isOnCurve p`,

```haskell
ghci> :t isOnCurve 
isOnCurve :: Field a => EllipticCurve a -> Bool
```

## Bilinearity

It is expected that the pairing satisfies the **bilinearity** property:

```haskell
pairing (p1 <> p2) q == pairing p1 q * pairing p2 q
```

```haskell
pairing p (q1 <> q2) == pairing p q1 * pairing p q2
```

The test of the property can be checked for the BN-128 curve  with the following commands: 

```bash
cabal test bilineal-property-128
```

And the BLS12-381 curve:

```bash
cabal test bilineal-property-381
```

## ZK verification

To check the viability using our pairing implementation in conjunction with an open source ZK tooling like [_snarkjs_](https://github.com/iden3/snarkjs), we test the validation of a proof produced by such a tool; see files in `src/Groth16`.
