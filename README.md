# [bins][hackage]

Bin continuous values into discrete containers in an interval, useful for
histograms.

[hackage]: https://hackage.haskell.org/package/bins

## Usage

```haskell
-- divide into 10 bins between 5 and 50, logarithmically
ghci> withBinner (logBS @10 5 50) $ \toBin -> do
        print (toBin 1)
        print (toBin 30)
        print (binIx (toBin 30))
        print (toBin 100)
```

```
Bin (-inf .. 5.0)       -- 1 is outside of range
Bin [25.06 .. 31.55)    -- 30 is inside bin enclosed by 25.06 and 31.55
PElem (Finite 7)        -- 30 is in Bin #7 (indexed from 0)
Bin [50 .. -inf)        -- 100 is outside of range
```

