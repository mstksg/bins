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

```haskell
-- Generate a histogram based on the bins from valules in a list
ghci> xs = [1..100] :: [Double]
ghci> withBinner (logBS @10 5 50) $ \toBin -> do
         mapM_ (\(b, n) -> putStrLn (displayBinDouble 4 b ++ "\t" ++ show n))
       . M.toList
       $ binFreq toBin xs
```

```
(-inf .. 5.0000)        4
[5.0000 .. 6.2946)      2
[6.2946 .. 7.9245)      1
[7.9245 .. 9.9763)      2
[9.9763 .. 12.5594)     3
[12.5594 .. 15.8114)    3
[15.8114 .. 19.9054)    4
[19.9054 .. 25.0594)    6
[25.0594 .. 31.5479)    6
[31.5479 .. 39.7164)    8
[39.7164 .. 50.0000)    10
[50.0000 .. +inf)       51
```
