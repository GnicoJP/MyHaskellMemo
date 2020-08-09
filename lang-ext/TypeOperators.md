# TypeOperators
型定義に対して中置演算子が使えるようにするものである．  
`Either`を`+`に定義しなおそう．
```haskell
{-# LANGUAGE TypeOperators #-}
type a + b = Either a b
-- または
type (+) a b = Either a b
```
型定義などで使うことができる．
```haskell
someFunc :: forall a b c d. a + b -> (a -> c) -> (b -> d) -> (c + d)
someFunc (Left v) lmap _ = Left $ lmap v
someFunc (Right v) _ rmap = Right $ rmap v
```
