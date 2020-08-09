# PolyKinds/カインド多相,種多相
```haskell
data SomeBuffer v c = SomeBuffer (c v)
```
通常のGHCで定義するとカインドは
```haskell
SomeBuffer :: * -> (* -> *) -> *
```
となる．しかし，vのカインドを`*`に具体化せずに，抽象的に`a`とおけば，
```haskell
SomeBuffer :: a -> (a -> *) -> *
```
としても，大丈夫そうだろう．  
PolyKinds言語拡張を用いると，
```haskell
{-# LANGUAGE PolyKinds #-}
```
`SomeBuffer`は抽象的に定義される．

## [KindSignatures](KindSignatures.md)と併用
特に注意することなく`:kind`に表示されるように書けばよい.
```haskell
data SomeBuffer (v :: a) (c :: a -> *) = SomeBuffer (c v)
```
