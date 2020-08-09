# KindSignatures/カインド注釈, 種注釈
カインドを注釈したい場合に使う．
```haskell
data IntBuffer c d = IntBuffer (c Int) d
```
このような型`IntBuffer`があったとすると，カインドは
```
c :: * -> *
d :: *
```
となる．これを注釈するには，
```haskell
data IntBuffer (c :: * -> *) (d :: *) = IntBuffer (c Int) d
```
と記述する．  
利用には，KindSignatures言語拡張が必要である．
```haskell
{-# LANGUAGE KindSignatures #-}
```

## [GADTs](GADTs.md)を使用する場合
以下のように書く．
```haskell
data IntBuffer (a :: * -> *) (b :: *) where
    IntBuffer :: (c Int) -> d -> IntBuffer c d
```