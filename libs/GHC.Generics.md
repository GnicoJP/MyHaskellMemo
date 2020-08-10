```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generic
```
# GHC.Generics
[Generic Haskell](../misc/GenericHaskell.md)をHaskellで出来るようにするためのライブラリである．
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Generic
```
を定義する．GHCiで`:kind! Rep (Tree *)`とすると，
```haskell
Rep (Tree *) :: * -> *
= D1
    ('MetaData "Tree" "Ghci1" "interactive" 'False)
    (
        (C1
            ('MetaCons "Leaf" 'PrefixI 'False)
            (S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 *))
        )
    :+:
        (C1
           ('MetaCons "Node" 'PrefixI 'False)
           (
                (S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 (Tree *)))
            :*:
                (S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 (Tree *)))
            )
        )
    )
```
`D1`, `C1`, `S1`は次のような定義である．
```haskell
type S1 = M1 S -- レコードセレクタ（束縛される値）を表す
type C1 = M1 C -- コンストラクタを表す
type D1 = M1 D -- データ型を表す
```
`M1`は以下の定義だ．
```haskell
newtype M1  i t f p = M1 { unM1 :: f p }  -- ラッパ，ジェネリックな型の引数の情報を格納する．
-- i : ラッパの種類（S, C, D）を表すデータ型
-- t : メタデータ．（'MetaData, 'MetaCons, 'MetaSel）
-- f : 中身(K1, M1)
-- p : パラメータ
```
`Rec0`は再帰的な物を表す．
```haskell
type Rec0 = K1 R
newtype K1    i c p = K1 { unK1 :: c } -- cのコンテナ
-- i : ラッパの種類（R）を表すデータ型
-- c : 入っているものの情報
-- p : パラメータ
```
他にも以下の型がある．
```haskell
data    V1        p                       -- lifted version of Empty
data    U1        p = U1                  -- lifted version of ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
```

この`Rep`は，型の情報を表すデータ型である．  
`Generic`型クラスの関連型であり，`Generic`型クラスに本当の値と`Rep`のデータとの変換をするコードがある．

## `Generic1`
`Generic1`を`derive`すると，`Rep1`(`Generic1`の関連型)が生成される．  
`Rep`と違い，`Rec0 *`は`Par1`に，`Rec0 (a *)`は`Rec1 (a)`になる．
```haskell
newtype Par1   p = Par1 { unPar1 ::   p } -- gives access to parameter p
newtype Rec1 f p = Rec1 { unRec1 :: f p } -- a wrapper
```
また，`a`が入れ子になった場合（`[a b]`の場合など）は，`([] :.: Rec1 a)`と表現される．
```haskell
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }
```

## Unlifted Types
Unliftedな型は特別な`URec`が提供される．
```haskell
data family URec a p

data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#   }
data instance URec Char     p = UChar   { uChar#   :: Char#   }
data instance URec Double   p = UDouble { uDouble# :: Double# }
data instance URec Int      p = UFloat  { uFloat#  :: Float#  }
data instance URec Float    p = UInt    { uInt#    :: Int#    }
data instance URec Word     p = UWord   { uWord#   :: Word#   }

type UAddr   = URec (Ptr ())
type UChar   = URec Char
type UDouble = URec Double
type UFloat  = URec Float
type UInt    = URec Int
type UWord   = URec Word
```

## 参考
[GHC.Generics](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-Generics.html)