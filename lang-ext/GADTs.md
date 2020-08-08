# GADTs, Generalized algebraic data type
普通のコンストラクタの書式では戻り値の型を変更する（型宣言する）ことができない．  
だから，戻り値の型を変更できるようにするのがGADTsである．  
```haskell
data DeserializeData a =
    IntData Int | FloatData Float
{-
IntData :: Int -> DeserializeData a
FloatData :: Float -> DeserializeData a
-}
```
という型`DeserializeData`があった時に，コンストラクタの戻り値は必ず`DeserializeData a`となる．  
これを次のように記述することで型宣言することができる．
```haskell
data DeserializeData a where
    IntData :: Int -> DeserializeData Int
    FloatData :: Float -> DeserializeData Float
{-
IntData :: Int -> DeserializeData Int
FloatData :: Float -> DeserializeData Float
-}

```
利用には，GADTs言語拡張が必要である．
```haskell
{-# LANGUAGE GADTs #-}
```
また，返す幽霊型の型引数は，引数と一致しなくても良い．  

この機能は例えば，[幽霊型](../misc/PhantomType.md)を使うときに有用である．