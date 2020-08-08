# Phantom Types/幽霊型
値に出てこない型引数がある事．  
```haskell
data DeserializeData a = IntData Int | FloatData Float
```
という`DeserializeData`型があったとして，`a`が幽霊になる．  
  
ちなみに，それぞれのコンストラクタの型は，
```haskell
IntData :: Int -> DeserializeData a
FloatData :: Int -> DeserializeData a
```
となる．

## 使い方
```haskell
getInt :: DeserializeData a -> Int
getInt (IntData i) = i

main = getInt (FloatData 3.0)
```
これは実行時エラーになる．これではまずいので，
```haskell
getInt :: DeserializeData Int -> Int
getInt (IntData i) = i

deserializeInt :: ... -> DeserializeData Int
deserializeInt =
    ... -- 省略
    IntData hoge

deserializeFloat :: ... -> DeserializeData Float
deserializeFloat =
    ... -- 省略
    FloatData hoge

main =
    let f = deserializeFloat
    getInt f
```
としてあげることで，コンパイルエラーに持って行ける．  

### GADTsの利用
しかし，悪意を持って，次のように関数`deserializeFloat`を作成することができてしまう．
（型が`DeserializeData Int`として定義できてしまう．）
```haskell
deserializeFloat :: ... -> DeserializeData Int
deserializeFloat =
    ... -- 省略
    FloatData hoge
```
これには[GADTs](../lang-ext/GADTs.md)で対処する．  
ライブラリ作者は，`FloatData`コンストラクタで作った`DeserializeData`について，型引数`a`がFloat以外になって欲しくないはずである．  
ということは，`FloatData`コンストラクタが`DeserializeData Float`を返すようにGADTsで指定すればよい．
```haskell
{-# LANGUAGE GADTs #-}
data DeserializeData a where
    IntData :: Int -> DeserializeData Int
    FloatData :: Float -> DeserializeData Float
```