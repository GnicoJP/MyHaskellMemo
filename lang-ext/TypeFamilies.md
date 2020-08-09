```haskell
{-# LANGUAGE TypeFamilies #-}
```

# Type Families/型族
型を特定の型引数にオーバーロードするものである．  
## data family
書き方は次のとおりである．
```haskell
data family XList a
```
これは，型引数`a`を取る型族`XList`を定義している．  
`data family`の`family`は省略可能である．  
インスタンスを定義してみる．以下のインスタンスは，良く見慣れたリストの形である．  
型引数`a`を`Char`型にしている．
```haskell
data instance XList Char = XCons !Char !(XList Char) | XNil
```
'!'がついているのは，正格評価するためである．（Bang Pattern）  
すると，いつものように普通にXListを利用することができる．  
```haskell
XCons 'a' XNil
XCons '1' (XCons '2' XNil)
```
`Char`しか定義していないから当たり前ながら，`Int`など他の値を`XCons`に渡すことはできない．
```haskell
XCons 1 (XCons 2 XNil) -- エラー
```
別に`Int`型向けに定義することができる．（別の名前で）
```haskell
data instance XList Int = XNext !Int !(XList Int) | XLast
```
パターンマッチは異なる型引数のものを一緒くたにすることはできない．
```haskell
errorFunc :: XList a -> ...
errorFunc XLast = ...
errorFunc XNil = ...
errorFunc (XNext v n) = ...
```
上のパターンは`XList Int`，真ん中のパターンは`XList Char`を受け取るからである．  

そして，この例は，型族を使わない場合次のように書ける．
```haskell
data CharList = XCons !Char !(CharList) | XNil
data IntList = XNext !Int !(IntList) | XLast
```
パターンマッチがエラーになる理由がおわかりいただけるだろう．

### カインド
カインドを指定することもできる．
```haskell
{-# LANGUAGE DataKinds #-}
data family Map k :: * -> *
```
`Map`は連想配列である．型引数`k`はキーである．  
そして，カインドを見ることによってもう一つ型引数を取る型であることが分かる．

以下は型族でできる指定とできない指定である．
```haskell
{-# LANGUAGE DataKinds #-}
class C a b c where { data T c a :: * }  -- OK
class C a b c where { data T a a :: * }  -- Bad: 型引数が繰り返されるのは意味がない．
class D a where { data T a x :: * }      -- Bad: xは型クラスの型引数にはない．以下のように書く．
class D a where { data T a :: * -> * }   -- OK
```

## type family
そして，型族で型を返す関数のように使うこともできる．  
挿入したり検索したりすることができるコレクションを表す型クラス`Collects`を定義する．
```haskell
class Collects ce where
  type Elem ce
  empty  :: ce
  insert :: Elem ce -> ce -> ce
  member :: Elem ce -> ce -> Bool
  toList :: ce -> [Elem ce]
```
2行目はtype familyである．data familyと同様に`family`は省略可能である．  
実装を見てみよう．  
```haskell
instance Eq e => Collects [e] where
  type Elem [e]   = e
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l
```
これは，配列に対する実装である．  
2行目を見る．ここで，`Elem`を関数として見てみると分かりやすい．配列型`[e]`を受け取り，`e`を返している．  
例えば，`toList [32, 24, 43]`のように使った時に，Elemには`[Int]`が渡されて，`Elem`は最終的に`Int`型となる．

このように，type familyは型を返す関数として見ることができる．  
これを用いると型の演算ができる．
```haskell
type family And (a :: Bool) (b :: Bool) :: Bool where
  And False c     = False
  And True  d     = d
  And e     False = False
  And f     True  = f
  And g     g     = g
```
型族`And`があった時に次の型定義の結果はコメントに示すものになる．
```haskell
And True Int
-- Int
And Int False 
-- False
And Int Int
-- Int
And Int Float
-- コンパイルエラー
```
普通に関数に組み込んで使うことができる．
```haskell
someFunc :: forall a b. a -> b -> (And a b)
someFunc left right = ...
```

## 使い方
### 関連データ型
[関連データ型](../misc/AssociatedDataType.md)へ．

## 参考
[GHC/Type families](https://wiki.haskell.org/GHC/Type_families)  
サンプルコードはすべて上のURLのサンプルコードを参考にしている．