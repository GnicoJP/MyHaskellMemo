```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
```
これを読む前に: [型族](../lang-ext/TypeFamilies.md), [カインド](../Kind.md)
# 関連データ型/Associated Data Type
次のような型クラスを定義しよう．
```haskell
class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v
```
重要なのは，カインド`* -> *`を持つ`GMap`型が`GMapKey`型クラスの`k`と関連していることである．  
その`GMap`型の定義を貰って各関数が定義されている．  
`Int`でのインスタンス化を見てみよう．
```haskell
instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
  empty                  = GMapInt Data.IntMap.empty
  lookup k   (GMapInt m) = Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)
```
追加で`v`を受け取るようにして（カインドがそうなので），コンストラクタが定義されている．  
（ちなみに，現在の実装ではそのコンストラクタにGADTsを使うことができない．）  
そして，タプルとEitherでの実装を見てみよう．  
```haskell
instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v            = GMapPair (GMap a (GMap b v))
  empty		                = GMapPair empty
  lookup (a, b) (GMapPair gm)   = lookup a gm >>= lookup b 
  insert (a, b) v (GMapPair gm) = GMapPair $ case lookup a gm of
				    Nothing  -> insert a (insert b v empty) gm
				    Just gm2 -> insert a (insert b v gm2  ) gm

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v                = GMapEither (GMap a v) (GMap b v)
  empty                                   = GMapEither empty empty
  lookup (Left  a) (GMapEither gm1  _gm2) = lookup a gm1
  lookup (Right b) (GMapEither _gm1 gm2 ) = lookup b gm2
  insert (Left  a) v (GMapEither gm1 gm2) = GMapEither (insert a v gm1) gm2
  insert (Right b) v (GMapEither gm1 gm2) = GMapEither gm1 (insert b v gm2)
```
普通に型クラスを実装するのに対して美しく実装することができる．  
関連型を使わなかった場合，`data (GMapKey k) => GMap k` を用意して， `GMap`に`empty`，`lookup`, `insert`関数を用意して，どんな`k`でもうまく行くように`k`の関数定義・実装とすり合わせなければならないだろう．
## 参考
[GHC/Type families](https://wiki.haskell.org/GHC/Type_families)  
サンプルコードはすべて上のURLのサンプルコードを参考にしている．