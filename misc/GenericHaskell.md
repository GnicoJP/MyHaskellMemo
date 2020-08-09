# Generic Haskell
Generic Programmingの基本的なアイデアは型（あるいはカインド）の帰納的な構造による等価性を調べる関数（言語機能）を用意することである．  
つまり，型構造の多相性を導入することである．  
等価性を調べる関数には3つの引数を用意する．2つは比較する型の構造，1つはその構造にある値を比較する関数である．  
（Haskellの`data`は入れ子にできるので，一般的にその入れ子を比較するために，等価性を調べる関数自身を渡すのだろう．）

---
今回のコードはGHCではコンパイル出来ない．すなわち，Generic Haskellコンパイラ用に書かれたコードである．

## 概念的な話，例：データの圧縮
次の型を用意する．（Haskellでは用意されている）
```haskell
type Bin = [Bit]
data Bit = O | I
```
簡単に`String <-> Bin`な関数を定義しよう．
```haskell
encodeString :: String -> Bin
decodeString :: Bin -> String
```
これは，`String`型にしか適用できない．そこで，`[Bin] <-> Bin`な関数を定義する．
```haskell
encodeBinList :: [a] -> (a -> Bin) -> Bin
decodeBinList :: Bin -> (Bin -> a) -> [a]
```
`a -> Bin`, `Bin -> a`は`a`と`Bin`を相互に変換する関数である．  
`String`は`[Char]`であるから，`Char <-> Bin`を用意する必要がある．  

次にOkasaki's binary random-access listを作ってみよう．
```haskell
data Fork a = Fork a a
encodeFork :: forall a.(a -> Bin) -> (Fork a -> Bin)
encodeFork ena (Fork a1 a2) = ena a1 ++ ena a2

data Sequ a = EndS | ZeroS (Sequ (Fork a)) | OneS a (Sequ (Fork a))
encodeSequ :: forall a.(a -> Bin) -> (Sequ a -> Bin)
encodeSequ ena EndS = 0 : []
encodeSequ ena (ZeroS s) = 1 : 0 : encodeSequ(encodeFork ena) s
encodeSequ ena (OneS a s) = 1 : 1 : ena a ++ encodeSequ (encodeFork ena) s
```
デコーダを作ると，
```haskell
decodesFork :: forall a.(Bin -> (a,Bin)) -> (Bin -> (Fork a,Bin))
decodesFork dea bin = (Fork a1 a2, bin2)
    where
        (a1, bin1) = dea bin
        (a2, bin2) = dea bin1

decodesSequ :: forall a.(Bin -> (a,Bin)) -> (Bin -> (Sequ a,Bin))
decodesSequ dea (0:bin) = (EndS,bin)
decodesSequ dea (1:0:bin) = (ZeroS s, bin1)
    where
        (s,bin1) = decodesSequ (decodesFork dea) bin
decodesSequ dea (1:1:bin) = (OneS a s,bin2)
    where
        (a,bin1) = dea bin
        (s,bin2) = decodesSequ(decodesFork dea) bin1
```
となる．
## kind-indexed types
カインドの帰納的な構造をkind-indexed typeと言おう．  
そして，kind-indexed typesを次のように表現する．（Haskellのコードではなく，Generic Haskellのコードである．）
```haskell
type Encode{[*]} t = t -> Bin
type Encode{[k -> v]} t = forall a. Encode{|k|} a -> Encode{|v|} (t a)
type Decodes{[*]} t = Bin -> t
type Decodes{[k -> v]} t = forall a. Decodes{|k|} a -> Decodes{|v|} (t a)
```
`{[]}`がkind-indexed typeである．  
これまでの関数をこの方法で示すと，
```haskell
encodeString :: Encode{[*]}String
decodesString :: Decodes{[*]}String
encodeFork :: Encode{[* -> * -> *]} Fork
DencodesFork :: Decodes{[* -> * -> *]} Fork
encodeSequ :: Encode{[* -> *]} Sequ
DencodesSequ :: Decodes{[* -> *]} Sequ
```

### 和と積，単位元
カインドの表現について，和，積，単位限を定めよう．
```haskell
data Unit = Unit -- 単位元
data (:*:) a b = a :*: b  -- 積
data (:+:) a b = Inl a | Inr b -- 和
```
型`Fork`や`Sequ`のカインドについて表現すると次の通りになる．
```haskell
type Fork a = a :*: a
type Sequ a = Unit :+: ((Sequ (Fork a)) :+: (a :*: (Sequ (Fork a))))
```
また，型`List`の場合は，
```haskell
type List a = Unit :+: (a :*: (List a))
```
## type-indexed values
具体的にkind-indexed typeに値を代入して（specialize），Genericな関数を定義しよう．
```haskell
encode{|t::k|} :: Encode{[k]} t
encode{|Char|} = encodeChar
encode{|Unit|} _ = []
encode{|:+:|} ena enb (Inl a)  = 0 : ena a
encode{|:+:|} ena enb (Inr b) = 1 : enb b
encode{|:*:|} ena enb (a:*:b) = ena a++enb b
```
1行目, `t`はカインド`k`である．`Char`などは`*`になり，`:+:`等は`*->*->*`となる．  
Genericな関数をtype-indexed valuesと呼ぶ．
```haskell
decodes{|t::k|} :: Decodes{[k]} t 
decodes{|Char|} = decodesChar
decodes{|Unit|} bin = (Unit,bin)
decodes{|:+:|} dea deb (0:bin) = (Inl a,bin1) where (a,bin1) = dea bin
decodes{|:+:|} dea deb (1:bin) = (Inr b,bin1) where (b,bin1) = deb bin
decodes{|:*:|} dea deb bin = ((a:*:b),bin2)
    where
        (a,bin1) = dea bin
        (b,bin2) = deb bin1
```
このようにして，様々な型で`encode`と`decodes`を用いることができる．  
(`ena`, `enb`には`encode`を，`dea`, `deb`には`decodes`を入れれば良い．)

結局のところ要点は次の2つだろうか
 - 直積・直和型を上手く表現することでどんな型にも対応できる．
 - カインドが`*`の場合は中の値を直接ごにょごにょしなければならない．つまり，そういうときのための特別化ができる．

これらを実現するために，型の構造を比較する関数（言語機能）が必要というわけである．

## Haskellでの実現方法
GHC.Genericsを使用する．

## 参考
[Generic Haskell: Practice and Theory.](https://www.researchgate.net/publication/221621571_Generic_Haskell_Practice_and_Theory)