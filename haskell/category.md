---
layout: post
title: Haskellと圏
---

圏論を知らなくてもHaskellは使えますが、数学科の皆さんは圏論をある程度知っている可能性が高いと思われるので、Haskellと圏論の関係についてゆるく書いておきます。

* TOC
{:toc}

# Hask 圏

Haskellの型を対象、Haskellの関数を射とする圏を Hask 圏と呼びます（“ell” はどこに消えたのかは知りませんが、こう呼ばれています）。

では、 Hask 圏の性質を見ていきましょう。なお、特に断らない限り、以下ではボトム ⊥ を無視します。

注：ボトム ⊥ について
Haskellの計算ではエラーが出たり、無限ループに陥ったりすることがあります。そのような「値が帰ってこない計算」は、ボトム ⊥ という特殊な値を持つと考えます。

## 積（直積）

2要素のタプル型 `(a,b)` は `a` と `b` の直積です。第一射影と第二射影は、標準ライブラリで fst, snd という名前で用意されています：
```haskell
fst :: (a,b) -> a
fst (x,y) = x
snd :: (a,b) -> b
snd (x,y) = y
```

もちろん、直積は一意的ではなく、自分で直積の普遍性を持つ（直積と同型な）データ型を定義することもできます。例えば、次のようにします：
```haskell
data Pair a b = Pair a b
```
注：「`(,)`」という名前は特別扱いされているので、自分で `(,)` という名前の型を定義することはできません。

`f :: a -> b` と `g :: a -> c` に対して `a -> (b,c)` を作る関数は、 Preludeにはありませんが、Control.Arrow に
```haskell
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b,c))
```
という関数（実際はもっと一般化されています）があって `f &&& g :: a -> (b,c)` となります。

Control.Arrow には `f :: a -> b` と `g :: c -> d` から `(a,c) -> (b,d)` を作る関数も用意されています：
```haskell
(***) :: (a -> b) -> (c -> d) -> ((a,b) -> (c,d))
```

## 余積（直和）

Either 型（`Either a b`）は `a` と `b` の直和です。
```haskell
data Either a b = Left a | Right b
```

Either 型のデータコンストラクターである Left と Right が、そのまま入射になります：
```haskell
Left :: a -> Either a b
Right :: b -> Either a b
```

## 始対象、終対象

単位型 `()` は（ボトムを除けば）唯一の値 `()` を持ちます。よって、どんな型からでも `()` への射（関数）を定義でき、かつそのような射（関数）は一意的です。従って、単位型 `()` は終対象です。

自分で同じような型を作るには次のようにします：
```haskell
data Unit = Unit
```
（「`()`」という名前は特別扱いされているので、自分で `()` という名前の型を定義することはできません）

一方、始対象の方はどうでしょうか。集合圏の場合は空集合が始対象だったので、その類推で行けば、空集合のように「値を持たない」型を作れば良さそうです。自分で定義しても良いですが、 Data.Void モジュールにそういう型が用意されています：
```haskell
data Void
```
（ちなみに、このように値を持たないことを、 “uninhabited” （無人の、居住者がいない）と呼びます。）

値を持たない型からは任意の型への関数が（一意的に）定義できますが、それも Data.Void モジュールで用意されています：
```haskell
absurd :: Void -> a
```
“absurd” という名前は、論理学で言う矛盾律（爆発律とも言う）に由来するものでしょう。

## 冪対象とデカルト閉圏

Hask 圏は冪対象を持っています。つまり、Hask 圏の射 `a -> b` はそれ自体が Hask 圏の対象ともみなせます。

（TODO: もっと詳しく書く）

# Haskell における関手

圏だけ定義しても何の役にも立たないので、 Hask 圏の関手を考えます。

## Hask 圏の自己関手と Functor クラス

IO や Maybe, リスト `[]` などの型コンストラクターは型に型を対応させます。これに加えて射の対応も用意できれば、型コンストラクターを Hask 圏から Hask 圏への関手だと思うことができます。

例えば、リスト `[]` に対しては `map :: (a -> b) -> ([a] -> [b])` が射の対応となっていて、これは関手の公理
```haskell
map id = id
map (f . g) = map f . map g
```
を満たします。よって、リスト `[]` は関手です。

IO 型に対しても `(a -> b) -> (IO a -> IO b)` という関数が定義でき、公理を満たすので、 IO も関手です。

Haskell には、こういう、 Hask 圏の自己関手 (endofunctor) を表す型クラス Functor が用意されています：
```haskell
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

圏論では、射の対応も関手と同じ記号を使って Ff : Fa → Fb と書くことが多いかと思いますが、Haskellの Functor クラスでは、射の対応の名前は常に fmap となります（`fmap f :: F a -> F b`）

Functor のインスタンスは、関手の公理
```haskell
fmap id = id
fmap (f . g) = fmap f . fmap g
```
を満たします。（…と言いたいところですが、Haskellの型クラスにはこういう公理を強制する力はないので、「満たすべきです」「満たすように定義するべきです」と言った方が適切です。）

なお、数学では恒等関手という、各対象について Id a = a となる関手 Id が作れますが、HaskellにおけるFunctorクラスのインスタンスは型コンストラクターなので、そのような Id をFunctorクラスのインスタンスにはできません。
```haskell
-- Id は Id a = a を満たすが、 instance Functor Id というインスタンスは作れない
type Id a = a

-- instance Functor Identity とはできるが、 Identity a = a ではない（同型にはなる）
newtype Identity a = Identity a
```
（ちなみに、この Identity 型は [Data.Functor.Identity モジュール](https://hackage.haskell.org/package/base/docs/Data-Functor-Identity.html)に用意されています）

## もっと一般の関手

圏論では直積圏とか双対圏をホイホイ扱えるので、直積 × を C×C から C への関手だと思ったり、 Hom を C<sup>op</sup> × C からの関手だと思う、というようなこともできますが、さっきも書いたように、Haskellの Functor クラスのインスタンスになるのは Hask 圏から Hask 圏の関手（の一部）だけです。

逆に言えば、 Functor クラスとは別の、新たな型クラスを作れば、別の種類の関手を扱うことができます。

例えば、次のような型クラスを作れば、Hask × Hask から Hask への関手（双関手; bifunctor）を一般的に扱えます：
```haskell
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> (p a c -> p b d)
```

2要素タプル `(,)` や Either 型はこの Bifunctor のインスタンスになります：
```haskell
instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)
instance Bifunctor Either where
  bimap f g (Left x) = Left (f x)
  bimap f g (Right y) = Right (g y)
```
この型クラスは、 [Data.Bifunctor モジュール](https://hackage.haskell.org/package/base/docs/Data-Bifunctor.html)ですでに用意されています。

あるいは、 Hask<sup>op</sup> から Hask への関手（Hask から Hask への反変関手）を扱いたければ、
```haskell
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
```
という型クラスを用意すればいいです。（この型クラスは [contravariant パッケージ](https://hackage.haskell.org/package/contravariant)の [Data.Functor.Contravariant モジュール](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html)で用意されています）

# モナド

## 圏論でのモナド

ある種の自己関手は**モナド**と呼ばれます。関手 T がモナドであるとは、

- （単位射）自然変換 η : I → T （I は恒等関手）
- （積）自然変換 μ : T∘T → T

があって、公理

- 単位則：μ∘ηT = μ∘Tη = id
- 結合則：μ∘(μT) = μ∘(Tμ)

を満たすことです。モノイド圏 (monoidal category) を知っている人向けの説明をすれば、モナドとは、自己関手の圏におけるモノイド対象です。

（注：自己関手の圏とは、自己関手を対象とし、その間の自然変換を射とする圏である。関手の合成によって、対象どうしの積（モノイド積とかテンソル積とか呼ばれる）が定まる。）

## Haskellにおけるモナド

モナドの定義を素直にHaskellの言葉で書けば、次のような型クラスになるでしょう：
```haskell
class (Functor m) => Monad m where
  unit :: a -> m a
  join :: m (m a) -> m a
{-
公理：
  join . unit      = id               :: m a -> m a
  join . fmap unit = id               :: m a -> m a
  join . join      = join . fmap join :: m (m (m a)) -> m a
-}
```

…が、実際には次のような定義になっています：
```haskell
class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  m >> k = m >>= \_ -> k

  fail :: String -> m a
  fail s = error s
{-
公理：
  return a >>= k          = k a
  m        >>= return     = m
  m >>= (\x -> k x >>= h) = (m >>= k) >>= h
  fmap f xs               = xs >>= return . f
-}

-- join は Monad クラスのメソッドではないが、 Control.Monad モジュールで定義されている
join :: Monad m => m (m a) -> m a
join m = m >>= id
```
（Applicative は Functor のサブクラスですが、ここでは解説しません）

- return は単位射のことです。
  - 他のプログラミング言語での return 文はその場で関数の実行を中断しますが、 Haskell の return はそんなことはしません。
- `(>>=)` は bind と呼ばれる演算で、 join を使って書くと次のようになります：
  - `x >>= g = join (fmap g x)`
  - 「公理」にもあるように、 fmap は return と `(>>=)` で書けます：`fmap f x = x >>= (return . f)`
  - 以前の Haskell では Functor が Monad のスーパークラスになっていませんでした（！）が、最近の GHC では Functor が（Applicative を介して）Monad のスーパークラスになっています。
  - モナドを「副作用のある計算」と捉える場合は、 `(>>=)` は「以前の処理の結果をもとに次の処理を実行する」という関数になります。
- `(>>)` は、「以前の処理の結果を捨てて次の処理を行う」という関数になります。
- `fail :: String -> m a` は、Haskellのdo記法におけるパターンマッチが失敗した場合に使われます。
  - 数学的なモナドに対応物はありません。そういう意味で、 Haskell の Monad クラスは数学的なモナドから乖離しています。
  - これでは美しくないので（？）、 fail を Monad クラスから追放しようという動きがあります（[MonadFail Proposal](https://wiki.haskell.org/MonadFail_Proposal)）

Haskellにおいてモナドがなぜ必要になるのか、どういう使われ方をするかについては、[モナド](monad.html)を見てください。
