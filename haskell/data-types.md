---
layout: page
title: データ型とパターンマッチ
---

# データ型の定義とパターンマッチの基本

これまでは主に既存のデータ型（整数やリスト）を扱ってきましたが、Haskellでは自分でデータ型を定義することもできます。

例えば、2個の整数の組を表すデータ型の定義は、次のようになります：
```haskell
data IntPair = MkIntPair Integer Integer
```
data の直後の IntPair が型名です。Haskellにおいて関数名や変数名は小文字から始めますが、型名は大文字から始めます。

イコール `=` の直後の MkIntPair はデータコンストラクター（データ構築子）と呼ばれ、IntPair 型の値を作るのに使えるほか、IntPair 型の値から構成要素であるIntegerを取り出す（パターンマッチ）のに使えます。データコンストラクターも大文字から始めます。

「IntPair型の値を作る」場合のデータコンストラクターは、関数として使えます。
```haskell
MkIntPair :: Integer -> Integer -> IntPair
-- MkIntPair 1 2 :: IntPair
```

IntPair型の値から、要素である Integer の値を取り出すには、パターンマッチを使います。
例えば、組の2つの整数の和を計算する関数は、次のようになります：
```haskell
f :: IntPair -> Integer
f (MkIntPair x y) = x + y
```

対話環境で、データ型の定義と関数の定義を試すには
```
Prelude> data IntPair = MkIntPair Integer Integer
Prelude> let f (MkIntPair x y) = x + y
Prelude> f (MkIntPair 3 5)
8
```
とすれば良いでしょう。

関数の引数に対してではなく、他の値（他の関数の結果など）についてパターンマッチを行う場合には、 case 式を使います。

さっきと同じ f を case 式を使って書くと、次のようになります：
```haskell
f p = case p of { MkIntPair x y -> x + y }
```

of 以下の波かっこ `{ }` は、適切にインデントする場合は省略できます：
```haskell
f p = case p of
  MkIntPair x y -> x + y
```

なお、データコンストラクターと型名を同じにすることもできます：
```haskell
data IntPair = IntPair Integer Integer
```

# 多相的なデータ型

さっきの IntPair の要素の型は Integer で固定でしたが、他の型についても似たような型を使いたいとしましょう。
```haskell
data DoublePair = MkDoublePair Double Double
data StringPair = MkString String String
```

こんな時に、いちいち新しい型を定義しなくても、要素の型をパラメーターとして指定できるようにすると便利です。

Haskellでは、そのような多相的なデータ型を定義することができます。
```haskell
data Pair a = MkPair a a
```

この Pair は型ではなく、「型を受け取って型を返す写像」と見なすことができます。このような写像を型コンストラクター（型構築子）と呼びます。さっきの IntPair 型に相当するのは Pair Integer 型になります。

# 成功と失敗を表す

計算や処理は失敗する場合があります。例えば、整数の割り算の場合は、分母が0だと失敗します。このような場合は例外を発生させることもあります：
```
Prelude> 1 `div` 0
*** Exception: divide by zero
```
…が、例外は扱いづらいので、通常の計算結果として失敗を表すことを考えましょう。

ここでは、「成功か失敗か」、そして「成功した場合の結果」を表せるデータ型を作り、計算結果としてそのデータ型の値を返すことにします。集合の直和みたいな感じです。
```haskell
data DivisionResult = Successful Integer | DivisionByZero

safeDiv :: Integer -> Integer -> DivisionResult
safeDiv n m | m == 0    = DivisionByZero
            | otherwise = Successful (n `div` m)
```

DivisionResult 型は、 Successful と DivisionByZero の2つのデータコンストラクターを持ちます。定義の際にはデータコンストラクターを縦棒 `|` で区切ります。

この DivisionResult 型から場合分けをして値を取り出す場合も、パターンマッチが使えます。
```haskell
isSuccessful :: DivisionResult -> Bool
isSuccessful (Successful _) = True
isSuccessful DivisionByZero = False

divisionResultOrZero :: DivisionResult -> Integer
divisionResultOrZero (Successful x) = x
divisionResultOrZero DivisionByZero = 0 -- ゼロ除算の場合は計算結果の代わりに 0 を返す
```

case 式を使うと、次のようにも書けます：
```haskell
isSuccessful x = case x of { Successful _ -> True ; DivisionByZero -> False }
divisionResultOrZero x = case x of { Successful x -> x ; DivisionByZero -> 0 }
```

または、波かっこを省略して：
```haskell
isSuccessful x = case x of
  Successful _ -> True
  DivisionByZero -> False

divisionResultOrZero x = case x of
  Successful x -> x
  DivisionByZero -> 0
```

実はこの DivisionResult 型をもっと一般化したものが Prelude で定義されています：
```haskell
data Maybe a = Just a
             | Nothing
```
Maybe は多相的で、2つのデータコンストラクターを持ちます。Just が「成功」を表し、 Nothing が「失敗」を表します。

課題：Maybe 型を使って、 safeDiv, isSuccessful, divisionResultOrZero のそれぞれの関数を定義せよ。

# もっと複雑な型

もっと複雑な型を定義してみましょう。

「定数と足し算、引き算、掛け算からなる式」を表す型を作ってみましょう。
```haskell
data Expr = Const Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          deriving (Eq,Show)
```
このように、データ型を定義する際に、中身の型としてそのデータ型自身（この場合は Expr）を使うことができます。
（`deriving (Eq,Show)` は、コンパイラーに Eq クラスと Show クラスのインスタンスを適当に作ってもらうための指示です。）
この Expr 型で表される式は、例えば `Mul (Add (Const 1) (Const 2)) (Const 3)` となります。

このような式を計算する関数 eval は、次のように定義できます：
```haskell
eval :: Expr -> Integer
eval (Const n) = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
```

# 標準ライブラリーのデータ型

Haskellには言語組み込み、あるいは標準ライブラリー（Prelude）で、いくつかのデータ型が定義されています。

## 単位型 ()

単位型は、値を1つだけ持つ型です。唯一の値（データコンストラクター）も同じ空かっこ `()` で表されます。

ちなみに、空かっこ `()` を型名やデータコンストラクターとする型は自分で定義できません。同等な型を自分で定義するとすれば、次のようになるでしょう：
```haskell
data Unit = Unit
```

## タプル (,)

2種類の型の組です。集合の直積みたいなものです。

Int と String の組であれば、`(Int,String)` 型となります。値（データコンストラクター）の記法も `(x,y)` です。

型コンストラクターとデータコンストラクターを単独で書く状況では、カッコの中にカンマだけ `(,)` を書いて表します。
```haskell
Prelude> :t (,)
(,) :: a -> b -> (a, b)
```

これも名前が特殊なので、このような型名およびデータコンストラクターを持つ型は自分で定義できません。同等な型を自分で定義するとすれば、次のようになるでしょう：
```haskell
data Pair a b = Pair a b
```

## Maybe

さっきも説明したように、「成功か失敗か」および「成功した場合の値」を持ちます。定義は次の通りです：
```haskell
data Maybe a = Just a
             | Nothing
```

## Either

2種類の型のいずれか一方を表します。集合の直和みたいなものです。

これも Maybe と同じように、「成功か失敗か」を表すのに使われますが、「成功時の値」だけではなく「失敗時の値」（エラーメッセージ等）も持てるのが違いです。Right が成功時の値に使われることが多いようです。

データコンストラクターは Left と Right です。
```haskell
data Either a b = Left a | Right b
```

## リスト []
すでに以前のページで扱ったと思います。データコンストラクターは空リスト `[]` とコロン `(:)` です。
これも型名とデータコンストラクターの名前が特殊です。あえてHaskell風の定義を書くとすれば、次のようになるでしょうか：
```haskell
data [a] = []         -- 空リスト
         | (:) a [a]  -- 先頭の要素と、残り
```

同等な型を自分で定義するとすれば、次のようになるでしょう：
```haskell
data List a = Nil
            | Cons a (List a)
```
