---
layout: post
title: リストと文字列
---
[入門編](intro.html)では数のリストを少し扱いました。この記事ではリストについてもう少し見ていきます。

この記事では主に対話環境を使って説明するので、対話環境を開いておきましょう。
```
$ ghci
```

# 等差数列

Haskellでは等差数列を簡単に作ることができます。
```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

初項の他に第二項を指定することで、公差を指定した等差数列を作ることができます。
```
Prelude> [1,3..12]
[1,3,5,7,9,11]
Prelude> [0,100..1000]
[0,100,200,300,400,500,600,700,800,900,1000]
```

# 文字列

Haskellにおいては、文字列というのは単なる文字のリストです。文字列は二重引用符、文字は一重引用符で括ります。
```
Prelude> "Hello"
"Hello"
Prelude> 'A'
'A'
Prelude> ['H','e','l','l','o']
"Hello"
```
文字列がリストだということは、つまりリストに対する演算子や関数がそのまま文字列に対して使えるということです。

show関数を使うと、数やリストなどを文字列に変換することができます。
```
Prelude> show 123
"123"
Prelude> show [1..10]
"[1,2,3,4,5,6,7,8,9,10]"
```

# リストに関する演算子
コロン `:` 演算子を使うと、リストの先頭に要素を付け足すことができます。
```
Prelude> 1 : [2,3,4]
[1,2,3,4]
```
`:` 演算子は右結合なので、 `1:2:3:4:[]` と書くと `1:(2:(3:(4:[])))` と解釈されます。
```
Prelude> 1:2:3:4:[]
[1,2,3,4]
Prelude> 'T':"odai"
"Todai"
```

`!!` 演算子を使うと、リストの要素を番号で指定して得ることができます。リストの最初の要素が0番目となります。
```
Prelude> [2,3,5,7] !! 2
5
Prelude> "Komaba" !! 3
'a'
```

リストを連結するには、 `++` 演算子を使います。
```
Prelude> [2,3,5] ++ [10,20,30]
[2,3,5,10,20,30]
Prelude> "Tensai" ++ "Genius"
"TensaiGenius"
```

# リストに関する関数

## map関数

map関数は、リストの各要素に対して関数を適用し、新しいリストを作ります。

f を関数として、 `map f [x,y,z]` と書くと、 `[f x,f y,f z]` と等価になります。
```
Prelude> map (\x -> x + 1) [3,5,7]
[4,6,8]
Prelude> map show [1..5]
["1","2","3","4","5"]
```

## reverse関数
reverse関数は、その名の通りリストを反転させます。
```
Prelude> reverse [1,3,5]
[5,3,1]
Prelude> reverse "tensai"
"iasnet"
```

## filter関数
filter関数を使うと、リストから条件に合致する要素のみを抜き出すことができます。`filter （条件を表す関数） （リスト）` の形で使います。
```
Prelude> filter (\n -> n `mod` 2 == 0) [2,3,5,7,11]
[2]
Prelude> filter (\n -> n `mod` 4 == 1) [2,3,5,7,11,13,17]
[5,13,17]
```

入門編で定義したisPrime関数を使って、整数のリストから素数だけを取り出してみましょう。
```
Prelude> let isPrime n = n > 1 && all (\k -> n `mod` k /= 0) [2..n-1]
Prelude> filter isPrime [1..100]
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

問：100番目の素数を求めてください。

ヒント：リストのn番目の要素を得るには `!!` 演算子を使う。

## length関数

length関数はリストの長さを返します。

```
Prelude> length [2,3,5,7]
4
Prelude> length "Hello"
5
Prelude> length [0,5..100]
21
```

100以下の素数がいくつあるか数えてみましょう。
```
Prelude> length (filter isPrime [1..100])
25
```

問：1000以下の素数は何個あるでしょうか。1000以下の素数で、4で割ると1あまるものは何個あるでしょうか。

## concat関数

concat関数は、リストからなるリストを繋げて1つのリストにします。
```
Prelude> concat [[1,2,3],[7,8],[],[100,101]]
[1,2,3,7,8,100,101]
Prelude> concat ["Hel","lo ","wor","ld!"]
"Hello world!"
Prelude> concat (map show [1..5])
"12345"
```

## take関数

take関数を使うと、リストの最初の数項だけを取り出すことができます。
```
Prelude> take 10 [1,3..100]
[1,3,5,7,9,11,13,15,17,19]
Prelude> take 10 (filter isPrime [1..100])
[2,3,5,7,11,13,17,19,23,29]
```

## drop関数

drop関数を使うと、リストの最初の数項を「落とす」ことができます。
```
Prelude> drop 10 [1,3..100]
[21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99]
Prelude> drop 10 (filter isPrime [1..100])
[31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

# 無限リスト

Haskellでは無限リストを扱うことができます。

等差数列で最後の項を指定しないと無限リストになります。

例：`[0..]` と書くと `[0,1,2,3,…]` という無限リストができる。（対話環境で表示しようとすると止まらないので注意！）

無限リストの中身を見てみたい時は、take関数と組み合わせて使うと便利です。
```
Prelude> let naturalNumbers = [0..] -- 無限リスト
Prelude> take 10 naturalNumbers
[0,1,2,3,4,5,6,7,8,9]
Prelude> take 20 naturalNumbers
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
```

filterやmapなどの関数も、普通に無限リストに対して使えます。
```
Prelude> let primes = filter isPrime [1..]
Prelude> take 10 primes
[2,3,5,7,11,13,17,19,23,29]
Prelude> take 20 primes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
```

一方で、reverse関数やlength関数は無限リストに対しては使えません（使おうとすると無限ループに陥ります）。

# 補足：文字についての関数

Data.Charモジュールを読み込むと、文字に関する関数を使えるようになります。Data.Charモジュールの関数と、この記事で紹介したリストの関数を使うと、文字列操作の幅が広がります。

ghciで `import Data.Char` と入力すると、Data.Charモジュールが読み込まれます。
```
Prelude> import Data.Char
Prelude Data.Char>
```

## toUpper / toLower関数
アルファベットを大文字／小文字にします。map関数と組み合わせてみましょう。
```
Prelude Data.Char> map toUpper "TensaiGenius"
"TENSAIGENIUS"
Prelude Data.Char> map toLower "TensaiGenius"
"tensaigenius"
```

## isUpper / isLower関数
アルファベットが大文字か・小文字か判断します。filter関数と組み合わせると、文字列から大文字だけ・小文字だけを抜き出すことができます。
```
Prelude Data.Char> filter isUpper "I'm Tensai Genius!"
"ITG"
Prelude Data.Char> filter isLower "I'm Tensai Genius!"
"mensaienius"
```
