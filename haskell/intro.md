---
layout: post
title:  入門編
usemath: true
---
# 電卓代わりに使う

端末を開いて、
```sh
$ ghci
```
と打ち込みます。すると、以下のようなメッセージ及びプロンプト（対話環境）が出てきます。
```sh
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude>
```

まずは、電卓の代わりに使ってみましょう。
```
Prelude> 1+1
2
Prelude> 1+3*2
7
Prelude> 42/7
6.0
```

べき乗もできます。
```
Prelude> 2^100
1267650600228229401496703205376
```

対話環境を終了するには、`:quit` コマンドを使います。`:quit` はHaskellの文法ではなく、対話環境専用のコマンドです。
```
Prelude> :quit
Leaving GHCi.
```

# 数のリスト

Haskellでは、リストも扱えます。値をカンマで区切って、カッコ `[ ]` で囲むと、リストを作ることができます。
```
Prelude> [3,5,7]
[3,5,7]
```

`[a..b]` という表記を使うと、 `a` から `b` までの数のリストを作ることができます。
```
Prelude> [1..100]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
```

数のリストに対して `sum` 関数を使うと、総和を計算することができます。
```
Prelude> sum [1..100]
5050
```

一方、`product` 関数を使うと、数のリストの積を計算することができます。
```
Prelude> product [3,5,7]
105
```

問：product 関数を使って、10の階乗を計算してください。

解答例：
```
Prelude> product [1..10]
3628800
```

# 関数の定義

sum 関数や product 関数は、Haskell処理系であらかじめ定義されている関数ですが、自分で新しい関数を定義することもできます。

対話環境で関数を定義するには、let 文を使います。与えられた数に1を加える関数 succ を定義してみましょう。
```
Prelude> let succ n = n + 1
Prelude> succ 42
43
```

式の中で関数を定義するには、ラムダ式に似た記法を使います。よくあるラムダ式の記法だと今の succ 関数は `λn. n + 1` となりますが、Haskellの記法では `\n -> n + 1` となります（ラムダの代わりにバックスラッシュ、`.` の代わりに矢印 `->` を使う）。
```
Prelude> (\n -> n + 1) 42
43
```

問：与えられた数の2乗を計算する関数 square を定義してください。

問：与えられた自然数の階乗を計算する関数 fact を定義してください。

解答例：
```
Prelude> let square x = x * x
Prelude> square 42
1764
Prelude> let fact n = product [1..n]
Prelude> fact 5
120
Prelude> fact 10
3628800
```

# 条件

2つの値が等しいかどうか調べるには、 `==` 演算子を使います。結果は、真偽値 `True` / `False` で返ってきます。
```
Prelude> fact 5 == 120
True
```

逆に、 `/=` 演算子を使うと2つの値が異なる時に `True` が返ってきます。ほかの多くのプログラミング言語では `!=` 演算子を使いますが、Haskellでは数学の≠を意識して `/=` という演算子を使っているようです。
```
Prelude> 1 /= 0
True
```

剰余を求める関数 `mod` を使って、与えられた整数が偶数・奇数か判定する関数 isEven, isOdd を定義してみましょう。
```
Prelude> let isEven n = n `mod` 2 == 0
Prelude> isEven 6
True
Prelude> isEven 7
False
Prelude> let isOdd n = n `mod` 2 /= 0
Prelude> isOdd 6
False
Prelude> isOdd 7
True
```

2つの条件の論理積は `&&` 演算子、論理和は `||` 演算子でとることができます。
```
Prelude> let isPositiveEven n = n > 0 && n `mod` 2 == 0
Prelude> isPositiveEven 42
True
Prelude> isPositiveEven (-42)
False
```

# 例：素数判定

さっきまでよりも、もうちょっと複雑な関数を定義してみましょう。与えられた自然数が素数かどうか判定する関数 isPrime を定義します。
```
Prelude> let isPrime n = n > 1 && all (\k -> n `mod` k /= 0) [2..n-1]
Prelude> isPrime 7
True
Prelude> isPrime 9
False
```

この定義の中で、ここで初登場の all 関数を使っています。 all 関数は、 `all (関数) (リスト)` という形で使い、 `(関数)` が表す条件が `(リスト)` の全ての要素に対して成り立つなら `True` 、そうでなければ `False` を返します。

数学の全称記号∀を使えば、Haskellの ``all (\k -> n `mod` k /= 0) [2..n-1]`` という式は ``∀k∈{2,...,n-1}. (n `mod` k ≠ 0)`` という風に解釈できるでしょう。

ちなみに、この isPrime 関数は与えられた n を 2 から n-1 までの各自然数について割ることで素数かどうか判定しているので、あまり頭のいい実装とは言えません。

# リストの内包表記

これまで、リストの表記法として、値を `,` で区切って列挙する表記、 `..` を使った等差数列の表記を見てきました。Haskellにはもう一つ、便利なリストの表記法、「内包表記」があります。まずは例を見てみましょう。
```
Prelude> [p | p <- [1..100], isPrime p]
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

この内包表記 `[p | p <- [1..100], isPrime p]` は、与えられたリスト `[1..100]` の要素 `p` で、条件 `isPrime p` を満たすもののみからなる新しいリストを表します。

条件は複数書くことができます。次の例は、1から100までの素数で、7で割ると1余るものを計算しています。
```
Prelude> [p | p <- [1..100], isPrime p, p `mod` 7 == 1]
[29,43,71]
```

もう一つ、例を見てみましょう。次の例では、1から10までの自然数をそれぞれ2乗したリストを作っています。
```
Prelude> [n*n | n <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]
```

数学では、集合を `{p | p∈{1,...,100}, p is prime}` という風に記述することがありますが、Haskellのリストの内包表記を使うとそれに近い書き方ができます。

# 有理数の計算

Haskellでは、Data.Ratioモジュールを使うと有理数の計算ができます。対話環境では import Data.Ratio と打つとData.Ratioモジュールを読み込むことができます。
```
Prelude> import Data.Ratio
Prelude Data.Ratio>
```

（補足：importした後に、対話環境のプロンプトが `Prelude>` から `Prelude Data.Ratio>` に変わりました。これは、「対話環境で Prelude モジュールの関数を使える状態」から「対話環境で Prelude モジュールと Data.Ratio モジュールの関数を使える状態」に変わった、というような意味合いです。Preludeモジュールは、これまで使ってきた各種演算子や、sum、productなどの関数を定義しているモジュールです。）

分数を表すには `n`, `m` を整数として、 `n % m` という表記を使います。有理数同士の割り算は、普通の `/` 演算子でできます。
```
Prelude Data.Ratio> 1 % 2 + 1 % 3
5 % 6
Prelude Data.Ratio> (1 % 2 + 1 % 3) * (4 % 7) / (9 % 7)
10 % 27
```

# コードをファイルに記述する

ここまでは全て対話環境でコードを実行していましたが、せっかく定義した関数も、対話環境を終了すると消えてしまいます。そこで、これまで定義した関数をファイルに書いておくことにしましょう。intro.hs という名前のテキストファイルを、以下の内容で作ってください。
```haskell
succ n = n + 1

square x = x * x

fact n = product [1..n]

isEven n = n `mod` 2 == 0

isOdd n = n `mod` 2 /= 0

isPositiveEven n = n > 0 && n `mod` 2 == 0

isPrime n = n > 1 && all (\k -> n `mod` k /= 0) [2..n-1]
```

ghciの起動後に、`:load (ファイル名)` というコマンドを実行すると、そのファイルを読み込むことができます。
```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :load intro.hs
[1 of 1] Compiling Main             ( intro.hs, interpreted )
Ok, modules loaded: Main.
*Main> fact 5
120
*Main>
```

（補足：今度は、対話環境のプロンプトが `Prelude>` から `*Main>` になりました。Mainというのは、今ロードした intro.hs のモジュール名です。Haskellのソースコードにモジュール名を指定しなかった場合は Main というモジュール名になるのです。Preludeというのは「特にロードしなくても使える」モジュールなので、プロンプトが `*Main>` の状態でもPreludeの関数を使えます。）

# 場合分けと再帰による関数の定義

（詳しい説明：[型と関数](types-and-functions.html)）

関数の定義をファイルに書くことにより、複数行に渡る関数の定義を書くことができます。例えば、階乗を漸化式 n!=n×(n-1)! により計算してみましょう。さっき intro.hs に書いた fact の定義を消して（あるいは、新しいファイルを作って）、以下のように書き換えてください：
```haskell
fact 0 = 1
fact n | n > 0 = n * fact (n-1)
```

1行目は、見ての通り「`fact 0 = 1`」と定義しています。

2行目では、「`n > 0` の場合、`fact n = n * fact (n-1)`」と定義しています。縦棒 `|` と等号 `=` の間に条件を書いています。縦棒を使って書いた条件は「ガード」と呼ばれます。

縦棒の位置を揃えることで、複数の条件＆定義を連続して書けます。さっきの fact の定義は、次のように書いても同じことです：
```haskell
fact n | n == 0 = 1
       | n > 0  = n * fact (n-1)
```

問：二重階乗 n!!=n×(n-2)×…× (1 or 2) を計算する関数 doubleFact を定義してください。

解答例：
```haskell
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n | n > 1 = n * doubleFact (n-2)
```

あるいは
```haskell
doubleFact n | n == 0 = 1
             | n == 1 = 1
             | n > 1  = n * doubleFact (n-2)
```
あるいは
```haskell
doubleFact n = product [n,n-2..1]
```

# 応用：ベルヌーイ数の計算

これまでに学んだことを応用して、[ベルヌーイ数](http://ja.wikipedia.org/wiki/%E3%83%99%E3%83%AB%E3%83%8C%E3%83%BC%E3%82%A4%E6%95%B0)を計算するHaskellの関数を実装してみましょう。

ベルヌーイ数は、以下のように定義される有理数の列です。（ほかにも流儀があります）
\\[\frac{z}{e^z-1}=\sum_{n=0}^\infty \frac{B_n}{n!} z^n\\]
ですが、この定義は（性質の証明には役立ちますが）計算には向いていません。そこで、計算には、上の定義と等価な、次の漸化式を使います。（問：上の定義から漸化式を導け）
\\[
\begin{aligned}
B_0 &= 1 \\\\\\\\
B_{n} &= -\frac{1}{n+1} \sum_{k=0}^{n-1} \binom{n+1}{k} B_k \quad (n > 0)
\end{aligned}
\\]

では、ベルヌーイ数を計算するHaskellのコードを書いてみましょう。

まず、この漸化式では二項係数を使っているので、二項係数を計算する関数 binom を定義します。
```haskell
binom n k = product [n-k+1..n] `div` product [1..k]
```

ここで使った div というのは、整数の除算を行う関数です。すでに `` `mod` `` で使っていますが、関数名をバッククォート `` ` `` で囲むと中置の演算子として使うことができます。

この binom を使うと、ベルヌーイ数を計算する関数 bern は次のように書けます。
```haskell
bern 0 = 1
bern n | n > 0 = (-1 % (n+1)) * sum [fromInteger (binom (n+1) k) * bern k | k <- [0..n-1]]
```

関数 fromInteger が初出です。fromInteger は整数を有理数やその他の数に変換する関数（包含写像）です。`binom (n+1) k` の型は整数なのに対し、`bern k` の型は有理数なので、型を合わせる必要があるのです。

fromInteger の他は、上の bern の定義で使っているのはこれまでに説明した関数・文法ばかりなので、だいたい読めると思います。

これら binom と bern 関数を定義するファイルの内容は、次のようになります。bern.hsという名前で保存してください。Data.Ratioモジュールで定義されている有理数を使うため、ファイルの先頭に import Data.Ratio という記述が必要です。
```haskell
import Data.Ratio

binom n k = product [n-k+1..n] `div` product [1..k]

bern 0 = 1
bern n | n > 0 = (-1 % (n+1)) * sum [fromInteger (binom (n+1) k) * bern k | k <- [0..n-1]]
```

このファイルを対話環境に読み込んで、値をいくつか計算してみましょう。
```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :load bern.hs
[1 of 1] Compiling Main             ( bern.hs, interpreted )
Ok, modules loaded: Main.
*Main> bern 0
1 % 1
*Main> bern 1
(-1) % 2
*Main> bern 2
1 % 6
*Main> bern 3
0 % 1
*Main> [bern n | n <- [0..10]]
[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42,0 % 1,(-1) % 30,0 % 1,5 % 66]
*Main> bern 12
(-691) % 2730
```

なお、bern 20 あたりを計算してみるとわかると思いますが、この bern 関数はとても遅いです。 bern n の値を計算するためには 1から n-1 までについての n 個の bern の値が必要になるわけですが、このプログラムは一回計算した bern の値を覚えておらず、値が必要になるたびに計算し直しているので遅いのです。

逆に言えば、一回計算した値を覚えておくようなプログラムなら、それなりの速度で動きます。以下のコードを bern.hs の末尾にコピペします。（コードの意味は分からなくて結構です）
```haskell
bern' = 1 : [(-1 % (n+1)) * sum (zipWith (*) (map (fromInteger . binom (n+1)) [0..n-1]) bern') | n <- [1..]]
```

ここで定義した `bern'` は関数ではなく無限リストです。リストの i 番目の要素を取得するには `!!` 演算子を使って `(リスト) !! i` と書きます。take 関数を使って `take n (リスト)` と書くと、リストの最初の n 個の要素からなるリストを取得できます。

では、先ほどと同じようにghciを開いて bern.hs を開いてください。（あるいは、すでに bern.hs を開いていた場合は、 `:reload` コマンドによって bern.hs を再読み込みできます）
```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :load bern.hs
[1 of 1] Compiling Main             ( bern.hs, interpreted )
Ok, modules loaded: Main.
*Main> take 20 bern'
[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42,0 % 1,(-1) % 30,0 % 1,5 % 66,0 % 1,(-691) % 2730,0 % 1,7 % 6,0 % 1,(-3617) % 510,0 % 1,43867 % 798,0 % 1]
*Main> [bern n | n <- [0..19]]
[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42,0 % 1,(-1) % 30,0 % 1,5 % 66,0 % 1,(-691) % 2730,0 % 1,7 % 6,0 % 1,(-3617) % 510,0 % 1,43867 % 798,0 % 1]
*Main> bern' !! 20
(-174611) % 330
*Main> bern' !! 50
495057205241079648212477525 % 66
*Main>
```

bern 関数と bern' リストでは、明らかに bern' リストの方が計算が速いことが分かったでしょうか。

# まとめ（？）

- Haskellでは、数学の定義に近い書き方で関数を定義できる。
- ただし、素朴に書いたのでは計算が遅い場合があるので、その時は工夫が必要。
