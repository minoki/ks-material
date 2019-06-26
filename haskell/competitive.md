---
layout: page
title: Haskellで競技プログラミング
---

競技プログラミングの問題を解こうと思ったら、真っ先に「標準入力から整数を複数個読み取る」というような処理を書く必要があります。しかし、普通のプログラミングの普通のチュートリアルでは標準入力の扱いは後回しにされます。ここの[入門編](intro.html)でも、最初はREPLから始めています。

ここでは、競技プログラミング向けのHaskell入門として、「標準入力から整数を複数個読み取る」などの競技プログラミング特有の事柄を解説します。

# 題材

題材として、AtCoderのpractice contestを取り上げます。

* [practice contest -- A - Welcome to AtCoder](https://atcoder.jp/contests/practice/tasks/practice_1)

この問題では、入力が3行、

```
a
b c
s
```

という形で与えられます。ただし、 `a`, `b`, `c` は整数で、 `s` は文字列です。2行目は、2つの整数が空白区切りで与えられます。

# 標準入力からの読み取りと、整数への変換

入力を読み取って解釈するために、

* 標準入力から文字列を読み取る
* 読み取った文字列を整数に変換する

のそれぞれを考えます。

標準入力から文字列を読み取るには、 `getLine` 関数や `getContents` 関数を使います。

```haskell
getLine :: IO String
getContents :: IO String
```

`getLine` 関数はその名の通り、標準入力から1行読み取ります。一方、 `getContents` 関数は標準入力を最後まで読み取ります。

人によっては `getContents` の方が好きという人もいるかもしれませんが、ここでは、 `getLine` 関数を使ってプログラムを書いていきます。

さて、1行目を読み取ったら、それを整数に変換しなくてはなりません。（Haskellの整数型はいくつかありますが、ここでは固定長整数 `Int` を使います）

と言っても、Haskellには文字列を _整数に_ 変換する専用の関数はありません。例によって、**型クラス**を使って「文字列から変換できる型」という風に一般化されています。

この場合の「文字列から変換できる型」が属するクラスは `Read` クラスで、実際に変換を行う関数はいくつか用意されています：

```haskell
read :: Read a => String -> a
readIO :: Read a => String -> IO a
reads :: Read a => ReadS a
type ReadS a = String -> [(a, String)]
```

この中では `read` 関数が一番単純です。文字列を受け取って、解釈した値を返します。もしも文字列を対象の型（この場合は `Int` ）として解釈できなかった場合は、例外が投げられてプログラムが終了します。

`readIO` 関数もだいたい同じですが、値をそのまま返すのではなく、 `IO` モナドに包んでいます。`read` との違いは、エラー時にIO例外が投げられることです。この場合はプログラムの側で例外を処理することができます。

`reads` 関数は、文字列が解釈できなかった場合は空のリストを、解釈できた場合は、値と、後続のの文字列のペアからなるリストを返します。例えば、 `"123yay!"` という文字列を解釈すると

```haskell
> reads "123yay!" :: [(Int,String)]
[(123,"yay!")]
```

という具合です。

このほか、 `getLine` 関数と `readIO` 関数を組み合わせた

```haskell
readLn :: Read a => IO a
```

という関数もあります。

`read` 系関数の注意点として、型が曖昧になりがち、という点があります。例えば、REPLで `read "123"` を実行した時に何が出力されるでしょうか？

```haskell
> read "123"
*** Exception: Prelude.read: no parse
```

GHCiのデフォルトでは、このように型が曖昧な状況では単位型 `()` や整数型 `Integer` 等のいくつかの型を順番に当てはめて最初に型チェックが成功した物を採用します。この場合は単位型 `()` が採用されたようで、パースエラーとなっています。

`read` した値を後で使う場合は、その使い方から型が決定する場合があります。例えば、

```haskell
> read "123" + 1
124
```

という例では、`read` した値に対して算術演算 `+` を適用しているので、GHCiによって `read` する型は数値っぽい型（`Num` クラス）だと判断されます。その結果、単位型 `()` ではなく多倍長整数型 `Integer` が採用されます。

型の曖昧性を完全に無くすには、 `read "123" :: Int` という風に型注釈を書くのが確実です。

# 実践

まずは入力の1行目です。`getLine` で読み取った文字列を `read` 系関数で整数に変換します。

ここでは3通り紹介しておきます：

```haskell
main = do
  line1 <- getLine
  let a = read line1 :: Int
```

```haskell
main = do
  line1 <- getLine
  a <- readIO line1 :: IO Int
```

```haskell
main = do
  a <- readLn :: IO Int
```

次、入力の2行目は少し難しいです。`read` 系関数では一度に1つしか整数を読み取れません。

一つの方法としては、 `reads` を使って入力行を「整数1個」と「残りの部分」に分割し、「残りの部分」をさらに `read` することです。

```haskell
main = do
  ... -- 1行めの読み取り：略
  line2 <- getLine
  let [(b,line2')] = reads line2 :: [(Int,String)]
      c = read line2' :: Int
```

別の方法としては、 `words` 関数を使って文字列を空白区切りでリストに変換することです。`words` 関数の型と使い方の例は、次のようになります：

```haskell
> :t words
words :: String -> [String]
> words "foo bar"
["foo","bar"]
> words "123 456"
["123","456"]
> map (read :: String -> Int) $ words "123 456"
[123,456]
```

返ってきたリストに対して `map` 関数を使って要素ごとに `read` 関数を適用すれば、整数のリストが得られます。

この場合は次のようになります：

```haskell
main = do
  ... -- 1行めの読み取り：略
  line2 <- getLine
  let [b,c] = map read $ words line2
```

これを1行で格好良く書くと

```haskell
main = do
  ...
  [b,c] <- map read . words <$> getLine
```

となります。

`<$>` 演算子は `fmap` 関数の別名です。 `<$>` よりも `.` の方が優先順位が高いので、 `map read . words <$> getLine` は

```haskell
fmap (map read . words) getLine
```

と同じ意味になります。

3行目は文字列を文字列のまま読み取ればいいので、 `getLine` 関数一発です。まとめると、入力を読み取る部分は

```haskell
main = do
  a <- readLn :: IO Int
  [b,c] <- map read . words <$> getLine
  s <- getLine
```

となります。

入力の部分ができたので、あとはロジックの部分と出力を実装します。

```haskell
main = do
  a <- readLn :: IO Int
  [b,c] <- map read . words <$> getLine
  s <- getLine
  putStrLn $ show (a + b + c) ++ " " ++ s
```

整数を文字列に変換するのに `show` 関数を使っています。文字列の連結には、リストと同じ `++` 演算子が利用できます。

ちなみに、「空白区切りでリストを文字列化する」ためには `unwords` 関数を使うこともできます。

```haskell
> :t unwords
unwords :: [String] -> String
> unwords ["123", "foo"]
"123 foo"
```

これを使って

```haskell
main = do
  a <- readLn :: IO Int
  [b,c] <- map read . words <$> getLine
  s <- getLine
  putStrLn $ unwords [show (a + b + c), s]
```

と実装することもできます。

# この先へ

もっと実践的な問題をHaskellで解く例が見たい、という人はhsjoihs氏の記事を読むと良いでしょう。

- hsjoihs氏の [AtCoder に登録したら解くべき精選過去問 10 問を Haskell で解いてみた](https://qiita.com/hsjoihs/items/25a08b426196ab2b9bb0)

## 中級者以上を目指す方へ

この記事では `map read . words <$> getLine` というパターンを紹介しましたが、実はこれは**非効率的**です。Haskellを使って競技プログラミング中級者以上を目指したい人は、効率を考えたデータ型を使う必要があります。

例えば、入力の読み取りに使う文字列型としては `String` 型ではなく `ByteString` 型を使います。

また、配列 `Array` やベクター `Vector` およびそれらの unboxed 版も使いこなせることが望ましいでしょう。

Haskellは純粋関数型言語で、リスト `[a]` や配列 `Array` は**不変**（一度定義したら中身を変更できない）ですが、アルゴリズムによっては破壊的代入を行う必要があります。そういう場合は可変な配列 `IOArray` や可変なベクター `MVector` を使います。

詳しいことは自分で調べてください。いくつかリンクを置いておきます：

* @myuon_myon, [Haskellで解くAtCoder – The curse of λ](https://myuon.github.io/posts/haskell-atcoder/), 2019年4月28日
* @hnw, [HaskellでAtCoderの問題を解く（入力の高速化編） – Qiita](https://qiita.com/hnw/items/3f7d27b742c5a1a99a9a), 2019年5月27日
* @mod_poppo, [Haskell で高速なプログラムを書くときに注意すること](https://blog.miz-ar.info/2016/06/writing-efficient-program-with-haskell/), 2016年6月28日
* @mod_poppo, [HaskellでAtCoderに参戦して水色になった](https://blog.miz-ar.info/2019/05/atcoder-with-haskell/), 2019年5月27日

書籍では、「Haskell入門 関数型プログラミング言語の基礎と実践」に `ByteString` や `Vector` の紹介があります。
