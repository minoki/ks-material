---
layout: page
title: 入出力入門
---

これまでのセクションでは、主に対話環境を用いて、（数値・リスト等の）「計算」をしてきました。今度は、自力で入出力を行い、対話環境を使わなくても単体で動作するプログラムを書いてみましょう。

次の内容を `hello.hs` というファイル名で保存してください。Haskellのプログラム（ソースコード）の拡張子は `.hs` です。`main` という名前の関数で定義された処理が実行されます。
```haskell
main :: IO ()
main = putStrLn "Hello world!"
```

端末に `runghc hello.hs` と打って実行します。`runghc` というのは、Haskellのプログラムを実行するコマンドです。
```sh
$ runghc hello.hs
Hello world!
```

putStrLn は、文字列と改行を出力する関数です。文字列の後に改行を出力しない、putStr 関数もあります。

数やリストなどを出力したいときは、 show 関数を使って一旦文字列に変換します。

例（`hello2.hs`）：
```haskell
main = putStrLn (show [2,3,5,7])
```

実行例：
```sh
$ runghc hello2.hs
[2,3,5,7]
```

putStrLn関数とshow関数の組み合わせはよく使うので、これらを合成したprint関数が標準ライブラリで定義されています。
例：
```haskell
main = print [2,3,5,7]
```

putStrLn関数やputStr関数は、次のような型を持っています。
```haskell
putStrLn :: String -> IO ()
putStr :: String -> IO ()
```

空のカッコ `()` というのは、ただ一つの値 `()` を持つ、情報量0の型です。型の前に `IO` をつけると、実行するとその型の値を返す**処理**の型になります。

`IO` 型の値も、普通の値なので、他の型のようにリストに入れることができます。ただし、実際に「実行」されるのは、main関数の定義に書いたものだけです。

例：
```haskell
greetings :: [IO ()]
greetings = [putStrLn "Hello!", putStr "Goodbye!", putStrLn "Hajimemashite!"]

main :: IO ()
main = greetings !! 0
-- greetings !! 0 は putStrLn "Hello!" という値。
-- ちなみに、ハイフン2つから行末まではコメントとなる（行コメント）。
```

複数の処理を続けて行うには、 `>>` 演算子を使います。`>>` 演算子は、モノイドのように、結合的です。

例：
```haskell
main = putStrLn "Hello world!" >> putStrLn "Hajimemashite!"
```

# 1行読み込む

今度は、入力を受け取るプログラムを書いてみましょう。標準入力から1行読み込むには、getLine関数を使います。getLine関数の型は
```haskell
getLine :: IO String
```
です。`IO String` 型は、「実行するとString型の値を生み出す」処理の型、と言えるでしょう。

例（`getline.hs`）：
```haskell
main = putStrLn "Type your name: " >> getLine >> putStrLn "Thank you!"
```

実行例：
```
$ runghc getline.hs
Type your name: （入力待ち）
```

「`Jukousei`」と入力してエンターを押した後：
```
$ runghc getline.hs
Type your name: Jukousei
Thank you!
```

上の例（`getline.hs`）では、getLine関数で標準入力から読み込んでいますが、その結果（読み込んだ文字列）は使っていません。今度は、getLine関数で読み込んだ文字列に対して処理を行う（例えば、おうむ返しで出力する、すべて大文字にして出力する、など）プログラムを書いてみましょう。

前の処理の結果を使ってさらに処理をするには、 `>>=` 演算子を使います。演算子の左側が型 `IO a` の処理であれば、右側は型 `a -> IO b` をもつ、つまり型 `a` の値を受け取って別の処理 `IO b` を返す関数です。

最初は、`>>=` 演算子の右側に `putStrLn :: String -> IO ()` を渡して、入力をおうむ返ししてみましょう。

例（`getline2.hs`）：
```haskell
main :: IO ()
main = putStrLn "Type your name: " >> getLine >>= putStrLn
```

実行例：
```haskell
$ runghc getline2.hs
Type your name:
```

「`Jukousei`」と入力してエンターを押した後：
```haskell
$ runghc getline2.hs
Type your name: Jukousei
Jukousei
```

putStrLnの代わりに別の関数を使えば、入力された文字列に対して別の処理を行うことができます。与えられた文字列を大文字にして出力するputUpper関数を定義してみましょう。Data.Charモジュールで定義されたtoUpper関数を使います。

例（`getline3.hs`）：
```haskell
import Data.Char
-- モジュールのimportはファイルの先頭に書く。

putUpper :: String -> IO ()
putUpper s = putStrLn (map toUpper s)

main :: IO ()
main = putStr "Type your name: " >> getLine >>= putUpper
```

実行例：
```
$ runghc getline3.hs
Type your name:
```

「`Jukousei`」と入力してエンターを押した後：
```
$ runghc getline3.hs
Type your name: Jukousei
JUKOUSEI
```

関数を別に定義する代わりに、ラムダ式を `>>=` 演算子の後に直接書くこともできます。
```haskell
main :: IO ()
main = putStr "Type your name: " >> getLine >>= \s -> putStrLn (map toUpper s)
```
あるいは、関数合成演算子を使えば、次のようにも書けます。
```
main :: IO ()
main = putStr "Type your name: " >> getLine >>= putStrLn . map toUpper
```

問：入力された文字列の前後に `"Domo, "` と `"-san."` をつなげて出力するプログラムを書いてみましょう。

ヒント：文字列の連結には `++` 演算子を使う。

問の答えの実行例：
```
$ runghc question.hs
Type your name: tensai genius
Domo tensai genius-san.
```

## 入力を数値に変換する

今度は、数を入力させるプログラムを書いてみましょう。readIO関数を使うと、文字列を、数（Integerとか）などの値に変換できます。readIO関数の型は大雑把に言うと次のようになります（本当は正しくない）。
```haskell
readIO :: String -> IO a
```

例（`square.hs`）：
```haskell
-- 1行読み込んで、それを整数として解釈する関数
readInt :: IO Integer
readInt = getLine >>= readIO

main = readInt >>= \x -> print (x * x)
```

実行例：
```
$ runghc square.hs
42
1764
$ runghc square.hs
1001
1002001
```

getLine関数とreadIO関数の組み合わせはよく使うので、標準ライブラリでreadLnという関数が定義されています。

例：
```haskell
main = (readLn :: IO Integer) >>= \x -> print (x * x)
-- main = readLn >>= \x -> print (x * x) だと型があいまいになるので注意。
-- 型があいまいでも、この場合は適当にデフォルトの型が使われるが、なるべく明示したほうが良い。
```

# ファイルの内容を読み込む

今度は、ファイルの内容を読み込んでみましょう。

テキストファイルの内容を読み込むには、readFile関数を使います。readFileは次の型を持っています。ただし、FilePathというのはStringの別名です。ファイルの内容はStringとして帰ってきます。
```haskell
readFile :: FilePath -> IO String
```

次の内容で `input.txt` を作成しましょう。
```
Keisan sugaku!
Tanoshii jinsei!
Tanoshii Haskell!
```

`input.txt` の内容をそのまま出力するプログラムを書いてみましょう。

例：
```haskell
main = readFile "input.txt" >>= putStr
```

問：`input.txt` の内容をすべて大文字にして出力するプログラムを書いてみましょう。

## lines関数

lines関数を使うと、複数行からなる文字列を、行ごとに分割してリストにすることができます。
```haskell
lines :: String -> [String]
```

各行の文字数をリストとして出力するプログラムを書いてみましょう。

例（`len.hs`）：
```haskell
main = readFile "input.txt" >>= \s -> print (map length (lines s))
```

実行例：
```
$ runghc len.hs
[14,16,17]
```

# ファイルに書き込む

今度は、ファイルに書き込んでみましょう。

テキストをファイルに書き込むには、writeFile関数を使います。最初の引数はファイル名、二番目の引数は書き込む内容です。
```haskell
writeFile :: FilePath -> String -> IO ()
```

次のプログラムは、1から100までの素数のリストを `prime100.txt` というファイルに書き込みます。

例：
```haskell
isPrime :: Integer -> Bool
isPrime = （以前のセクションからコピペしてください）

main = writeFile "prime100.txt" $ show $ filter isPrime [1..100]
```

unlines関数を使うと、文字列のリストを、改行区切りの一個の文字列にすることができます。
```haskell
unlines :: [String] -> String
```

unlines関数を使って、素数のリストを改行区切りで出力するようにさっきの例を改造してみましょう。
```haskell
（さっきと同じなので略）

main = writeFile "prime100.txt" $ unlines $ map show $ filter isPrime [1..100]
```

問（復習）：改行を挟まない、ただ単に、リストに入った文字列を連結する関数は何だったでしょうか？

# リストに入った処理を順番に実行する

一般的なプログラミング言語では、「ループ」という文法を使って、たくさんの処理を順番に実行することができます。Haskellにはループはありませんが、代替手段はあります。

`sequence` または `sequence_` 関数を使うと、リストに格納したIO処理を順番に実行できます。実行した結果をリストとして欲しい場合は `sequence` を、実行結果を今後使わない場合は `sequence_` （最後にアンダースコアがつく）を使います。これらの関数は、次の型を持っています。
```haskell
sequence :: [IO a] -> IO [a]
sequence_ :: [IO a] -> IO ()
```

つまり、これらの関数を使えば、Haskellに「ループ」の文法がなくても、[前のセクション](list.html)で見たような豊富なリスト処理関数を使って、ループ相当のことができるというわけです。

最初に書いたハローワールドを、別の方法で出力してみましょう。

例：
```haskell
a :: IO ()
a = [putStr "Hello ", putStrLn "world!"]

main :: IO ()
main = sequence_ a
-- main = putStr "Hello " >> putStrLn "world!" と等価
```

map関数と組み合わせてみましょう。

例：
```haskell
a :: IO ()
a = map putStrLn ["Hello world!","Goodbye!"]

main :: IO ()
main = sequence_ a
```

複数の入力処理をsequence関数で実行してみましょう。

例：
```haskell
a :: [IO String]
a = [putStr "Type your name: " >> getLine, readFile "input.txt"]

main :: IO ()
main = sequence a >>= \x -> putStr (show x)
-- xは「標準入力から読み込んだ文字列」と「input.txtの内容」からなるリスト
```

例：
```haskell
main = sequence_ (map print [1..5])
```

実行結果：
```
1
2
3
4
5
```

# 練習問題

問：標準入力から整数 n を読み込み、n 未満の素数の一覧を改行区切りで prime.txt に出力するプログラムを書け。

ヒント：整数 n を読み込むには getLine 関数と readIO 関数の組み合わせ、または readLn 関数を使う。素数のリストを得るには、isPrime関数を使う。整数を文字列化するには show 関数を使う。改行区切りの文字列を作るには unlines 関数を使う。文字列をファイルに書き出すには writeFile 関数を使う。
