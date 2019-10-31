# 電卓を作る（Parsecによるパーサーの実装）

この入門記事の「入門編」では、Haskellを電卓代わりに使いました。
今度は、電卓っぽいものを自分で作ってみましょう。

ここでの「電卓っぽいものを作る」とは、「文字列 `"1 + 2 * 3"` を与えると 7 を返すようなHaskellの関数、またはそういうプログラムを作る」ことが目標です。

自分で電卓言語を実装することには次のようなメリットがあります：

* 型システムに縛られない
    * Haskellでは整数型 `Integer` と有理数型 `Rational`、浮動小数点数型 `Double` は厳格に区別され、混ぜて使うことはできません。しかし、自分で実装した電卓であれば「整数を自然に有理数として扱う」ような規則を実現できます。
* 独自の文法を追加できる
    * Haskellの文法では、例えば階乗 `!` のような後置演算子は実装できません。ですが、文法を自分で定義すれば、（定義できる範囲で）なんでもありです。LaTeX風の `\binom{6}{2}` という文法を定義することだってできます。

## プロジェクトを作る

まずは作業用のプロジェクトを作成しましょう。

### stackの場合

`stack` を使う場合は

```shell-session
$ stack new simple-calc
$ cd simple-exec/
```

コードを書くには `app/Main.hs` を編集します。
ビルドには `stack build --fast` を実行し、作ったプログラムを実行するには `stack exec simple-calc-exe` を実行します。

`package.yaml` というファイルを編集します。
Haskellの標準機能を使うだけのプログラムならこの状態でビルドできるのですが、今回は `parsec` というHaskellパッケージを使いたいので、そのことをstackに指示します。
そのために、 `package.yaml` の以下の部分を

```yaml
dependencies:
- base >= 4.7 && < 5
```

このように書き換えます：

```yaml
dependencies:
- base >= 4.7 && < 5
- parsec
```

この状態で `stack build --fast` を実行すると、初回は `parsec` 自体のビルドも行われます。2回目以降は、自分で書いたプログラムのビルドだけが行われます。

### cabalの場合

`cabal` を使う場合は新しいディレクトリを作って `cabal init` を実行します。何回か入力を求められますが、だいたいデフォルトで良いでしょう。

`What does the package build: `　では `2) Executable` を選んでおけば良いでしょう。

```shell-session
$ mkdir simple-calc
$ cd simple-calc
$ cabal init
Please choose version of the Cabal specification to use:
 * 1) 1.10   (legacy)
   2) 2.0    (+ support for Backpack, internal sub-libs, '^>=' operator)
   3) 2.2    (+ support for 'common', 'elif', redundant commas, SPDX)
   4) 2.4    (+ support for '**' globbing)
Your choice? [default: 1.10   (legacy)]  
Package name? [default: simple-calc] 
Package version? [default: 0.1.0.0] 
Please choose a license:
   1) GPL-2
   2) GPL-3
   3) LGPL-2.1
   4) LGPL-3
   5) AGPL-3
   6) BSD2
 * 7) BSD3
   8) MIT
   9) ISC
  10) MPL-2.0
  11) Apache-2.0
  12) PublicDomain
  13) AllRightsReserved
  14) Other (specify)
Your choice? [default: BSD3] 
Author name? [default: ***] 
Maintainer email? [default: ***] 
Project homepage URL? 
Project synopsis? 
Project category:
 * 1) (none)
   2) Codec
   3) Concurrency
   4) Control
   5) Data
   6) Database
   7) Development
   8) Distribution
   9) Game
  10) Graphics
  11) Language
  12) Math
  13) Network
  14) Sound
  15) System
  16) Testing
  17) Text
  18) Web
  19) Other (specify)
Your choice? [default: (none)] 
What does the package build:
   1) Library
   2) Executable
   3) Library and Executable
Your choice? 2
What is the main module of the executable:
 * 1) Main.hs (does not yet exist, but will be created)
   2) Main.lhs (does not yet exist, but will be created)
   3) Other (specify)
Your choice? [default: Main.hs (does not yet exist, but will be created)] 
Source directory:
 * 1) (none)
   2) src
   3) Other (specify)
Your choice? [default: (none)] 
What base language is the package written in:
 * 1) Haskell2010
   2) Haskell98
   3) Other (specify)
Your choice? [default: Haskell2010] 
Add informative comments to each field in the cabal file (y/n)? [default: n] 

Guessing dependencies...

Generating LICENSE...
Generating Setup.hs...
Generating CHANGELOG.md...
Generating Main.hs...
Generating simple-calc.cabal...

Warning: no synopsis given. You should edit the .cabal file and add one.
You may want to edit the .cabal file and add a Description field.
```

ビルドするには

```
$ cabal v2-build
```

を実行します。
コンパイルが成功したら、

```
$ cabal v2-exec simple-calc
```

で実行できます。
いちいち `v2-build` してから `v2-exec` するのが面倒であれば、

```
$ cabal v2-run simple-exec
```

という書き方もできます。

Haskellの標準機能だけを使う簡単なプログラムであればこの状態でビルドできるのですが、今回は `parsec` というHaskellパッケージを使いたいので、そのことをcabalに指示します。
具体的には、 `simple-calc.cabal` の

```
  build-depends:       base >=4.12 && <4.13
```

という部分を

```
  build-depends:       base >=4.12 && <4.13, parsec
```

という風に書き換えます。

### プログラム

では、実際のプログラムを書きましょう。
`Main.hs` に以下の内容を書き込みます。

```haskell
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

type Parser a = Parsec String () a

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell

reservedOp :: String -> Parser ()
reservedOp = TT.reservedOp Lang.haskell

natural :: Parser Integer
natural = TT.natural Lang.haskell

atom :: Parser Integer
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> natural

expr :: Parser Integer
expr = buildExpressionParser
       [[binary "*" (*) AssocLeft, binary "/" div AssocLeft]
       ,[binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

main :: IO ()
main = do putStrLn "Enter expression:"
          s <- getLine
          case parse expr "stdin" s of
            Left err -> print err
            Right x -> print x
```

細かい説明は抜きにして、ビルド・実行してみましょう。

stackの場合は

```
$ stack build --fast
$ stack exec simple-calc-exe
```

cabalの場合は

```
$ cabal v2-build
$ cabal v2-exec simple-calc
```

です。

起動したら `Enter expression:` というメッセージが表示されるので、適当な式を打ち込みます。すると、その計算結果が表示されます。

```
Enter expression:
1 + 2
3
```

## `Parser` モナド

コードの上の方（importの後）で定義している `Parser` 型が、今回主役となる型です。
`parsec` パッケージの提供する抽象的な `Parsec` 型に、パースする文字列の型 `String` とパーサーの状態の型 `()` を指定しています。
`Parser` 型の引数 `a` は、パースした結果を表す型です。

`Parser` 型の後に定義している `symbol`, `reservedOp`, `natural` はそれぞれ、「記号列」「記号列（ただし後続の文字は記号ではない）」「自然数（符号のつかない整数）」をパースするパーサーです。
パースに成功した場合は、それぞれパースされた記号列やパースされた自然数を返します。
これらのパーサーは、parsec側で用意されているものを利用します。

その後の `atom` と `expr` が、自前で用意するパーサーです。

`atom` はカッコで囲われた式か、自然数をパースします。
`<|>` 演算子を使うことで、「複数のパーサーを順番に試し、最初に成功した結果を返す」という挙動を実現できます。
つまり、先に「カッコで囲われた式」のパースを試し、それが失敗したら自然数のパースを試します。

（細かいことを言うと、 `<|>` が後ろのパーサーを試すのは、先に試したパーサーが入力を消費せずに失敗した時に限ります。今回の状況では、`123` というような自然数の入力に対しては `symbol "("` は入力を消費せずに失敗するので、うまくいきます。）

`expr` は `atom` を組み合わせて四則演算をパースするパーサーです。
ここでは parsec が提供する `buildExpressionParser` という、演算子の優先順位と結合性を指定するだけでよきに計らってくれる便利な関数を使っています。
`buildExpressionParser` にはリストのリストを与えます。外側のリストは、先に与えた方が優先順位が高くなります。
つまり、 `*` と `/` の演算子の方が `+` と `-` よりも優先順位が高くなるようにしています。

さて、 `Parser` 型はモナドとなります。
モナドの `p >>= g` は、「先に `p` によるパースを試し、成功すれば `p` のパース結果（文字列や整数）を関数 `g` に渡す」と言う挙動になります。

## エラーの扱い

不正な入力を与えた場合はどうなるのでしょうか。
試しに、数式じゃないものを入れてみましょう。

```
Enter expression:
hello
"stdin" (line 1, column 1):
unexpected "h"
expecting "(" or natural
```

エラーとなったことがわかります。
この場合、 `parse` 関数は文字列の解釈に失敗し、エラーの内容を `Left` に包んで返します。
さっき書いたプログラムでは `parse` 関数から `Left` が返ってきた場合はその内容を `print err` で表示するので、プログラムを実行した我々にもエラーの内容がわかるようになっています。

一方で、まだ実装していない演算子を使った場合はどうなるでしょうか。
べき乗っぽい `2 ^ 3` を書いてみましょう。

```
Enter expression:
2 ^ 3
2
```

なんと、エラーが出るのでもなく、2の3乗が計算されるのでもなく、 `2` が出力されました。
これはどういうことでしょうか。

パーサーは `2` の解釈に成功しますが、その後の `^` の解釈に失敗します。
この場合、パーサーは解釈に成功した部分だけを返し、その後の解釈に失敗した部分は放置するのです。

入力を最後まで解釈させたい場合は、 `expr` パーサーの後に `eof` パーサーを実行します。
これによって、 `expr` によって入力を最後までパースできなかった場合はエラーとなります。

そのためには、 `Main.hs` に次のコードを書き加えて、

```haskell
wholeExpr :: Parser Integer
wholeExpr = do x <- expr
               eof
               return x
```

`parse expr "stdin" s` を `parse wholeExpr "stdin" s` に書き換えます。

再度実行してみると、

```
Enter expression:
2 ^ 4
"stdin" (line 1, column 3):
unexpected '^'
expecting operator or end of input
```

となって期待通りエラーとなることがわかります。

ついでに言うと、ここまでの実装では式の途中に空白があった場合は適宜読み飛ばしてくれますが、先頭に空白があった場合はエラーとなります。

```
Enter expression:
 1 + 1 
"stdin" (line 1, column 1):
unexpected " "
expecting "(" or natural
```

空白を読み飛ばすパーサー

```haskell
whiteSpace :: Parser ()
whiteSpace = TT.whiteSpace Lang.haskell
```

を追加し、 `wholeExpr` の先頭でこれを呼び出します。

```haskell
wholeExpr :: Parser Integer
wholeExpr = do whiteSpace
               x <- expr
               eof
               return x
```

実行例：

```
Enter expression:
 1 + 1 
2
```

先頭に空白があってもうまくいくようになりました。

## 単項マイナスの実装

前置の単項マイナスを実装してみましょう。
`buildExpressionParser` を使う場合は、 `Prefix` で前置演算子を定義します。

```haskell
expr :: Parser Integer
expr = buildExpressionParser
       [[binary "*" (*) AssocLeft, binary "/" div AssocLeft]
       ,[binary "+" (+) AssocLeft, binary "-" (-) AssocLeft, prefix "-" negate]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)
```

`fun <$ reservedOp name` は `reservedOp name >> return fun` と同じ意味です。

ここでは単項マイナスの優先順位を掛け算・割り算よりも低く設定したため、 `-2 * -3` みたいな入力はパースエラーとなります。

## べき乗の実装

べき乗も実装してみましょう。

べき乗は掛け算・割り算よりも優先順位を高くします。
つまり、`3 * 2 ^ 4` は `3 * (2 ^ 4)` と解釈され、 `3 ^ 2 * 4` は `(3 ^ 2) * 4` と解釈されます。

今まで実装した二項演算子は左結合（`2 * 3 * 4` が `(2 * 3) * 4` と解釈される）でしたが、べき乗の記号 `^` は右結合とすることが一般的です。
右結合にするには、 `Infix` に `AssocLeft` の代わりに `AssocRight` を渡します。

```haskell
expr :: Parser Integer
expr = buildExpressionParser
       [[binary "^" (^) AssocRight]
       ,[binary "*" (*) AssocLeft, binary "/" div AssocLeft]
       ,[binary "+" (+) AssocLeft, binary "-" (-) AssocLeft, prefix "-" negate]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)
```

## 階乗の実装

階乗には `Postfix` を使います。

```haskell
expr :: Parser Integer
expr = buildExpressionParser
       [[binary "^" (^) AssocRight, postfix "!" fact]
       ,[binary "*" (*) AssocLeft, binary "/" div AssocLeft]
       ,[binary "+" (+) AssocLeft, binary "-" (-) AssocLeft, prefix "-" negate]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)
    postfix name fun = Postfix (fun <$ reservedOp name)
```

優先順位を `^` と同じに設定しましたが、 `2 ^ 3 !` は `64` になりました。

## 抽象構文木の作成

プログラムのソースコードを抽象化して木構造として表したものを、**抽象構文木**（Abstract Syntax Tree, AST）と呼びます。
例えば `(1 + 2) * 3 * 4` は

```
Mul
├ Mul
│ ├ Add
│ │ ├ Const 1
│ │ └ Const 2
│ └ Const 3
└ Const 4
```

という風な木構造になります。
木構造になってしまえば、演算子の結合やカッコの有無を考える必要はなくなります。

Haskellの代数的データ型を使うと、抽象構文木を簡潔に表現できます。

```haskell
data Expr = Const Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Fact Expr
          | Negate Expr
          deriving (Eq, Show)
```

この `Expr` 型を使うと、先ほどの構文木は `Mul (Mul (Add (Const 1) (Const 2)) (Const 3)) (Const 4)` となります。

これまでのパーサーでは `Parser Integer` のようにパース結果は `Integer` でしたが、 `Parser Expr` と、抽象構文木を返すようにします。

```haskell
atom :: Parser Expr
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> (Const <$> natural)

fact :: Integer -> Integer
fact n | n < 0 = error "negative factorial"
       | otherwise = product [1..n]

expr :: Parser Expr
expr = buildExpressionParser
       [[binary "^" Pow AssocRight, postfix "!" fact]
       ,[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
       ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft, prefix "-" Negate]
       ]
       atom
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    prefix name fun = Prefix (fun <$ reservedOp name)
    postfix name fun = Postfix (fun <$ reservedOp name)

wholeExpr :: Parser Expr
wholeExpr = do whiteSpace
               x <- expr
               eof
               return x
```

この状態で実行してみると、

```
Enter expression:
(1 + 2) * 3 * 4
Mul (Mul (Add (Const 1) (Const 2)) (Const 3)) (Const 4)
```

と、値の代わりにASTが表示されるようになります。

本格的なプログラミング言語を作る場合は、抽象構文木を元にしてコンパイルや最適化を行いますが、今回はただの電卓なので、せいぜい「値を計算する」のが関の山でしょう。
`Expr` 型の値を計算するコードは次のようになります：

```haskell
eval :: Expr -> Integer
eval (Const x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Pow x y) = eval x ^ eval y
eval (Fact x) = fact (eval x)
eval (Negate x) = - eval x
```

main関数で、ASTと計算した値の両方を表示させるようにしてみましょう。

```haskell
main = do ...
          case parse wholeExpr "stdin" s of
            Left err -> print err
            Right x -> do print x
                          print (eval x)
```

実行例は次のようになります。

```
Enter expression:
(1 + 2) * 3 * 4
Mul (Mul (Add (Const 1) (Const 2)) (Const 3)) (Const 4)
36
```

## buildExpressionParserを使わない方法

ここまでは演算子の優先順位や結合は `buildExpressionParser` に丸投げしてきました。
ここでは `buildExpressionParser` を使わずに同様の機能を手書きしてみましょう。

```haskell
atom :: Parser Expr
atom = do symbol "("
          x <- expr
          symbol ")"
          return x
   <|> (Const <$> natural)

factor :: Parser Expr
factor = do a <- atom
            morePostfix a
  where
    morePostfix :: Expr -> Parser Expr
    morePostfix a = (reservedOp "^" >> Pow a <$> factor)
                <|> (reservedOp "!" >> morePostfix (Fact a))
                <|> return a

term :: Parser Expr
term = do a <- factor
          moreFactors a
  where
    moreFactors :: Expr -> Parser Expr
    moreFactors a = do reservedOp "*"
                       b <- factor
                       moreFactors (Mul a b)
                <|> do reservedOp "/"
                       b <- factor
                       moreFactors (Div a b)
                <|> return a

expr :: Parser Expr
expr = do a <- term
          xs <- many (do reservedOp "+"
                         b <- term
                         return (\x -> Add x b)
                      <|> do reservedOp "-"
                             b <- term
                             return (\x -> Sub x b)
                     )
          return $ foldl (flip ($)) a xs
```

`atom` は従来通りです。

`buildExpressionParser` を使っていた `expr` は、

* 中置 `^` と後置 `!` をパースする `factor`
* 中置 `*` と中置 `/` をパースする `term`
* 中置 `+` と中置 `-` をパースする `expr`

の3つに分割しました。
これらがそれぞれパースする文字列の例は

* `factor` は例えば `3^2!` を最後までパースし、 `3^2*4` の `3^2` までをパースする。
* `term` は例えば `3^2*4` を最後までパースし、 `3^2*4+2` の `3^2*4` までをパースする。
* `expr` は従来通り。

です。

`factor` は右結合の演算子をパースします。
`atom` をパースした後、 `^` または `!` が出現するか確認します。
`^` が出現したら再帰的に `factor` を呼び出してパースします。
`!` が出現したら、さっきパースした式に階乗をくっつけて、再び「`^` または `!` が出現するか」確認します。
いずれも出現しなかった場合は入力の終わりに達したか `*` や `+` などの優先順位の低い演算子に遭遇したということです。
その場合はこれ以上 `factor` でパースするものはないので、パース結果を返します。

`term` と `expr` はいずれも左結合の演算子をパースします。
ここで文法通りに

```haskell
term = do a <- term
          reservedOp "*"
          b <- factor
          return (Mul a b)
   <|> factor
```

と実装してしまうと、 `term` が無限再帰してしまいます。
そこで一工夫して、

```
f1 * f2 * f3 * ... * fn
```

という形の式（`f` は `factor`）を読み取ってから左結合

```
(...((f1 * f2) * f3) * ... * fn)
```

にします。
（もちろん、実際には `*` ではなく「`*` または `/`」「`+` または `-`」を使います）

実際のコードでは、 `factor` を一個読み取ってから、 `('*' | '/') factor` の繰り返しをパースしています。
