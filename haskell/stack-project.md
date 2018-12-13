---
layout: post
title: Stackでプロジェクト管理
---

`ghci` (`stack repl`) コマンドや `runghc` (`stack runghc`) コマンドを使うと、インタラクティブにHaskellコードを実行したり、ファイルに記述したコードをその場で実行できました。

一方、Haskellである程度の規模のプログラムを書くときは、Stackでプロジェクト管理すると便利です。
Stackのインストール方法は[トップページ](.)に書いたので省略します。

# 新規プロジェクトの作成

`stack new` コマンドを使って、 `hello` という名前のプロジェクトを作ってみましょう。

```sh
$ stack new hello
Downloading template "new-template" to create project "hello" in hello/ ...

...中略...

Selecting the best among 14 snapshots...

* Matches lts-12.22

Selected resolver: lts-12.22
Initialising configuration using resolver: lts-12.22
Total number of user packages considered: 1
Writing configuration to file: hello/stack.yaml
All done.

$ ls hello
ChangeLog.md README.md    app          package.yaml stack.yaml
LICENSE      Setup.hs     hello.cabal  src          test
$ cd hello
```

`lts-12.22` の部分はその時点で利用できる最新のバージョンが使われます。
`stack new` によって、 `hello/` ディレクトリ以下に次のようなファイルが作られました：

```
hello/
├── ChangeLog.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── hello.cabal
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

3 directories, 10 files
```

# プロジェクトの設定

プロジェクトの設定は `package.yaml` に記述されます。

```yaml
name:                hello
version:             0.1.0.0
github:              "githubuser/hello"
license:             BSD3
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2018 Author name here"

extra-source-files:
- README.md
- ChangeLog.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on GitHub at <https://github.com/minoki/hello#readme>

dependencies:
- base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  hello-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello

tests:
  hello-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello
```

最初の方にあるのは、プロジェクト名、バージョン、作者の情報などです。適宜書き換えておきましょう。

中盤の `dependencies:` セクションには、利用する外部パッケージの名前を記述します。

その後にある `library:` セクションにはこのプロジェクトに含まれるライブラリー（複数のプログラムで再利用できるコード）の情報を、 `executables:` セクションにはこのプロジェクトに含まれる実行ファイルの情報を記述します。
`executables:` セクションには複数の実行ファイルの情報を記述できます（初期状態では `hello-exe` のみ）。

`tests:` セクションはそのまんまで、テストが含まれるコード（初期状態では `Spec.hs`）の情報を記述します。

`package.yaml` の書き方について詳しいことは、**hpack**で検索してください。

# プロジェクトのビルドと実行

`stack build` コマンドによって、プロジェクトをビルドすることができます。

```sh
$ ls .
ChangeLog.md README.md    app          package.yaml stack.yaml
LICENSE      Setup.hs     hello.cabal  src          test
$ stack build
Building all executables for `hello' once. After a successful build of all of them, only specified executables will be rebuilt.
hello-0.1.0.0: configure (lib + exe)
Configuring hello-0.1.0.0...
...中略...
hello-0.1.0.0: copy/register
Installing library in .../hello/.stack-work/install/x86_64-osx/lts-12.22/8.4.4/lib/x86_64-osx-ghc-8.4.4/hello-0.1.0.0-CH2ARFUwJag5y9ns449U7A
Installing executable hello-exe in .../hello/.stack-work/install/x86_64-osx/lts-12.22/8.4.4/bin
Registering library for hello-0.1.0.0..
```

エラーもなく成功したようです（何もいじっていないのだから当然ですね）。

実行ファイル名は hello-exe ですが、ファイルの場所が `hello/.stack-work/install/x86_64-osx/lts-12.22/8.4.4/bin` という奥まったところにあり、直接実行するのは不便です。
Stackでビルドしたプログラムを実行する際には、 `stack exec` コマンドを使うと便利です。

```sh
$ stack exec hello-exe
someFunc
```

someFuncって何だよ！Hello worldじゃないのかよ！と突っ込みたくなるのは置いておいて、ともかく、何かしら実行されたようです。

# コードの編集

初期状態では、実行ファイル (`hello-exe`) のソースコードは `app/` 以下に、ライブラリーのソースコードは `src/` 以下に置かれています。
`app/Main.hs` を見てみましょう：

```haskell
module Main where

import Lib

main :: IO ()
main = someFunc
```

一方、 `src/Lib.hs` は次のようになっています：

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

さっき `hello-exe` を実行して `someFunc` という文字列が表示されたのは、 `src/Lib.hs` に `putStrLn "someFunc"` というコードが記述されていることが原因だったようです。

試しに、 `app/Main.hs` を次のように書き換えてみましょう：

```haskell
main = putStrLn "Hello world!"
```

`app/Main.hs` を上書き保存したら `stack build` を再度実行してビルド、 `stack exec` によって実行します。

```
$ stack build
hello-0.1.0.0: unregistering (local file changes: app/Main.hs)
hello-0.1.0.0: build (lib + exe)
...中略...
hello-0.1.0.0: copy/register
Installing library in .../hello/.stack-work/install/x86_64-osx/lts-12.22/8.4.4/lib/x86_64-osx-ghc-8.4.4/hello-0.1.0.0-CH2ARFUwJag5y9ns449U7A
Installing executable hello-exe in .../hello/.stack-work/install/x86_64-osx/lts-12.22/8.4.4/bin
Registering library for hello-0.1.0.0..
$ stack exec hello-exe
Hello world!
```

ちゃんと `Hello world!` というメッセージが表示されました。

# 外部パッケージの追加

TODO

# もっと知りたい方へ

「Haskell入門 関数型プログラミング言語の基礎と実践」にはStackによるプロジェクトの作成例が載っています。

Web上にも日本語で書かれたStackのチュートリアルが存在します：

- [Stackでやる最速Haskell Hello world! (GHCのインストール付き！) - Qiita](https://qiita.com/igrep/items/da1d8df6d40eb001a561)
- [本気で Haskell したい人向けの Stack チュートリアル - Qiita](https://qiita.com/waddlaw/items/49874f4cf9b680e4b015)
