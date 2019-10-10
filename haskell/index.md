---
layout: page
title: Haskell入門
---

# はじめに

[Haskell](https://www.haskell.org/)は、強い静的型付けを持つ純粋関数型プログラミング言語です。

# インストール

Haskellの環境は、Haskell PlatformまたはHaskell Stackを使って導入します。
<!-- 最近だと ghcup もあり。ただし Unix 系のみ。 -->

## Haskell Platform

[Haskell Platform](https://www.haskell.org/platform/)をインストールすると、Haskellの処理系（GHC）や周辺ツールを一括で導入することができます。Ubuntuでは、以下のコマンドでHaskell Platformを導入できます。

```sh
$ sudo apt-get install haskell-platform
```

ただし、この方法で入るHaskell処理系（GHC）は**やや古い**ので、最新のHaskellを使いたい場合は後述するHaskell Stackを導入しましょう。

## Haskell Stackのインストール

[Haskell Stack](https://docs.haskellstack.org/en/stable/README/)のインストール手順は次のようになります：

1. apt を使って `haskell-stack` をインストールする。これによって**やや古い** `stack` コマンドが `/usr/bin/stack` にインストールされる。
2. `stack upgrade` を実行する。これによって**最新版の** `stack` が `/home/<user>/.local/bin/stack` にインストールされる。（`<user>` はユーザー名）
3. `.bashrc` か `.bash_profile` を編集し、
   `export PATH=$HOME/.local/bin:$PATH`
   という行を追加する。
4. `source .bashrc` なり `source .bash_profile` を実行し、 `$PATH` の設定を反映させる。
5. `stack` コマンドで最新の `stack` が起動することを確認する。

コマンド列で書くと次のようになります：

```sh
$ sudo apt install haskell-stack
$ which stack
/usr/bin/stack
$ export https_proxy=http://cache.ks.prv:8080
$ stack upgrade
Current Stack version: 1.5.1, available download version: 1.9.1
...

WARNING: Installation path /home/<user>/.local/bin not found on the PATH environment variable
New stack executable available at /home/<user>/.local/bin/stack
$ nano .bashrc
（最後の方に export PATH=$HOME/.local/bin:$PATH を追加する）
$ source .bashrc
$ which stack
/home/<user>/.local/bin/stack
$ stack --version
Version 1.9.1, Git revesion ...
```

（執筆時点で、aptで入るstackのバージョンは1.5.1, 最新のstackのバージョンは1.9.1でした。）

この状態では、Haskellの処理系であるGHCはまだインストールされていません。
本来のstackの使い方としては、プロジェクトごとにGHCのバージョンを指定し、プロジェクトで必要なGHCをインストールします。
ですが、プロジェクトを作らずに最新のGHCを適当にインストールして使うこともできます (global project)。

```sh
$ stack setup
Writing implicit global project config file to: /home/<user>/.stack/global-project/stack.yaml
Note: You can change the snapshot via the resolver field there.
Using latest snapshot resolver: lts-12.20
Downloaded lts-12.20 build plan.
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-8.4.4.
Installed GHC.
...
```

執筆時点では、GHC 8.4.4がインストールされました。

Haskell Platformでは `ghc` や `ghci`, `runghc` という名前でGHCのコマンドが使えますが、Haskell Stackでは `stack ghc` や `stack ghci`, `stack runghc` という風に、 `stack` を冠してGHCのコマンドを使います（`stack ghci` の別名として `stack repl` を使うこともできます）。

以下、解説記事での各コマンドは、 `stack` を冠したコマンド名に適宜読み替えてください。

# 本編

ここのページは自己完結的な入門には程遠いので、他のWebサイトや書籍なども適宜参照することをお勧めします。

- [入門編](intro.html)
  - Haskellを電卓代わりに使ってみましょう。
- [リストと文字列](list.html)
- [型と関数](types-and-functions.html)
- [入出力入門](io.html)
  - ユーザーに入力を求めたり、ファイルを読み書きしたりするプログラムを書きましょう。
- [データ型とパターンマッチ](data-types.html)
- [型クラス](type-classes.html)
  - Haskell最大の特徴の一つである、型クラスについて解説します。
- [モナド](monad.html)
  - Haskellでは入出力処理のためにモナドが使われます。ここでは、実用的な側面からモナドをゆる〜く解説します。圏論の知識は使いません。
- [Stackでプロジェクト管理](stack-project.html)

# おまけ

- [Haskellと圏](category.html)
  - Haskellのどの辺で圏論の概念が登場するのか、そしてどこが圏論から乖離しているのかをざっくり解説します。
  - 圏論を知らなくてもHaskellは使えますし、圏論を知っているからといって良いHaskellプログラムが書けるとも限らないので、あくまで「おまけ」です。
- [Haskellで競技プログラミング](competitive.html)
  - AtCoder等の問題を解くのに必須な、「標準入力から読み取る」処理などを解説します。

# この資料を読んで真面目にHaskellを学びたいと思った人へ

公式サイトの[Documentation](https://www.haskell.org/documentation)に、Haskellの様々な入門書、ドキュメントが紹介されています。ただし、これらの中にはプログラミング経験を前提とするものが多いので注意してください。

いくつかの本には日本語訳が存在します。

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)（2011年） 日本語訳が「[すごいHaskellたのしく学ぼう！](https://www.ohmsha.co.jp/book/9784274068850/)」として出版されています。原著はWebで無料で読めます。
    - 内容が若干古くなっているので、 [2017年に「すごいHaskellたのしく学ぼう」を読む](https://qiita.com/Aruneko/items/e72f7c6ee49159751cba) も併せて読んでおきましょう。
- [Real World Haskell](http://book.realworldhaskell.org/)（2008年） 日本語訳が「[Real World Haskell——実戦で学ぶ関数型言語プログラミング](https://www.oreilly.co.jp/books/9784873114231/)」として出版されています。原著はWebで無料で読めます。
- [Programming in Haskell](http://www.cs.nott.ac.uk/~gmh/book.html)（2007年） 日本語訳が「[プログラミングHaskell](https://www.ohmsha.co.jp/book/9784274067815/)」として出版されています。[第2版の邦訳](https://www.lambdanote.com/collections/haskell)も2019年に出ています。

最近のHaskell事情（例：Haskell Stackの登場）に対応した日本語の書籍も出ています：

- [Haskell入門 関数型プログラミング言語の基礎と実践](http://gihyo.jp/book/2017/978-4-7741-9237-6) （2017年）
- [Haskell 教養としての関数型プログラミング](http://www.shuwasystem.co.jp/products/7980html/4806.html) （2017年）
- [［増補改訂］関数プログラミング実践入門](http://gihyo.jp/book/2016/978-4-7741-8390-9) （2016年）

文責：TA荒田
