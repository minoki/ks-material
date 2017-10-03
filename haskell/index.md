---
layout: page
title: Haskell入門
---

# はじめに

[Haskell](https://www.haskell.org/)は、強い静的型付けを持つ純粋関数型プログラミング言語です。

# インストール

[Haskell Platform](https://www.haskell.org/platform/)をインストールすると、Haskellの処理系（GHC）や周辺ツールを一括で導入することができます。Ubuntuでは、以下のコマンドでHaskell Platformを導入できます。
```sh
$ sudo apt-get install haskell-platform
```

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

# おまけ

- [Haskellと圏](category.html)
  - Haskellのどの辺で圏論の概念が登場するのか、そしてどこが圏論から乖離しているのかをざっくり解説します。
  - 圏論を知らなくてもHaskellは使えますし、圏論を知っているからといって良いHaskellプログラムが書けるとも限らないので、あくまで「おまけ」です。

# この資料を読んで真面目にHaskellを学びたいと思った人へ

公式サイトの[Documentation](https://www.haskell.org/documentation)に、Haskellの様々な入門書、ドキュメントが紹介されています。ただし、これらの中にはプログラミング経験を前提とするものが多いので注意してください。

いくつかの本には日本語訳が存在します。

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)（2011年） 日本語訳が「[すごいHaskellたのしく学ぼう！](http://www.amazon.co.jp/%E3%81%99%E3%81%94%E3%81%84Haskell%E3%81%9F%E3%81%AE%E3%81%97%E3%81%8F%E5%AD%A6%E3%81%BC%E3%81%86-Miran-Lipova%C4%8Da/dp/4274068854)」として出版されています。原著はWebで無料で読めます。
- [Real World Haskell](http://book.realworldhaskell.org/)（2008年） 日本語訳が「[Real World Haskell——実戦で学ぶ関数型言語プログラミング](http://www.amazon.co.jp/Real-World-Haskell%E2%80%95%E5%AE%9F%E6%88%A6%E3%81%A7%E5%AD%A6%E3%81%B6%E9%96%A2%E6%95%B0%E5%9E%8B%E8%A8%80%E8%AA%9E%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0-Bryan-OSullivan/dp/4873114233)」として出版されています。原著はWebで無料で読めます。
- [Programming in Haskell](http://www.cs.nott.ac.uk/~gmh/book.html)（2007年） 日本語訳が「[プログラミングHaskell](http://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0Haskell-Graham-Hutton/dp/4274067815)」として出版されています。

文責：TA荒田
