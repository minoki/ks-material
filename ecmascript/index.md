---
layout: page
title: JavaScript (ECMAScript) 入門
---

# JavaScript と ECMAScript について

JavaScriptは、Webページで動作することを目的として作られた言語です。それをEcma Internationalという団体が標準化したものがECMAScriptです。両者はほぼ同じものと考えて差し支えありません。

現在では、JavaScript (ECMAScript) は、Webページだけではなく、デスクトップアプリ、モバイルアプリや、Webサーバー等をも動かせる汎用プログラミング言語として使われています。

# 規格のバージョン

ECMAScriptの規格は、1999年に出たECMAScript 3を最後に長らく停滞していました。しかし、ゼロ年代にAjaxだの何だのでJavaScriptの重要性が増した結果、言語としても進化が必要だということになり、2009年にECMAScript 5が制定されました。その後継となるECMAScript 6はECMAScript 2015として2015年に制定され、以降は毎年規格を改定していくことになりました。

定義にこだわる数学科の諸君は、言語の習得にあたって、その規格書を参照したいと思われるかもしれません。最新のECMAScriptの規格は、以下のページで閲覧・ダウンロードできます。（2017年4月の段階では、最新版は2016年のEdition 7）
<https://www.ecma-international.org/publications/standards/Ecma-262.htm>

ブラウザー等による実装状況についてはややばらつきがありますが、現代の全てのブラウザーはECMAScript 5を実装しています。さらに、今はもう2017年なので、最新のブラウザー等ではまず間違いなくECMAScript 6が使えます。そこで、ここではECMAScript 6に基づいて解説することにします（ただし、ECMAScript 6で追加された「新しい」機能については、なるべく注釈を入れるようにします）。

機能ごとにブラウザーでの対応状況を調べたい場合は、次のページが参考になります：

- <https://kangax.github.io/compat-table/es6/>

# Node.js について

先述の通り、JavaScriptはWebページ上で実行されることを念頭に進化してきた言語です。しかし、汎用プログラミング言語としてWebブラウザーの外でも実行したいという需要もあり、そのための実行環境がNode.jsです。

JavaScriptに入門する際にブラウザーを実行環境としてももちろん良いのですが、その場合はHTMLやDOM等の「余分な」概念も扱わないといけません。言語の習得にあたっては覚えることは最小限の方がいいだろうという考えで、ここではNode.jsを使うことにします。

Ubuntuでは、次の手順で Node.js （執筆時点での最新版である、 8 系）をインストールします：

```sh
$ export https_proxy=cache.ks.prv:8080
$ curl -Lo node_setup_8.x.sh https://deb.nodesource.com/setup_8.x
$ sudo -E bash node_setup_8.x.sh
$ sudo apt install nodejs
```

参考：[Node.js 公式のインストール手順](https://nodejs.org/ja/download/package-manager/#debian-and-ubuntu-based-linux-distributions-debian-ubuntu-linux)、[NodeSource によるインストール手順](https://github.com/nodesource/distributions#installation-instructions)

注：[配布元の手順](https://github.com/nodesource/distributions#installation-instructions)では、 curl コマンドで取得したシェルスクリプトを直接 sudo bash で実行していますが、このページに書いた手順では、実行するスクリプトの内容を確認できるように、 node_setup_8.x.sh という名前のファイルに一旦保存しています。

注：最初の三行を飛ばして `$ sudo apt install nodejs` だけ実行しても Node.js はインストールされますが、バージョンが古いものが入ってしまいます（最初の二行を飛ばした場合に入るのはバージョン 4.7 系で、使いたい最新バージョンは 8 系）。

無事にインストールが済めば、 `node` というコマンドでNode.jsがインストールされると思います。

# はじめの一歩

次のコードを hello.js という名前で保存します。
```js
console.log("Hello world!");
```

ターミナルで次のように実行します：
```sh
$ node hello.js
Hello world!
```

console.log は、標準出力（この場合はターミナル）に文字列を出力するための関数です。厳密にいうと、console という名前のオブジェクトがあり、そこに所属する log という名前の関数（メソッド）を呼び出しています。

JavaScriptにおける文字列リテラルは `""` (double quotation) で囲みます。

注： console オブジェクトは ECMAScript 標準で定められたものではなく、Node.jsが独自に用意しているものです。この関数の元ネタは、ブラウザー上でJavaScriptをデバッグするために用意されている同名の関数です。

課題：ターミナルに “Welcome to the Hell!” と出力する JavaScript プログラムを書いてください。

ターミナルで、ファイル名を与えずに単独で node を実行した場合、対話モードが起動します。対話モードでは、一行ごとにコードを入力し、結果を見ることができます。
```
$ node
> 1+1
2
> (Ctrl-D を押して終了)
```
node の起動後に表示される `> ` はコードの入力を受け付けるという意味なので、コード（この場合は `1+1`）を入力します。Ctrl-D を押すと入力の終わりとなって、 node が終了します。

# 次のステップ

必ずしも順番に読む必要はなく、わからないところは読み飛ばして先に進んでも構いません。

今後のコード例（実行例）で、行頭に `> ` がついているものは対話モードの実行例なので、対話モードで実行してください。そうでない場合は、コードをファイルに保存して実行してください。また、スラッシュ2つに続く内容はコメントなので、入力しなくても構いません。

## 基礎編

1. [数の扱い：Node.js を電卓として使ってみる](number.html)
2. [文字列処理](string.html)
3. [変数と関数](variables-and-functions.html)
4. [制御構造](control-structure.html)
5. [配列](array.html)
6. [オブジェクト](object.html)
7. [値と演算のまとめ](values-and-operators.html)
8. [プログラムの例](examples.html)

## Node.js 編

Node.js が提供する機能を使ってみましょう。

- [Node.js の基礎](nodejs-basics.html)
- ファイルの読み書きをする
- [HTTPによってファイルをダウンロードする](node-http-client.html)
- [HTTPサーバーを立てる](node-http-server.html)

## HTML 編

Web ブラウザーで JavaScript を動かしてみましょう。

- [HTML/DOM の基礎](html-basics.html)

# もっと勉強したい方へ

もっと勉強したい方向けに、Mozilla Developer Network (MDN) の JavaScript のページを紹介しておきます：<https://developer.mozilla.org/ja/docs/Web/JavaScript>

Node.js が提供する機能をもっと知りたい方は、Node.js の API マニュアルを参照すると良いでしょう： <https://nodejs.org/api/>

ほかにも、JavaScript入門を銘打つWebサイトや本屋に並んでいる参考書など、いろいろ資料はあると思います。その場合は、以下の点に注意してください：

- 対象とする規格（ECMAScript のバージョン）
    - ここのページでは ECMAScript 6 を対象としていますが、世間的な解説記事においてはまだまだ ECMAScript 5 かそれ以前の古い規格が主流であると思われます。
    - ECMAScript 5 （またはそれ以前）と ECMAScript 6 では、変数宣言の方法から無名関数の表記まで、書き方が大きく異なるものがあり、混乱するかもしれません。（古い書き方が ECMAScript 6 で使えないわけではありませんが、古い書き方には何かしら罠があって新しい書き方の方が優れているので、新しい書き方を使うべきです。）

- Webブラウザーか Node.js か
    - ここのページでは実行環境として Node.js を想定していますが、世間の解説記事ではWebブラウザーで JavaScript を実行することを念頭に置いているものが多いと思われます。
    - Webブラウザーを使って、Webページ上で JavaScript を動作させる場合は、HTML や DOM (Document Object Model) も関わってきます。圧倒されないように頑張りましょう。

文責：TA荒田
