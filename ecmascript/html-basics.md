---
layout: page
title: HTML/DOM の基礎
---

これまではNode.js上でJavaScriptを動かしてきましたが、Webブラウザー上でのJavaScriptも扱っておきます。

# HTML と DOM

**HTML**は、Webページを記述するためのマークアップ言語です。

例：
```html
<!DOCTYPE HTML>
<html>
  <head>
    <meta charset="utf-8">
    <title>すごいWebページ</title>
  </head>
  <body>
    <p id="sugoi">すごーい！</p>
  </body>
</html>
```

`< >` で囲まれた部分は、**タグ**と呼ばれ、何らかの機能を持ちます。
それぞれのタグは、名前と、いくつかの属性を持ちます。
名前の前にスラッシュが置かれているタグは閉じタグと呼ばれ、同名のタグ（開始タグ）の内容の終わりを示します。
開始タグと閉じタグの間は内容であり、内容としてはいくつかのタグとテキストを入れることができます。
タグと内容をひっくるめて要素と呼びます。

上記のHTML文書を解釈すると、次のような木構造となります：

* html
  * head
    * meta （属性：`charset="utf-8"`）
    * title
      * テキスト：すごいWebページ
  * body
    * p （属性：`id="sugoi"`）
      * テキスト：すごーい！

このような木構造をJavaScriptなどのプログラミング言語を扱えるようにしたものを、**DOM** (Document Object Model) と呼びます。

JavaScriptからHTMLの内容を書き換える際には、HTMLソースの文字列を直接書き換える方法と、木構造であるDOMを操作する方法があります。

## script 要素

HTML文書にJavaScriptを埋め込むには、scriptという名前のタグを使います。

例：
```html
<!DOCTYPE HTML>
<html>
  <head>
    <meta charset="utf-8">
    <title>すごいWebページ</title>
  </head>
  <body>
    <p id="sugoi">すごーい！</p>
    <script>
      console.log("Hello world!");
    </script>
  </body>
</html>
```

この内容を hello.html という名前で保存し、Webブラウザーで開きます。
次に、そのWebブラウザーのJavaScriptコンソールを開きます：

* Google Chrome: メニューから「JavaScriptコンソール」を開く

* Mozilla Firefox: 「ツール」＞「ウェブ開発」＞「ウェブコンソール」を開く

* Safari: メニューの「開発」＞「JavaScriptコンソールを表示」。メニューバーに「開発」がなかったら、「環境設定」＞「詳細」＞「メニューバーに“開発”メニューを表示」にチェックを入れます。

* Microsoft Edge:

開いたコンソールに「Hello world!」と表示されていれば成功です。

演習問題：コンソールに「Goodbye world!」を表示するJavaScriptを含むHTML文書を作ってみましょう。

演習問題：コンソールに100以下の素数を表示するJavaScriptを含むHTML文書を作ってみましょう。

# 組み込みオブジェクト

Webブラウザー上で動くJavaScriptでは、ECMAScript標準で定められていたもの以外にも、いくつかのグローバル変数が定義されています。

## window オブジェクト

window オブジェクトには、ページの表示や、ブラウザーのウィンドウに関するプロパティーやメソッドが定義されています。

window.alert メソッドは、警告メッセージを表示します。

例：
```javascript
window.alert("Hello world!");
```

window.prompt メソッドは、ユーザーに文字列の入力を求めます。

例：
```javascript
var haiku = prompt("ハイクを詠め：");
console.log(haiku);
```

window.confirm メソッドは、 OK か キャンセル かをユーザーに選択させます。
結果は `true` / `false` で帰ってきます。

例：
```javascript
var is_the_order_a_rabbit = confirm("Is the order a rabbit?");
if (is_the_order_a_rabbit) {
    console.log("The order is a rabbit.");
} else {
    console.log("The order is not a rabbit.");
}
```

alert, prompt, confirm 等のメソッドは、ユーザーの操作を中断するので、使用には注意しましょう。

実は、これらの関数は `window.` をつけなくても呼び出すことができます：
```javascript
alert("Hello world!");
var haiku = prompt("ハイクを詠め：");
var is_the_order_a_rabbit = confirm("Is the order a rabbit?");
```

これは window オブジェクトが**グローバルオブジェクト**であるのが理由です。
グローバルスコープに `var` 定義した変数は window オブジェクトのプロパティーとして見え、逆に window オブジェクトのプロパティーはグローバル変数として見えます：

```javascript
// 定義したグローバル変数が…
var Foo = 123;

// window オブジェクトのプロパティーとして見える。
console.log(window.Foo);

// window オブジェクトのプロパティーが…
window.Bar = 42;

// グローバル変数として見える。
console.log(Bar);
```

グローバルオブジェクトが `window` という名前なのは Web ブラウザー特有で、 Web ブラウザー以外の実行環境（Node.js など）では、グローバルオブジェクトが別の名前だったりします。

## document オブジェクト

document オブジェクトは、主に文書の内容を操作するためのメソッド等を持ちます。

```javascript
let n = document.createTextNode("Goodbye world!");
let e = document.createElement("p");
e.appendChild(n);
document.body.appendChild(e);
```

## その他のオブジェクト

### navigator

ページを開いているWebブラウザーに関する情報を取得できます。

よく使われるのは userAgent プロパティーで、Webブラウザーの種類やバージョン、動いているOS等の情報が含まれます。

userAgent プロパティー：
```javascript
console.log(navigator.userAgent);
```

### location

現在表示しているWebページのURLを取得、設定できます。

href プロパティー：
```javascript
console.log(location.href);
```

### console

すでに使っていますが、 console オブジェクトによってJavaScriptコンソールに色々と表示することができます。

```javascript
console.log("Hello world!");
console.info("JavaScriptを実行中です");
console.warn("警告：進捗がありません");
console.error("エラー：何もしていないのにパソコンが壊れた");
console.assert(Math.PI > 3.14); // 注：ある式が成り立つことをチェックすることをプログラミングでは assert と言う。その式が成り立たない場合はプログラムのバグであると見なして、エラーを表示したり強制終了したりすることが多い
```

# イベント

ユーザーがWebページに対して何か操作した時に、JavaScriptで処理を実行できると便利です。
そのために、イベントという仕組みが提供されています。

まず、捕捉したいイベントの名前と、イベントが発生した時に実行される関数（イベントハンドラー）を登録します。

イベントハンドラーを登録するには、イベントを捕捉する対象に関して addEventListener メソッドを呼びます。
最初の引数はイベント名、第2引数はイベントハンドラーで、第3引数（省略可能）は通常は false を指定します。

例として、ページ上でのマウスのクリックを捕捉してみましょう：
```javascript
let onclick_handler = () => {
    console.log(`clicked!`);
};
window.addEventListener("click", onclick_handler, false);
```

ページをクリックした時に、コンソールに clicked! と表示されれば成功です。

イベントハンドラーには引数としてイベントオブジェクトが渡されます。
イベントオブジェクトからは、発生したイベントに関するより詳しい情報を取得できます。
例えば、 click イベントに関するイベントオブジェクトからは、マウスをクリックした位置を取得できます。

さっき書いたコードを、次のように書き換えてみましょう：
```javascript
let onclick_handler = (event) => {
    console.log(`clicked! x:${event.clientX} y:${event.clientY}`);
};
window.addEventListener("click", onclick_handler, false);
```

クリックする位置によって、表示される数字が変わることがわかるでしょうか？

問題：click イベントのイベントオブジェクトの、 clientX と clientY プロパティーの原点はどこか。座標軸はどの向きか。
