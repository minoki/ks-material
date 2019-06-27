---
layout: page
title: HTTPによってファイルをダウンロードする
---

Node.js の提供する http モジュールまたは https モジュールによって、Webのファイルをダウンロードしてみましょう。

マニュアル（英語）はここから参照できます： <https://nodejs.org/api/http.html>

HTTPでダウンロードする場合は http モジュールを、HTTPSでダウンロードする場合は https モジュールを使用します。

http(s) モジュールには色々な関数（やクラス）が定義されていますが、今回使うのは [http(s).get 関数](https://nodejs.org/api/https.html#https_https_get_options_callback)です。

今回は `https://sites.google.com/g.ecc.u-tokyo.ac.jp/kspage` の内容を取得してみます。先に全体のコードを見てみましょう：
```javascript
const https = require("https");
https.get("https://sites.google.com/g.ecc.u-tokyo.ac.jp/kspage", (response) => {
    let statusCode = response.statusCode; // HTTP ステータスコード
    if (statusCode !== 200) { // ステータスコードが 200 以外の場合はエラーを吐いて終了する
        console.error(`Request failed (status code: ${statusCode})`);
        response.resume();
        return;
    }
    response.setEncoding("utf8");
    let rawData = '';
    response.on('data', (chunk) => { rawData += chunk; }); // 受信中に 'data' イベントが発生する
    response.on('end', () => { // 受信終了
        console.log(rawData);
    });
}).on('error', (e) => {
    console.error(`Got error: ${e.message}`);
});
```
実行してみて、HTML が吐き出されたら成功です。

# 実習環境での注意

普通は上記のコードでうまく動作するはずですが、実習環境ではプロキシの関係で改変が必要になります。
https.get の第一引数を、文字列リテラル `"https://sites.google.com/g.ecc.u-tokyo.ac.jp/kspage"` からオブジェクトリテラル` {host:"cache.ks.prv",port:8080,path:"https://sites.google.com/g.ecc.u-tokyo.ac.jp/kspage"}` に変えてください。

# 解説

コードの内容をざっくり解説します。

まず、 https.get 関数ですが、第一引数には URL, 第二引数には取得結果を処理する関数を渡します。ここでは、第二引数の関数は無名関数 `(response) => { ... }` として、その場に中身を書いています。
https.get 関数は [http.ClientRequest オブジェクト](https://nodejs.org/api/http.html#http_class_http_clientrequest)を返します。ここでは、その http.ClientRequest オブジェクトの `'error'` イベントを捕捉して、エラー時にメッセージを出力するようにしています。

課題1：取得する URL のドメインが存在しない場合（例：`http://utmsks.example.com/`）にどのようなエラーが発生するか確かめよう。

次に、サーバーに接続できて、何かしらの応答が帰ってきた場合の処理を見てみましょう。つまり、 `(response) => { ... }` の中身です。

まず、HTTPのステータスコードを確認します。HTTPステータスコードは、引数で渡されたオブジェクト（このコード例では response という名前をつけている）の、 statusCode プロパティーに入っています。正常に取得できた場合はステータスコード 200 (OK) が帰ってくるはずです。失敗時には、400番台か500番台のステータスコードが帰ってきます。

課題2：取得する URL のファイルが存在しない場合（例：`https://sites.google.com/g.ecc.u-tokyo.ac.jp/hoge`）にどのようなエラーが発生するか確かめよう。それは課題1のエラーと同じ種類のエラーだろうか？また、この場合のHTTPステータスコードはいくつだろうか？

ステータスコードをチェックした後は、 respose オブジェクトの setEncoding メソッドを呼び出しています。ここでは、サーバーから送られてくるバイト列を UTF-8 で解釈することを指示しています。 response オブジェクトは [http.IncomingMessage クラス](https://nodejs.org/api/http.html#http_class_http_incomingmessage)のオブジェクトで、http.IncomingMessage オブジェクトは [Readable インターフェース](https://nodejs.org/api/stream.html#stream_class_stream_readable)を実装しているので、 Readable インターフェースの [setEncoding メソッド](https://nodejs.org/api/stream.html#stream_readable_setencoding_encoding)が使えるのです。

この段階では、 Web ページの内容はまだサーバーから届いていません。内容が届き次第、 response オブジェクトの `'data'` イベントが発生するので、 rawData 変数にくっつけていきます。内容が全て届いたら `'end'` イベントが発生するので、 rawData 変数の内容を出力しています。
