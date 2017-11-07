---
layout: page
title: HTTPサーバーを立てる
---

http モジュールは、ファイルをダウンロードする他に、 HTTP サーバーを立てるための機能も提供しています。
```javascript
const http = require("http");

let server = http.createServer(); // http.Server クラスのインスタンスを作る

// リクエストが飛んできた時の動作を定義
server.on('request', (request, response) => {
    response.setHeader("Content-Type", "text/html; charset=utf-8");
    response.write("<!DOCTYPE html><html><head><title>Hello world!</title></head><body>Hello from Node.js!</body></html>\n");
    response.end();
});

server.listen(8000); // localhostの8000番ポート
console.log("Server is running at http://localhost:8000/");
```

上のスクリプトを実行したら、同じマシンのブラウザーで <http://localhost:8000/> を開いてみましょう。

HTTPサーバーを立てるには http.Server クラスのインスタンスを作り、 listen メソッドを呼びます。HTTPリクエストを処理するには、 http.Server クラスの 'request' イベントを捕捉します。

今回は、リクエストの内容（URL等）に関わらず同じHTMLを返しています。
