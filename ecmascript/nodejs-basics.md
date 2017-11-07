---
layout: page
title: Node.js の基礎
---

# モジュール

Node.js が提供する機能のほとんど、また、Node.js の開発元以外の第三者によって提供される機能は、「モジュール」という単位に分割されています。

ECMAScript が提供する Math オブジェクトや、 Node.js が提供する中でも console オブジェクトなどは、特別な処理をしなくてもプログラム中で使うことができますが、モジュールによって提供される機能は、使う前に読み込みが必要です。

モジュールを読み込むには require 関数を使います。fs モジュールを読み込むには、最初に次のように書きます：
```javascript
const fs = require("fs");
```

見ての通り、関数呼び出し `require("fs")` の結果を `fs` という名前の変数に代入するだけです。変数名は fs でなくても構いませんが、モジュール名と同じにしておくのが分かりやすくて良いでしょう。

モジュールもオブジェクトなので、モジュールの関数を呼び出す時はメソッド呼び出しと同じようにします：
```javascript
const fs = require("fs");
fs.existsSync("hoge");
```

モジュールを変数に代入しなくても、 require 呼び出しの結果に対して直接メソッド呼び出しをすることもできます（が、普通は変数に代入しましょう）：
```javascript
$ node
> require("os").cpus()
[ { model: 'Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz',
    speed: 2600,
    times:
     { user: 232213280,
       nice: 0,
       sys: 114305480,
       idle: 884267610,
       irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz',
    speed: 2600,
    times:
     { user: 92048390,
       nice: 0,
       sys: 39550910,
       idle: 1099181930,
       irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz',
    speed: 2600,
    times:
     { user: 230829350,
       nice: 0,
       sys: 89786630,
       idle: 910165440,
       irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz',
    speed: 2600,
    times:
     { user: 96048770,
       nice: 0,
       sys: 40968920,
       idle: 1093763140,
       irq: 0 } } ]
```
（この実行結果は筆者のマシンのものであり、実習用マシンでは異なる結果が出ます）

# イベント

（要執筆）
