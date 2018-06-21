---
layout: page
title: キャンバスに絵を描こう
---

Webページにcanvas要素を配置することで、その中に好きな図形を描画することができます。

例：
```html
<!DOCTYPE HTML>
<html>
  <head>
    <meta charset="utf-8">
    <title>お絵かき</title>
  </head>
  <body>
    <canvas id="mycanvas" width="300" height="300" style="border:1px solid black"></canvas>
    <script>
      let mycanvas = document.getElementById("mycanvas");
      let context = mycanvas.getContext("2d");

      // 長方形を描く
      context.fillStyle = "red";
      context.fillRect(10, 20, 200, 30); // x, y, width, height

      // 線分を描く
      context.strokeStyle = "blue";
      context.beginPath();
      context.moveTo(0, 200); // x, y
      context.lineTo(100, 300); // x, y
      context.stroke();

      // 円弧を描く
      context.strokeStyle = "green";
      context.beginPath();
      context.arc(200, 200, 30, 0, 2*Math.PI); // x, y, radius, startAngle, endAngle
      context.stroke();
    </script>
  </body>
</html>
```

解説：

* HTMLに記述したcanvas要素（IDは `"mycanvas"`）を、 `document.getElementById` 関数で取得します。
* `getContext` メソッドにより、canvas要素に2D描画を行うためのオブジェクト（レンダリングコンテキスト）を取得します。実際の描画操作はこのレンダリングコンテキストにより行います。
  * `getContext` メソッドに文字列 `"2d"` を与えると2D描画用のコンテキストが帰って来ます。WebGLという技術を使うとcanvas要素に3Dの描画ができますが、その場合は `"webgl"` という文字列を与えます。（WebGLについてはこのページでは扱いません）
* レンダリングコンテキストの `fillStyle`, `strokeStyle` プロパティーを使うことで、図形を描画した際に使用される塗りつぶしの方法、線の描画方法を指定できます。
* `fillRect` メソッドにより、長方形領域を塗りつぶせます。
* 次に曲線を描画してみましょう。一つの曲線を描画するために、「曲線を構築」してから「曲線を描画する」という手順を踏みます。
  * まず `beginPath` メソッドを使って、「構築中の曲線」をリセットします。
  * レンダリングコンテキストには、仮想的な「現在のペンの位置」が紐ついています。 `moveTo` メソッドにより、「現在のペンの位置」を指定できます。
  * `lineTo` メソッドにより、「現在のペンの位置」から、引数として与えた座標へ向かって線分を引く（構築中の曲線に、線分を新たに加える）ことができます。
  * `stroke` メソッドにより、構築中の曲線を描画します。
* 今度は円弧を描画してみましょう。円弧も曲線の一種なので、「曲線を構築」してから「描画する」という手順を踏みます。
  * まず `beginPath` メソッドを使って、「構築中の曲線」をリセットします。
  * `arc` メソッドを使って、構築中の曲線に円弧を追加します。
  * `stroke` メソッドにより、構築中の曲線を描画します。

問題：canvas要素の2D描画における、x軸、y軸の向きはどちら向きでしょうか。

問題：「円弧を描く」の部分の `context.beginPath()` 呼び出しを省略すると何が起こるでしょうか。

<https://developer.mozilla.org/ja/docs/Glossary/Canvas>

<!--演習問題：正多角形の描画-->

canvas要素を応用することによって、テトリスやリバーシ等の簡単な2Dゲームを作ることもできます。
