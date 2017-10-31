---
layout: page
title: オブジェクト
---

いくつかの値と名前（キー）の組をまとめたものを、オブジェクトと呼びます。
波かっこ `{ }` の中に、`〈キー〉: 〈値〉` をカンマ区切りで並べること（オブジェクトリテラル）によって、指定したキーおよび値を持つオブジェクトを作ることができます。

例：
```javascript
> let a = {x: 123, y: 456}; // {"x": 123, "y": 456} や {"y": 456, x: 123} と書いても同じ
undefined
> a.x
123
> a["y"]
456
```

# 値の参照と更新

上記の実行例のように、ドット `.` または角かっこ `[ ]` で、紐つけた値を参照、設定することができます。ドット `.` 演算子では、キーを JavaScript の識別子として書くことができます。一方、角かっこ `[ ]` 演算子の場合はキーとして任意の式を指定できます。

オブジェクトが指定したキーを持つかどうかは、 in 演算子を使って調べられます。
```javascript
（続き）
> "x" in a
true
> "sqrt" in Math
true
```

存在しないキーを取得しようとした場合は、undefinedが返ります。
```javascript
（続き）
> a.z
undefined
> a[100]
undefined
```

# キーの列挙

Object.keys 関数で、オブジェクトの持つキーの一覧を、配列として取得できます。
```javascript
（続き）
> Object.keys(a) // => [ 'x', 'y' ] または [ 'y', 'x' ]
[ 'x', 'y' ]
```

# メソッド

（要執筆）

```javascript
let a = {x: 123, y: 456, abs: function() { return Math.hypot(this.x, this.y); }};
```
オブジェクトには、キーに紐ついた値として関数を指定できます。

# プロパティー

（要執筆）
get / set
Object.defineProperty

# プロトタイプ

（要執筆）
Object.create
Object.getPrototypeOf

# new 演算子

（要執筆）

# JSON（ジェイソン; JavaScript Object Notation）
JavaScript のオブジェクトの記法は、（JavaScript プログラムとは独立して）データの記法としても使われています。

JSON で使えるのは、

`null`, `true` / `false`, 数値、文字列、配列、オブジェクト

のリテラル表記ですが、実際の JavaScript の表記よりも制限されています。例えば、オブジェクトのキーは識別子ではなく文字列リテラルとして与えなければなりません。

JSON文字列をJavaScriptのオブジェクトに変換するには、JSON.parse関数を使います。逆に、JavaScriptのオブジェクトをJSON文字列に変換するには、JSON.stringify関数を使います。
```javascript
> JSON.parse(`{"x": 123, "y": 456}`)
{ x: 123, y: 456 }
> JSON.stringify({x: 123, y: 456})
'{"x":123,"y":456}'
```
