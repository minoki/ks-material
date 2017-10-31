---
layout: page
title: 変数と関数
---

# let 文による変数の定義

計算した値に名前をつけて後で再利用することができます。このような変数を定義するには、 `let 〈変数名〉=〈初期値〉` という構文を使います。
```javascript
let x = Math.sqrt(123); // 右辺の値を x という名前の変数に保管する
console.log(x.toString()); // 変数に保管した値を使う
console.log(x.toPrecision(10)); // 保管した値をもう一度使う
x = x * 2; // 変数に値を代入し直す
console.log(x.toString()); // 変数 x の値が変化している
```

波かっこ `{ }` で囲った部分で定義した変数は、外側で定義したものとは別の変数になります。
```javascript
let x = 123;
console.log(x); // => 123
{
    let x = 456; // 外側の変数とは別
    console.log(x); // => 456
}
console.log(x); // => 123
```

ただし、内側で同じ名前の変数を定義する場合は、内側の let 文の前では外側の変数も内側の変数も参照できません。また、同じスコープでは同じ名前の変数を複数定義できません。
```javascript
let x = 123;
console.log(x); // => 123
{
    // console.log(x); // => エラー (ReferenceError: x is not defined)
    let x = 456;
    console.log(x); // => 456
    // let x = 789; // => エラー (SyntaxError: Identifier 'x' has already been declared)
}
console.log(x); // => 123
```

let 文は ECMAScript 6 で導入された新たな文法です。

## 発展：var 文による変数の定義

JavaScript には、変数を定義するのに let 文以外に var 文というものもあります。let 文の方が後発で var 文よりも洗練されているので、新しく JavaScript を勉強する人は var 文を使う必要はないでしょう。

let 文の場合は波かっこ `{ }` で新しいスコープが作られますが、 var 文は同じ関数内の変数は（ほぼ）全てスコープを共有します。

# 変数の更新

最初のコード例ですでに使っていますが、一度定義した変数の値を変更するには、代入演算子（イコール） `=` を使います。
```javascript
let x = 5;
console.log(x); // => 5
x = x + 1;
console.log(x); // => 6
```

代入の際、「元の変数の値に何か演算を施したもの」（上の例だと足し算）を使う場合は、複合代入演算子を使えます。演算子の直後に（空白を入れずに）イコールを書くと複合代入演算子になります。
```javascript
let x = 5;
console.log(x); // => 5
x += 1; // x = x + 1 と等価
console.log(x); // => 6
```
```javascript
let y = 4;
console.log(y); // => 4
y /= 2; // y = y / 2 と等価
console.log(y);
```

さらに、変数の値を「1だけ増やす」「1だけ減らす」という演算は頻繁に使われるので、専用の演算子（インクリメント演算子・デクリメント演算子）が用意されています。

インクリメント演算子は `++`, デクリメント演算子は `--` を使います。
```javascript
let x = 5;
console.log(x); // => 5
console.log(x++); // => 5 （更新前の値）
console.log(x); // => 6
console.log(++x); // => 7 （更新後の値）
```

インクリメント演算子を変数の前に書くと、式の値として更新前の値が帰ってきます。変数の後に書くと、更新後の値が帰ってきます。デクリメント演算子も同様です。
```javascript
let x = 6;
console.log(x); // => 6
console.log(x--); // => 6 （更新前の値）
console.log(x); // => 5
console.log(--x); // => 4 （更新後の値）
```

# 発展：定数（const 文）
let や var で宣言した変数は値を変更できます。逆に、値を変更しないと明示したい場合には const 文が使えます。
```javascript
// test-const.js
const x = Math.exp(1);
console.log(x.toString());
x = x * 2; // エラー (TypeError: Assignment to constant variable)
```

実行結果：
```shell
$ node test-const.js
2.718281828459045
ほにゃらら/test-const.js:3
x = x * 2;
  ^

TypeError: Assignment to constant variable.
    at Object.<anonymous> (ほにゃらら/test-const.js:3:3)
    at Module._compile (module.js:571:32)
    at Object.Module._extensions..js (module.js:580:10)
    at Module.load (module.js:488:32)
    at tryModuleLoad (module.js:447:12)
    at Function.Module._load (module.js:439:3)
    at Module.runMain (module.js:605:10)
    at run (bootstrap_node.js:423:7)
    at startup (bootstrap_node.js:147:9)
    at bootstrap_node.js:538:3
```

const 文は ECMAScript 6 で導入された新たな文法です。

# 関数

いくつかの値を受け取り、所定の処理を実行し、何かしらの結果を返すものを、関数と呼びます。数学で関数と言ったら、引数のみで値が決まるものですが、JavaScriptの関数は引数以外の要素（例：現在時刻）で結果が変わるものもあります。

関数定義の文法は
```
function 〈関数名〉(〈引数〉) {
〈処理〉
}
```
となります。〈処理〉の中で `return 〈式〉` という文が実行されると、その式の値が関数からの返り値となり、関数の実行が終了します。

例を見てみましょう。
```javascript
// 引数に1を加えたものを返す関数を、 succ という名前で定義する
function succ(a) {
  return a+1;
}
console.log(succ(3)); // 出力結果： 4
```
JavaScriptにおいては、関数も値の一種なので、変数に代入したり、関数に渡すことができます。
```javascript
// 続き
let f = succ;
console.log(f(4)); // 出力結果：5
console.log(succ); // 出力結果：[Function]
```

## 無名関数
上で定義した succ 関数は、名前付きの関数として定義していましたが、名前をつけずに、式の中に直接記述することもできます。この場合の文法は `(〈引数〉) => { 〈処理〉 }` （アロー表記）または `function(〈引数〉) { 〈処理〉 }` となります。

さっきと等価なプログラムを、無名関数表記で書くと、次のようになります：
```javascript
let succ = (a) => { return a+1; };
console.log(succ(3));
```
または
```javascript
let succ = function(a) { return a+1; };
console.log(succ(3));
```

処理の内容が `return 〈式〉` という形の文1つの場合は、アロー表記をさらに簡略化できます：
```javascript
let succ = (a) => a+1;
console.log(succ(3));
```

アロー表記は ECMAScript 6 で新しく導入された記法です。
