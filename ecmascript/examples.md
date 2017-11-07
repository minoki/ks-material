---
layout: page
title: プログラムの例
---

# ユークリッドの互除法

ユークリッドの互除法を使って最大公約数を求めるプログラムは次のようになります：
```javascript
function gcd(n,m) {
    while (m !== 0) {
        let k = n % m;
        n = m;
        m = k;
    }
    return n;
}
console.log(gcd(108,21)); // => 3
console.log(gcd(34,55)); // => 1
```

[分割代入 (destructuring assignment)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment) 構文を使うと、次のように書くこともできます：
```javascript
function gcd(n,m) {
    while (m !== 0) {
        [n, m] = [m, n % m];
    }
    return n;
}
```

あるいは、末尾再帰を使うと次のように書くこともできます：
```javascript
function gcd(n,m) {
    "use strict"; // ←対応している処理系で ECMAScript 6 の末尾再帰最適化がかかるようになるおまじない
    if (m === 0) {
        return n;
    }
    return gcd(m, n % m);
}
```
（処理系が「末尾呼び出しの最適化」を実装していればこれはループと等価になりますが、そうでない場合はループと比べて回数に制限がかかります。末尾呼び出しの最適化は ECMAScript 6 で定められていますが、2017年現在それを実装している処理系はほとんどありません。Node.js では起動時に `--harmony` オプションを指定すれば末尾呼び出しの最適化が有効になります。なお、 ECMAScript でまともに扱える整数の範囲ではユークリッドの互除法での繰り返しの回数は十分少ないので、この場合は末尾呼び出しがかからなくても問題はありません）

課題1：拡張されたユークリッドの互除法を実装してください。つまり、整数 n, m に対して p * n + q * m === r, r === gcd(n,m) となる [p,q,r] を返す関数 extEuclid(n,m) を実装してください。
```javascript
function extEuclid(n,m) {
   ???
}
console.log(extEuclid(34, 55)); // => [ -21, 13, 1 ]
console.log(extEuclid(108, 21)); // => [ 1, -5, 3 ]
```

# エラトステネスの篩

エラトステネスの篩を使って、100 以下の素数を列挙するプログラムです。a という名前の配列を作り、 a[i] が true なら i は素数、 false なら i は合成数となるようにしています。
```javascript
let a = [false, false];
for (let i = 2; i < 100; ++i) {
    a[i] = true;
}
for (let i = 2; i < a.length; ++i) {
    if (a[i]) {
        console.log(`${i} is a prime.`);
        // i の倍数は素数ではない
        for (let j = 2; j * i < a.length; ++j) {
            a[j * i] = false;
        }
    }
}
```

課題2：与えられた整数 n 以下の自然数について、上の例における a のような配列を返す関数 sieve(n) を実装してください。
```javascript
function sieve(n) {
   ???
}
console.log(sieve(10)); // => [ false, false, true, true, false, true, false, true, false, false ]
let a = sieve(100);
console.log(a[57] ? "57 is a prime!" : "57 is not a prime...");
```

課題3：整数 n が与えられた時、 n 以下の素数からなる配列を返す関数 primes(n) を実装してください。
```javascript
function primes(n) {
    ???
}
console.log(primes(10)); // => [ 2, 3, 5, 7 ]
let b = primes(200);
console.log(`200 以下の素数は ${b.length} 個あります`);
for (let p of b) {
    console.log(p);
}
```
