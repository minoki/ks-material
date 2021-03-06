---
layout: page
title: 数の扱い：Node.js を電卓として使ってみる
---

まずは、Node.jsの対話環境で、簡単な計算をさせてみましょう。

四則演算として `+` `-` `*` `/` の記号が使えます。数の頭にマイナス記号 `-` をつけると、符号の反転になります。
```
> 3 + 4
7
> 3 - 4
-1
> 3 * 4
12
> 3 / 4
0.75
> - (3 / 4)
-0.75
```
このように、割り算の結果は小数になります。（整数同士の割り算だからと言って小数点以下が切り捨てられるということはありません）

整数を割った余りは、 `%` で求められます。（整数じゃない数にも使えますが）
```
> 11 % 4
3
> (-11) % 4
-3
```

数の大小比較は、 `<`, `>`, `<=`, `>=` を使います。等号付きの比較は、数学では下にイコールを書きますが、JavaScript（に限らず、多くのプログラミング言語）では後ろにイコールを書きます。
```
> 3 < 4
true
> 3 > 4
false
> 3 <= 4
true
> 3 >= 4
false
```

数が同じかどうか判定するには、 `===` および `!==` 演算子を使います。これらの演算子では、数に限らず、JavaScriptの値の等価性を判定できます。
```
> 1 === 1
true
> Math.pow(2,3) === 7
false
```
JavaScriptには3文字の `===` 演算子とは別に、2文字の `==` 演算子というのもありますが、これからJavaScriptを学ぶ諸君らは2文字の方を使う必要も覚える必要もありません。

数の累乗には、Math.pow 関数を使います。
```
> Math.pow(3, 4)
81
> Math.pow(2, 1/2)
1.4142135623730951
> Math.pow(0, 0)
1
```
ちなみに、ECMAScript 2016 では、Math.powと同じ動作をする、累乗演算子 `**` が追加されました。

平方根、指数、対数、三角関数等の数学関数は、 Math オブジェクトに用意されています。Math.ホニャララ という形で呼び出せます。
```
> Math.sqrt(2) // 平方根
1.4142135623730951
> Math.exp(1) // 指数関数
2.718281828459045
> Math.exp(-1)
0.36787944117144233
> Math.log(2) // 自然対数
0.6931471805599453
> Math.log10(2) // 常用対数
0.3010299956639812
> Math.log2(2048) // 底が 2 の対数
11
> Math.PI // 円周率（の近似）
3.141592653589793
> Math.sin(Math.PI / 2) // 正弦関数
1.2246467991473532e-16
> Math.cos(Math.PI) // 余弦関数
-1
> Math.atan(Infinity) * 2 // 逆三角関数
3.141592653589793
> Math.sinh(1) // 双曲線関数
1.1752011936438014
> Math.abs(-1.34) // 絶対値
1.34
```
Math オブジェクトの関数にはここに紹介していないものもありますが、割愛します。自分で調べてください。
なお、ここで紹介した関数のうち、 Math.log10 および Math.log2 と、双曲線関数（Math.sinh など）は、ECMAScript 6 で追加された新しい関数です。

# 数の表記

プログラム中に数を記述する場合、整数や小数を10進表記できるのはすでに見ました。
整数については、16進表記、2進表記、8進表記も使うことができます。それぞれ、接頭辞 `0x`, `0b`, `0o` を使います。（それぞれ he**_x_**adecimal, **_b_**inary, **_o_**ctal から1字ずつ取った）
```
> 0x10
16
> 0b10
2
> 0o10
8
```
（ちなみに、2進表記と8進表記は、ECMAScript 6 で追加された、比較的新しい機能です）

10進表記の場合、いわゆる指数表記をすることができます。
```
> 1.5e3 // 1.5 かける 10の3乗
1500
> 1.5E-3 // e は大文字でも可。また、指数部分には符号をつけられる
0.0015
```

なお、細かいことですが、整数部分が1桁でない限り10進表記の最初の文字を 0 とすることはできません。
```
> 0 // OK
0
> 0.1 // OK
0.1
> 10 // OK
10
> 010 // ？？？
```

# 文字列化
数値を文字列化するには toString メソッドを使います。メソッド呼び出しにはドットを使いますが、今回の例のように数値リテラルを使う場合は、小数点と紛らわしいのでカッコで囲っています。
```
> (42).toString()
'42'
> (1e50).toString()
'1e+50'
```

toString に引数を指定することによって、10進数以外の表記に変換することができます。
```
> (42).toString(2) // 2進数
'101010'
> (42).toString(16) // 16進数
'2a'
```

小数を変換する際、 toPrecision メソッドを使うと、精度を指定できます。toPrecision の引数に、精度として、1以上20以下の整数を指定できます。
```
> (42).toPrecision(1)
'1e+2'
> (123.45).toPrecision(10)
'123.4500000'
```
JavaScriptの数値の精度は10進数で16桁程度であり、それ以上の桁数を指定するとゴミが出てきます。
```
> (123.45).toPrecision(20)
'123.45000000000000284'
```

toFixed メソッドを使うと、小数点以下の桁数を指定することができます。
```
> (123.45).toFixed(10) // 小数点以下10桁
'123.4500000000'
```

toExponential メソッドを使うと、常に指数表記に変換できます。
```
> (123.45).toExponential(10) // 指数表記、小数点以下10桁
'1.2345000000e+2'
```

# 文字列からの変換

文字列を数値に変換するには parseInt 関数または parseFloat 関数を使います。

parseInt は整数表記の文字列を数値に変換します。
```
> parseInt("123")
123
> parseInt("0x100") // 16進数表記
256
> parseInt("-28")
-28
> parseInt("-0x37") // 16進数表記
-55
```
parseInt に第二引数を指定することにより、10進数以外の表記を扱えます。第二引数を省略した場合、または0を指定した場合は、原則として10進数として解釈されますが、文字列が `"0x"` または `"0X"` から始まる場合は16進数として解釈されます。
```
> parseInt("123", 8) // 8進数として解釈
83
> parseInt("-27", 16) // 16進数として解釈
-39
```

文字列の途中に数値として解釈できない文字があった場合は、それ以降の部分は無視されます。また、全く数値として解釈できない文字列だった場合は、 NaN が返ります。
```
> parseInt("3.1415")
3
> parseInt("1e10")
1
> parseInt("Hello")
NaN
```

小数の文字列を数値に変換したい場合は、 parseFloat 関数を使います。parseFloat 関数には第二引数はありません。
```
> parseFloat("2.718281828")
2.718281828
> parseFloat("1e10")
10000000000
```

# 発展：浮動小数点数

JavaScriptの数値は、精度53ビットの浮動小数点数です。演算結果を正確に表すことができない場合は、最近接丸めが行われます。特に、浮動小数点数の演算の性質として、和や積の結合則を満たしません。
```
> 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1
0.6
> 0.1 + 0.1 + 0.1 + (0.1 + 0.1 + 0.1)
0.6000000000000001
> 0.1 + 0.1 + 0.1 === 0.3
false
```

整数を扱う場合、 -2<sup>53</sup> から 2<sup>53</sup> までの整数は正確に扱うことができますが、その範囲を超えると正確に表現できなくなります。
```
> Math.pow(2,53)
9007199254740992
> Math.pow(2,53)+1
9007199254740992
> Math.pow(2,53)+2
9007199254740994
> Math.pow(2,53)+3
9007199254740996
```
`Math.pow(2,53)+2` や `Math.pow(2,53)+4` は正確に表すことができますが、 `Math.pow(2,53)+1` や `Math.pow(2,53)+3` は正確に表すことができないので、最近接丸めが行われています。

表せる数の最大値は 10<sup>308</sup> 程度です。それを超えると `Infinity` になります。
```
> 1e308
1e+308
> 1e309
Infinity
```

浮動小数点数についての話はECMAScriptに限った話ではないので、これ以上の詳しい話は他のところで勉強してください。
