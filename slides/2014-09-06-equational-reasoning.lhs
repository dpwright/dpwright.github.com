---
date: 2014-09-06 15:00:00
tags: functional-programming, equational-reasoning, haskell, talks, 日本語
slides: SlidySlides
theme: plain
duration: 50
title: 等式推論
subtitle: 最適化と証明
---

自己紹介
--------

- <i class="fa fa-twitter"></i> [\@tataminomusi][tataminomusi] （日本語）
- <i class="fa fa-twitter"></i> [\@danielpwright][danielpwright] （英語）
- <i class="fa fa-github"></i>  [dpwright][github]
- 京都のゲーム会社、VITEIのリードプログラマー
- 基本的にC++、でもツール等は多言語
    - ビルドシステムはRubyとNinja
    - MayaプラグインはPython
    - コンパイラはHaskell
    - C#のツールもあります
- ホントはHaskellでゲームを作ってみたいけどまだまだ難しい

このセッション
--------------

. . .

### どの言語に向いてる？

- 💖 Haskell 💕
- 依存型言語：Agda, Idris, Coq...
- ML系：SML, Ocaml, F#...
- Scala？
- Lisp系？
- C++/Java/Pascal/BASIC...?!

<div class="notes">
大事な機能は：

- 参照透過性
- 置換モデル
- 純粋関数
- シンタックス
</div>

### どのレベル？

文芸的プログラミング
--------------------

このスライド自体をコンパイルして実行できる！

. . .

Haskellの基本関数を再定義するつもりなんで、とりあえず隠さないと

> import Prelude hiding (sum, foldr, sequence_, replicate, take, repeat, (>>))

等式推論 {.titlepage}
--------

. . .

### は、いったい何ですか？！

例　変数置換 
------------

基本的に、中学校で代数学を勉強したときの変数置換と近い

. . .

$$
\begin{align}
x &= 49       \\
y &= x + 1000 \\
              \\
y &= 1049
\end{align}
$$

例　連立方程式
--------------

$$
\begin{align}
2x + y &= 4 \\
x - y  &= -1
\end{align}
$$

方程式を組み替えたら同じ方法で解けます

. . .

$$
\begin{align}
y &= 4 - 2x \\
x &= y - 1
\end{align}
$$

例　連立方程式
--------------

+-----------------------+-----------------------+
| $$                    | $$                    |
| \begin{align}         | \begin{align}         |
| x  &= y - 1        \\ | y  &= 4 - 2x       \\ |
|    &= (4 - 2x) - 1 \\ |    &= 4 - 2(y - 1) \\ |
|    &= 3 - 2x       \\ |    &= 4 - 2y + 2   \\ |
| 3x &= 3            \\ | 3y &= 6            \\ |
| x  &= 1            \\ | y  &= 2               |
| \end{align}           | \end{align}           |
| $$                    | $$                    |
+-----------------------+-----------------------+

例　数学的証明
--------------

「黄金数」と呼ばれる　$\phi = \frac{1 + \sqrt{5}}{2}$

$\phi^2 = \phi + 1$　を証明せよ

例　数学的証明
--------------

$$
\begin{align}
\phi^2 &= \frac{1 + \sqrt{5}}{2} \times \frac{1 + \sqrt{5}}{2} \\
       &= \frac{(1 + \sqrt{5})(1 + \sqrt{5})}{4}               \\
       &= \frac{1 + \sqrt{5} + \sqrt{5} + \sqrt{5}^2}{4}       \\
       &= \frac{6 + 2(\sqrt{5})}{4}                            \\
       &= \frac{3 + \sqrt{5}}{2} = \frac{2}{2} + \frac{1 + \sqrt{5}}{2} \\
       &= 1 + \phi
\end{align}
$$

数学はもうええ。。。 {.titlepage}
-------------------

### コードを見せよ！

簡単な例
--------

Haskell Reportによると、`foldr`はこんな感じ

> foldr f z []     = z
> foldr f z (x:xs) = f x (foldr f z xs)

`foldr (+) 0 [1, 2, 3, 4]` ≡ `1 + 2 + 3 + 4` を証明してみよ

<div class="notes">
```haskell
foldr (+) 0 [1, 2, 3, 4] = (+) 1 (foldr (+) 0 [2, 3, 4])
                         = 1 + foldr (+) 0 [2, 3, 4]
                         = 1 + 2 + foldr (+) 0 [3, 4]
                         = 1 + 2 + 3 + foldr (+) 0 [4]
                         = 1 + 2 + 3 + 4 + foldr (+) 0 []
                         = 1 + 2 + 3 + 4 + 0
                         = 1 + 2 + 3 + 4
```
</div>

もっと一般的に
--------------

`sum`と`foldr (+) 0`の等しさを証明しよう

```haskell
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

> sum []           = 0
> sum (x:xs)       = x + sum xs

. . .

やり方は帰納法証明みたいな感じになる

<div class="notes">
```haskell
foldr f 0 []     = 0
                 = sum []

foldr f 0 (x:xs)   = f x (foldr f 0 xs)
foldr (+) 0 (x:xs) = x + (foldr (+) 0 (x:xs))
                   = x + sum xs
                   = sum (x:xs)
```
</div>

もっともっと一般的に
--------------------

リストは下記のように考えられる

```haskell
[1, 2, 3, 4] ≡ 1 : 2 : 3 : 4 : []
```

- `[]` は空リスト
- `(:)` （コンズ）はリストの先頭に追加する関数

. . .

実は`foldr`は、`(:)`に`f`と`[]`に`z`を交換するものだ！

```haskell
foldr  f  z (1:2:3:4:[])  = 1 `f` 2 `f` 3 `f` 4 `f` z
foldr (+) 0 (1:2:3:4:[])  = 1 + 2 + 3 + 4 + 0
foldr (*) 1 (1:2:3:4:[])  = 1 * 2 * 3 * 4 * 1
foldr (:) [] (1:2:3:4:[]) = 1 : 2 : 3 : 4 : []
  -- ∴ foldr (:) [] ≡ id
```

証明は？

証明は定義による

```haskell
foldr f z []     = z                    -- `[]`と`z`の交換
foldr f z (x:xs) = x `f` (foldr f z xs) -- `(:)`と`f`の交換
```

命令的なコードでも使える
------------------------

> main = do
>   putStrLn "What is your name?"
>   name <- getLine
>   replicateM_ 3 $ putStrLn ("Hello " ++ name)

. . .

do記法から翻訳すると...

> main' =
>   putStrLn "What is your name?" >>
>   getLine                       >>= \name ->
>   replicateM_ 3 $ putStrLn ("Hello " ++ name)

出力は本当に３回出るか？

（この例は <http://www.haskellforall.com/2013/12/equational-reasoning.html> による）

ソースコード参照
----------------

<div class="notes">
このセッションは出来るだけHaskell Reportのソースコードを使っている。
ただ、Haskell Reportに入っていない場合もある。`replicateM_`はその一つ。
検索する方法は色々ある、例えば。。。
</div>

- Haskell Report Standard Prelude <http://www.haskell.org/onlinereport/standard-prelude.html>
- Hoogle <http://www.haskell.org/hoogle/>
- Hayoo <http://hayoo.fh-wedel.de>
- Googleで"hackage <関数名>"を検索してみる

. . .

> replicateM_ n x = sequence_ (replicate n x)

`sequence_`を調べると
-------------------

> sequence_ ms = foldr (>>) (return ()) ms

`foldr`でたーー！

. . .

置換してみよ

> replicateM_' n x = foldr (>>) (return ()) (replicate n x)

`foldr`はもうよく分かっているので、`replicate`の方をみてみよう

`replicate`とは
---------------

> replicate n x          = take n (repeat x)

. . .

> take n _      | n <= 0 = []
> take _ []              = []
> take n (x:xs)          = x : take (n-1) xs

. . .

> repeat x               = xs where xs = x:xs

`n`の値を知らないと`take`の置換できないので、`3`を入れる

<div class="notes">
```haskell
replicate 3 x = take 3 (repeat x)
              = x : take 2 (tail (repeat x))
              = x : x : take 1 (tail (repeat x))
              = x : x : x : take 0 (tail (repeat x))
              = x : x : x : []
```
</div>

定義を広げる
------------

```haskell
replicateM_' 3 x = foldr (>>) (return ()) (replicate 3 x)
replicateM_' 3 x = foldr (>>) (return ()) (x : x : x : [])
```

- `(:)`は、`(>>)`になる
- `[]`は、`return ()`になる

. . .

```haskell
replicateM_' 3 x = x >> x >> x >> return ()
```

`x`を置換する
-------------

```haskell
replicateM_' 3 x = x >> x >> x >> return ()
  where x = putStrLn ("Hello " ++ name)
```

. . .

> main'' =
>   putStrLn "What is your name?" >>
>   getLine                       >>= \name ->
>   putStrLn ("Hello " ++ name)   >> 
>   putStrLn ("Hello " ++ name)   >> 
>   putStrLn ("Hello " ++ name)   >> 
>   return ()

. . .

疲れた。

もっと一般的にできる？
----------------------

 <blockquote>
`n`の値を知らないと`take`の置換できないので、`3`を入れる
 </blockquote>

と書くとイラっとする。

また、今の証明は`3`の値としか証明できてない。 `4`を入れるとまた証明が必要。

もっといい方法あるのでしょうか？

数学的法則
----------

数学では、分配法則という法則がある。

$$
\begin{align}
a \times (b + c) &= a \times b + a \times c \\
(a \times b) + c &= a \times c + b \times c
\end{align}
$$

. . .

`replicateM_` に似たような法則を証明できたら、
何の数字を入れても、方程式を「１」について書き直せる。

. . .

```haskell
replicateM_ 0       x = return ()                          -- 和
replicateM_ (m + n) x = replicateM_ m x >> replicateM_ n x
replicateM_ 1         = id                                 -- 積
replicateM_ (m * n)   = replicateM_ m . replicateM_ n
```

. . .

```haskell
replicateM_ 3 x = replicateM_ (1 + 1 + 1 + 0) x
                = replicateM_ 1 x >> replicateM_ 1 x >> replicateM_ 1 x >> return ()
                = x >> x >> x >> return ()
  where x = putStrLn ("Hello " ++ name)
```

どうやって証明する？

<div class="notes">
The non-pointfree version of `replicateM_` distribution over multiplication is:

```haskell
replicateM_ 1       x = x
replicateM_ (m * n) x = replicateM_ m (replicateM_ n x)
```
</div>

便利な法則を探す
----------------

```haskell
replicateM_ n x = sequence_ (replicate n x)
```

この方程式を見ると、分配法則を証明する方法は明らかでない。

ただ、プログラミング的に考えると当たり前でしょう。

. . .

- `replicate` は、ある値をリストに`x`回重ねる
- `sequence_` は、あるリストをモナド的に順序で行える

`replicate`に`replicateM_`の「分配法則」に似たような法則を使って、
`sequence_`でその法則の「順序化」できるのでしょうか？

<div class="notes">
まぁ、できなかったらそういうことは言わないからできるでしょう。
</div>

はい、できる
------------

. . .

### `replicate`の「分配法則」

```haskell
replicate 0       x = []                             -- 和
replicate (m + n) x = replicate m x ++ replicate n x
replicate 1         = return                         -- 積
replicate (m * n)   = replicate m <=< replicate n
```

<div class="notes">
The non-pointfree version of `replicate` distribution over multiplication is:

```haskell
replicate 1       x = [x]
replicate (m * n) x = concatMap (replicate m) (replicate n x)
```
</div>

### `sequence_`の「順序化」

```haskell
sequence_ []          = return ()                    -- 連結　→　順序制御
sequence_ (xs ++ ys)  = sequence_ xs >> sequence_ ys
sequence_ . return    = void . id                    -- リスト関数　→　普通関数
sequence_ . (f <=< g) = (sequence_ . f) . (sequence_ . g)
```

<div class="notes">
The non-pointfree version of the `sequence_` lifting equations is:

```haskell
sequence_ [x] = x >> return ()
sequence_ (concatMap f (g x)) = sequence_ (f (sequence_ (g x)))
```
</div>

### `replicateM_`の「分配法則」の証明

```haskell
replicateM_ 0       x = sequence_ (replicate 0 x)
                      = sequence_ []
                      = return ()

replicateM_ (m + n) x = sequence_ (replicate (m + n) x)
                      = sequence_ (replicate m x ++ replicate n x)
                      = sequence_ (replicate m x) >> sequence_ (replicate n x)
                      = replicateM_ m x >> replicateM_ n x
```

必要な証明が増えてきた　☹
--------------------------

この方程式は証明できた

```haskell
replicateM_ 0       x = return ()
replicateM_ (m + n) x = replicateM_ m x >> replicateM_ n x
replicateM_ 1         = id
replicateM_ (m * n)   = replicateM_ m . replicateM_ n
```

. . .

ただ、その証明をするため、下記の法則を使った

```haskell
replicate 0       x   = []
replicate (m + n) x   = replicate m x ++ replicate n x
replicate 1           = return
replicate (m * n)     = replicate m <=< replicate n
sequence_ []          = return ()
sequence_ (xs ++ ys)  = sequence_ xs >> sequence_ ys
sequence_ . return    = void . id
sequence_ . (f <=< g) = (sequence_ . f) . (sequence_ . g)
```

これも証明しようと思ったら、逆に証明しないといけない方程式は２倍増えた！

<div class="notes">
実はそういうときある。

コードと同じく、
抽象化したら、証明が分かりやすくなるかもしれないし、
他の証明でも使えるかもしれない。
だけど、
逆に抽象化をしすぎてただの仕事になるときもある。
抽象化をするべきか、しない方がいいか、
というのは、自分で判断するしかない。

因に「全部自分で証明しなきゃダメ」とは言わない。
数学もそうだけど、
信頼できる人が「こういう法則あれよ！」と言ってくれたら、
そのまま使ってもいい。
</div>

<div class="notes">
### `replicate`の証明

この証明は <http://www.haskellforall.com/2013/12/equational-reasoning.html> による

証明したいこと

```haskell
replicate  0      x = []
replicate (m + n) x = replicate m x ++ replicate n x
replicate  1      x = [x]
replicate (m * n) x = concatMap (replicate m) (replicate n x)
```

参考のため

```haskell
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []
```

証明

```haskell
replicate 0 x       = take 0 (repeat x)                           -- replicateの定義
                    = []                                          -- takeの定義

replicate 1 x       = take 1 (repeat x)                           -- replicateの定義
                    = take 1 (x:repeat x)                         -- repeatの定義
                    = x:take 0 (repeat x)                         -- takeの定義
                    = x:[] = [x]                                  -- takeの定義

-- 和、m = 0
replicate (m + n) x = take (m + n) (repeat x)                     -- replicateの定義
                    = take (0 + n) (repeat x)                     -- 推定： m = 0
                    = take n (repeat x)                           -- 0 + n = n
                    = [] ++ take n (repeat x)                     -- (++)の定義　（逆）
                    = take 0 (repeat x) ++ take n (repeat x)      -- takeの定義　（逆）
                    = take m (repeat x) ++ take n (repeat x)      -- m = 0
                    = replicate m x ++ replicate n x              -- replicateの定義　（逆）

-- 和、m > 0 (m = 1 + m' where m' >= 0)
replicate (m + n) x = take (m + n) (repeat x)                     -- replicateの定義
                    = take (1 + m' + n) (repeat x)                -- 推定： m = 1 + m'
                    = take (1 + m' + n) (x:repeat x)              -- repeatの定義
                    = x:take (m' + n) (repeat x)                  -- takeの定義
                    = x:replicate (m' + n) x                      -- replicateの定義（逆）
                    = x:(replicate m' x ++ replicate n x)         -- 帰納法
                    = (x:replicate m' x) ++ replicate n x         -- (++)の定義　（逆）
                    = (x:take m' (repeat x)) ++ replicate n x     -- replicateの定義
                    = take (1 + m') (repeat x) ++ replicate n x   -- takeの定義　（逆）
                    = replicate (1 + m') x ++ replicate n x       -- replicateの定義（逆）
                    = replicate m x ++ replicate n x              -- m = 1 + m'（逆）

-- 積、n = 0
replicate (m * n) x = replicate 0 x                               -- 推定： n = 0
                    = []                                          -- replicate 0 x = []
                    = foldr ((++) . replicate m) [] []            -- foldrの定義（逆）
                    = concatMap (replicate m) []                  -- concatMapの定義（逆）
                    = concatMap (replicate m) (replicate 0 x)     -- replicate 0 x = [] （逆）
                    = concatMap (replicate m) (replicate n x)     -- n = 0 （逆）

-- 積、n > 0 (n = 1 + n' where n' >= 0)
replicate (m * n) x = replicate (m * (1 + n')) x                  -- 推定： n = 1 + n'
                    = replicate (m + m * n') x                    -- m * (1 + n') = m + m * n'
                    = replicate m x ++ replicate (m * n') x       -- replicateの「分配法則」（和）
                    = replicate m x ++ concatMap (replicate m) (replicate n' x)
                                                                  -- 帰納法
                    = replicate m x ++ foldr ((++) . replicate m) [] (replicate n' x)
                                                                  -- concatMapの定義
                    = foldr ((++) . replicate m) [] (x:replicate n' x)
                                                                  -- foldrの定義（逆）
                    = concatMap (replicate m) (x:replicate n' x)  -- concatMapの定義（逆）
                    = concatMap (replicate m) (x:take n' (repeat x))
                                                                  -- replicateの定義
                    = concatMap (replicate m) (take (1 + n') (x:repeat x))
                                                                  -- takeの定義（逆）
                    = concatMap (replicate m) (take n (x:repeat x))
                                                                  -- n = 1 + n' （逆）
                    = concatMap (replicate m) (take n (repeat x)) -- repeatの定義（逆）
                    = concatMap (replicate m) (replicate n x)     -- replicateの定義（逆）
```

### `sequence_`の証明

証明したいこと

```haskell
sequence_ []                  = return ()
sequence_ (xs ++ ys)          = sequence_ xs >> sequence_ ys
sequence_ [x]                 = x >> return ()
sequence_ (concatMap f (g x)) = sequence_ (f (sequence_ (g x)))
```

証明

```haskell
sequence_ []  = foldr (>>) (return ()) [] -- sequence_の定義
              = return ()                 -- foldrの定義 (foldr f z [] = z)

sequence_ [x] = foldr (>>) (return ()) [x]     -- sequence_の定義
              = x >> foldr (>>) (return ()) [] -- foldrの定義
              = x >> return ()                 -- foldrの定義

-- 和、xs = []
sequence_ (xs ++ ys) = sequence_ ([] ++ ys)        -- 推定： xs = []
                     = sequence_ ys                -- (++)の定義
                     = return () >> sequence_ ys   -- return () >> m = m　（逆）
                     = sequence_ [] >> sequence_ ys -- sequence_ [] = return ()　（逆）
                     = sequence_ xs >> sequence_ ys -- xs = []

-- 和、xs = x:xs' where xs' is a (possibly empty) list
sequence_ (xs ++ ys) = sequence_ (x:xs' ++ ys)                        -- 推定： xs = x:xs'
                     = foldr (>>) (return ()) (x:xs' ++ ys)           -- sequence_の定義
                     = x >> foldr (>>) (return ()) (xs' ++ ys)        -- foldrの定義
                     = x >> sequence_ xs' >> sequence_ ys             -- 帰納法
                     = foldr (>>) (return ()) (x:xs') >> sequence_ ys -- foldrの定義　（逆）
                     = sequence_ (x:xs') >> sequence_ ys              -- sequence_の定義　（逆）
                     = sequence_ xs >> sequence_ ys                   -- xs = x:xs'　（逆）


-- 積、g x = []
sequence_ (concatMap f (g x)) = sequence_ (foldr ((++) . f) [] (g x))  -- concatMapの定義
                              = sequence_ (foldr ((++) . f) [] [])     -- 推定： g x = []
                              = sequence_ []                           -- foldrの定義
                              = return ()                              -- sequence [] = return ()

                              = sequence_ (((++) . f) [] [])
                              = sequence_ (f [] ++ [])
                              = sequence_ (f [])
                              = sequence_ f [x'] >> sequence_ []

-- 積、f x = []
sequence_ (concatMap f (g x)) = sequence_ (concat (map f (g x)))              -- concatMap = concat . map
                              = sequence_ (foldr ((++) . f) [] (g x))         -- concatMapの定義
                              = sequence_ (foldr (\a b -> f a ++ b) [] (g x)) -- pointful式
                              = sequence_ (foldr (\_ b -> [] ++ b) [] (g x))  -- f x = []
                              = sequence_ []


                              = sequence_ (foldr (++) [] (foldr f [] (g x)))   -- foldr (f . g) x = foldr f x . foldr g x


                              = sequence_ (concat (map f []))    -- g x = []
                              = sequence_ (concat [])            -- mapの定義
                              = sequence_ []                     -- concatの定義


sequence_ (foldr ((++) . f) [] (g x))  -- concatMapの定義
                              = sequence_ 


                              = sequence_ (foldr (++) [] (foldr f [] (g x)))   -- foldr (f . g) x = foldr f x . foldr g x
                              = sequence_ (foldr (++) [] (foldr f [] ([x']:[]))) -- 推定： g x = [x']:[]
                              = sequence_ (foldr (++) [] (f [x'] []))          -- foldrの定義
                              = sequence_ ((f [x'] []) ++ [])                  -- foldrの定義
                              = sequence_ (f [x'] []) >> sequence_ []
                              = sequence_ (f [x'] []) >> return ()

型

f :: m1 () -> [m a1]
g :: a     -> [m1 ()]
x :: a

foldr (>>) (return ()) (f (foldr (>>) (return ()) (g x)))
sequence_ (f (foldr (>>) (return ()) (g x)))

                              = sequence_ (f (sequence_ (g x)))

sequence_ ms = foldr (>>) (return ()) ms
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []
map _ []     = []
map f (x:xs) = f x : map f xs
```

</div>

何の価値？ {.titlepage}
----------

<div class="notes">
学校でこういう事を勉強したときに、「めんどくせぇ〜」としか思わなかった。
何でわざわざプログラミングでするのか？
</div>

正確さを証明できること {.titlepage}
----------------------

### あるいは、ある法則に従っていることを証明できること

最適化をできること {.titlepage}
------------------

### ＝最適化されたバージョンは元のバージョンに等しいと証明すること

<div class="notes">
証明できたら、コンパイル時に自動的に最適化を行える！
</div>

ドキュメンテーション {.titlepage}
--------------------

### Hackageではよく使われている

難しいコンセプトを理解すること {.titlepage}
------------------------------

### 分からないモナドがあったら等式推論をやってみ！

なれたら実は楽しい！ {.titlepage}
--------------------

等式推論やってみたい！アドバイスとコツ
--------------------------------------

- 自分のコードに便利な法則を探す
    - 特にtypeclass
- ドキュメンテーションに書いてある方程式を法則として使う
- 「抽象化できるか？」を考えながら証明する
- 証明ができたらその法則をドキュメンテーションに書く！
    - 証明自体はアップしなくていい
    - それでもしたいと思ったら、コメントか、ブログに書いたらいいでしょう
- 自分のモナド、レンズ、パイプ等を書くとき、法則を証明しよう！

参考文献
--------

- Gabriel Gonzalezさん (Tekmo)　がよく使っている
    - Equational Reasoning          <http://www.haskellforall.com/2013/12/equational-reasoning.html>
    - Equational Reasoning at Scale <http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html>
- Pearls of Functional Algorithm Design, Richard Bird
    - <http://www.amazon.co.jp/Pearls-Functional-Algorithm-Design-Richard-ebook/dp/B009019VUK/ref=sr_1_1?ie=UTF8&qid=1409933562&sr=8-1&keywords=pearls+of+functional+algorithm+design>
- Typeclassopedia <http://www.haskell.org/haskellwiki/Typeclassopedia>
    - 等式推論の話自体はないけど、Typeclass法則は全部方程式で書いてて、等式推論で証明するのはいい練習になるかもしれない

終 {.titlepage}
--

. . .

### ありがとうございました

<div class="notes">
例　Stateモナド
---------------

> newtype State s a = State { runState :: s -> (a, s) }
> instance Monad (State s) where
>     return a = State $ \s -> (a, s)
>     m >>= k  = State $ \s -> let
>         (a, s') = runState m s
>         in runState (k a) s'
> get   = State $ \s -> (s, s)
> put s = State $ \_ -> ((), s)

> m >> k = m >>= \_ -> k

> tick = do
>   n <- get
>   put $ n + 1
>   return n

```haskell
tick = get >>= \n -> put (n + 1) >> return n                        -- do記法翻訳
     = get >>= \n -> put (n + 1) >>= \_ -> return n                 -- (>>)の定義
     = get >>= \n -> put (n + 1)                                    -- returnの定義
           >>= \_ -> State (\s -> (n, s))
     = State (\s -> (s, s)) >>= \n -> put (n + 1)                   -- getの定義
                            >>= \_ -> State (\s -> (n, s))
     = State (\s -> (s, s)) >>= \n -> State (\_ -> ((), n + 1))     -- putの定義
                            >>= \_ -> State (\s -> (n, s))
     = State (\s -> let (a, s') = runState (State (\t -> (t, t))) s -- (>>=)の定義
                    in runState ((\n -> State (\_ -> ((), n + 1))
                             >>= \_ -> State (\s'' -> (n, s''))) a) s')
     = State (\s -> let (a, s') = (s, s)                            -- runState適用
                    in runState ((\n -> State (\_ -> ((), n + 1))
                              >>= \_ -> State (\s'' -> (n, s''))) a) s')
     = State (\s -> runState ((\n -> State (\_ -> ((), n + 1))       -- (a, s')変数置換
                           >>= \_ -> State (\s'' -> (n, s''))) s) s)
     = State (\s -> runState (State (\_ -> ((), s + 1))              -- n変数置換
                           >>= \_ -> State (\s'' -> (s, s''))) s)
     = State (\s -> runState                                         -- (>>=)の定義
        (State (\t -> let (a, s') = runState (State (\_ -> ((), s + 1))) t
                      in runState ((\_ -> State (\s'' -> (s, s''))) a) s')) s)
     = State (\s -> runState                                         -- runState適用
        (State (\t -> let (a, s') = ((), t + 1)
                      in runState ((\_ -> State (\s'' -> (s, s''))) a) s')) s)
     = State (\s -> runState                                         -- (a, s')変数置換
        (State (\t -> runState ((\_ -> State (\s'' -> (s, s''))) ()) (t + 1))) s)
     = State (\s -> runState (State (\t -> (s, t + 1))) s)           -- runState適用
     = State (\s -> (s, s + 1))                                      -- runState適用
```
</div>

[tataminomusi]:  http://twitter.com/tataminomusi
[danielpwright]: http://twitter.com/danielpwright
[github]:        http://github.com/dpwright
