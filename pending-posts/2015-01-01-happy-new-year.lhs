---
date: 2015-01-01 08:14:31
tags: 日本語, functional-programming, haskell, お正月, new year, 年賀状
title: あけましておめでとうございます
---

<center>![](/images/2015-01-01-happy-new-year/nengajou.png "明けましておめでとうございます")</center>

年賀状をHaskellで
-----------------

上の画像が今年の僕達の年賀状です。

実は、妻が最初アプリでデザインを考えていたのですが、
画像のダウンロードができず、Pixelmatorで作りなおそうという話がでました。
でも、これってHaskellでできるんじゃないの…？と思い、
試しにやってみることにしました。

結局５分の仕事が２日になったけど、面白かったです。

概要
----

まず、上の画像をみて、デザインを考えます。

最初は、下記のやりかたでやろうと思っていました。

1. 写真の上に、真っ白のレイヤーを載せる。
2. その白いレイヤーから３つの三角を切って、下の写真が見えるようになる。
3. 最後にメッセージを追加する。

ただ、diagramsでは、レイヤーを作って、そのレイヤーから切る機能がなかった
（というか、あるかもしれないけど、僕が分からなかったので）。

結局、「上の白いレイヤーから切る」という方法ではなく、
「そのまま５つの三角を描く」という方法でやりました。

文章ではちょっと分かりづらいと思うので、絵で説明します。

<center>![](/images/2015-01-01-happy-new-year/triangles.png "５つの三角")</center>

この下にメッセージをつけます。

では、実際にどう作ったか振り返ります。

依存関係
--------

まず、Haskellのライブラリをインストールします。

下記の`cabal`設定でインストールしました。

```
executable nengajou2015
  main-is:          	2015-01-01-happy-new-year.lhs	
  build-depends:    	base                	>= 4.7 && <4.8,
                    	diagrams            	== 1.2.*,
                    	diagrams-lib        	== 1.2.*,
                    	diagrams-rasterific 	== 0.1.*,
                    	text                	== 1.1.*,
                    	directory           	== 1.2.*,
                    	filepath            	== 1.3.*,
                    	process             	>= 1.1.0.0 && <1.3,
                    	SVGFonts            	== 1.4.0.3
  default-language: 	Haskell2010	
```

`arithmoi`は最近`llvm`のバージョンによってエラーになるときがあります。
その場合は、下記のコマンドで`cabal install`してください。

```
$ cabal install --constrain "arithmoi -llvm"
```

フォントを`svn`に変換するため、`fontforge`が必要です。
Macを使ってたので、まず[XQuartz]をインストールしました。
それから、Homebrewで、

```
$ brew install fontforge
```

を入力して、インストールします。

予備
----

`LANGUAGE`プラグマは、意外と多くなりました。
まず、いつもの`OverloadedStrings`と、
diagramsのドキュメンテーションにすすめされる`NoMonomorphismRestriction`。

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

実は`NoMonomorphismRestriction`は今回はいらなかったけど、
あった方がいいと書いてあったから入れてみました。

フォントを設定するためのデータ構造で、`Functor`と`Traversable`の関数を使いたいです。

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}

よっし！diagramsをインポートしよ！

> import Diagrams.Prelude hiding ((<.>))
> import Diagrams.Backend.Rasterific.CmdLine

あ、テキストも書きたいからSVGFontsも必要になるね…

> import Graphics.SVGFonts
> import Graphics.SVGFonts.ReadFont

でも、SVGFontsを使うため、フォントをttfからsvgに変換しないと…

そのため、システム的なモジュールも必要だね…

> import System.Directory
> import System.FilePath
> import System.Process
> import System.Exit

あ、きっとそのsvgファイルを調整しないとダメだろー

`Data.Text`を使った方が最適…

> import qualified Data.Text    	as T
> import qualified Data.Text.IO 	as TIO

え、さっき`Traversable`って言わなかった？

> import Data.Foldable (Foldable (..))
> import Data.Traversable as TV

まだだよー

> import Control.Arrow
> import Control.Monad

まだだよー

> import Data.Char

終わり。

型定義
------

今回のプロジェクトは殆どの型はdiagramsで定義されています。

一つだけを自分で定義します。それは、フォントを設定するための型です。

> data Fonts a 	= Fonts
>              	{ english  :: a
>              	, numbers  :: a
>              	, japanese :: a
>              	} deriving (Functor, Foldable, Traversable)

なぜ多相型で定義する必要があるのか？
フォントは自動的に`.ttf`から`.svg`に変換するつもりなんで、
同じストラクチャーで、ロードする前のファイル名と、
準備ができた使える状況のフォントを入れたいのです。

> newtype PreparedFont = PreparedFont { fromPF :: String }

このnewtypeを使って、「フォントが変換された」と、型安全的に証明します。

それでは、コード自体を始めます。

二等辺三角形
------------

もう一回三角の画像を見てみましょう。

<center>![](/images/2015-01-01-happy-new-year/triangles.png "５つの三角")</center>

この５つの三角は全部二等辺三角形です！
diagramsは正三角形を作る関数を定義されているけど、
二等辺三角形はないため、自分で定義します。

> isosceles :: Angle -> Double -> Diagram B R2
> isosceles θ a = polygon (with &
>   polyType .~ PolySides
>   [ (180 @@ deg) ^-^ θ ]
>   [ legLength, legLength ]
>   ) where legLength = a / cosA (θ^/2)

`θ`は頂角、`a`は高度（altitude）です。

`(^-^)`、`(^/)`は[vector-space]というパッケージで定義され、
角度やベクトル等を引算、割算ができるような関数です。
分からないときは`^`をなしにしてみたら、だいたいあってます
（`^-^` → `-`、`^/` → `/`)。

`(&)`、`(.~)`は[lens]のオペレーターです。
今からlensの説明しようとすると話が終わらないので、省きます。

画像のレイアウトをするため、底面の長さが必要なときがあります。
それを計算するために下記のユーティリティ関数を使います。

> isoscelesBase :: Angle -> Double -> Double
> isoscelesBase θ a = 2 * tanA (θ^/2) * a

レイアウト
----------

簡単に言うと、このデザインは「上に画像があって、その下にメッセージ」と説明できます。

とりあえず、フォントや写真はもう既にロードされているとしてみます。
そうすると、下記のようになります。

> nengajou :: Fonts PreparedFont -> Diagram B R2 -> Diagram B R2
> nengajou fs photo = 	topImage θ photoShiftedRight # inViewport
>                     	===
>                     	message fs imageWidth (photoHeight / 3)

`(===)`は、２つの*`Diagram`*を重ねる関数です。

分かりやすいでしょう！
`topImage`は上の画像、`message`は下のメッセージ。
重ねると、年賀状になると。

あとは、ここで使った値を定義するだけです。

>   where
>     θ                 	= 	50 @@ deg
>     photoHeight       	= 	height photo
>     imageWidth        	= 	isoscelesBase θ photoHeight

まずは、写真や画像についての変数です。`θ`はここで変更したら、三角形の形が変わります。
`photoHeight`は写真全体の高さけど、
`imageWidth`はできた画像（メーン三角）の幅となります。

>     photoShiftedRight 	= 	photo # translateX 40

ちょっと細かいことですけど、
実はもとの写真では僕達は真ん中じゃなかったので、
これで調整します。

>     inViewport = 	view 	(p2 ((-imageWidth) / 2, (-photoHeight) / 2))
>                  	     	(r2 (imageWidth, photoHeight))

色が付いている三角の画像をみると、ちょっと外側が汚いのです。
`inViewport`は、そとのいらない分を消す関数です。

画像
----

画像自体も簡単に説明できます。まず、５つの三角と合わすため、
写真を切ります。その切った写真の上に、三角を載せます。

> topImage :: Angle -> Diagram B R2 -> Diagram B R2
> topImage θ photo = triangles <> clippedPhoto where

切るサイズは、写真の元の高さ✕真ん中の、大きい三角の底面の長さです。
それではちょっと幅が見えてしまうので、あと２ピクセルずつも、念の為に切ります。

>   clippedPhoto 	= photo # clipBy (rect 	(centralTriangleBase - 2)
>                	                       	(photoHeight - 2))
>   photoHeight         	= height photo
>   centralTriangleBase 	= isoscelesBase θ photoHeight

`triangles`は、左から右に言うと、

>   triangles 	= mconcat
>             	[ edgeTriangleLeft
>             	, bottomTriangleLeft
>             	, centralTriangle
>             	, bottomTriangleRight
>             	, edgeTriangleRight
>             	]

になります。この画像では順番は何でもいいけど、
実は描く順番になります（`edgeTriangleLeft`の上に
`bottomTriangleLeft`を描いて、その上に
`centralTriangle`を描く…との形）。

この画像の三角を見ると２種類があります。
まずは、真ん中の３つの三角。

<center>![](/images/2015-01-01-happy-new-year/outline-triangles.png "Picture of 3 central triangles goes here")</center>

輪郭のみが描いて、下にある写真が見える三角ですね。

>   outlineTriangle a 	= isosceles θ a
>                     	# centerXY
>                     	# lc white
>                     	# lw ultraThick

なぜ`centerXY`が必要か？Diagramsはデフォルトでは
原点は高度から見る真ん中ではなく、重心に設定してあります。
二等辺三角形を鏡映すると、ずれてしまう。
`centerXY`をしたら、簡単に鏡映できるようになります。

次は左と右の、真っ白の三角です。

<center>![](/images/2015-01-01-happy-new-year/outline-triangles.png "Picture of 2 edge triangles goes here")</center>

この三角形の頂角は真ん中の三角の反対角度になっています。

>   θ' = (180 @@ deg) ^-^ θ

位置は

>   edgeTriangle 	= isosceles θ' bottomTriangleHalfBase
>                	# centerXY
>                	# lw none
>                	# fc white
>                	# translateY (bottomTriangleHalfBase / 2)
>                	# rotate (90 @@ deg)

>   centralTriangle   	= outlineTriangle photoHeight # reflectY

>   bottomTriangle 	= outlineTriangle (photoHeight / 2)
>                  	# translateY (-(photoHeight / 4))
>   bottomTriangleRight    	= bottomTriangle # translateX bottomTriangleHalfBase
>   bottomTriangleLeft     	= reflectX bottomTriangleRight
>   bottomTriangleHalfBase 	= centralTriangleBase / 4

>   edgeTriangleRight 	= edgeTriangle # translateX (bottomTriangleHalfBase * 2)
>   edgeTriangleLeft  	= reflectX edgeTriangleRight


メッセージ
----------

> message :: Fonts PreparedFont -> Double -> Double -> Diagram B R2
> message fs w h = center messageText where
>   text' f (a, d) l s	= 	stroke (textPath s f a d) # lw l # fc black <> strutY d
>   textPath s f a d 	= 	textSVG' $ TextOpts s (outlMap . fromPF $ f fs) INSIDE_WH KERN False a d
>   proportions  	= 	[ (0, 6), (w * 9 / 10, 10), (0, 1), (w / 6, 6)
>                	  	, (0, 4), (w * 33 / 40, 4), (w * 33 / 40, 5) ]
>   totalHeight  	= 	sum (map snd proportions)
>   part n       	= 	second ((/ totalHeight) . (* h)) $ proportions !! (n - 1)
>   space        	= 	strut . r2
>   messageText  	= 	space (part 1)
>                	  	===
>                	  	text' english (part 2) thin (map toUpper "Happy New Year")
>                	  	===
>                	  	space (part 3)
>                	  	===
>                	  	text' numbers (part 4) none "2015"
>                	  	===
>                	  	space (part 5)
>                	  	===
>                	  	text' japanese (part 6) none "昨年はお世話になりました　今年もよろしくお願いします"
>                	  	===
>                	  	text' english (part 7) none "Wishing you a fantastic New Year, from Aki & Dani"

不純なところ
------------

> loadPhoto :: FilePath -> IO (Diagram B R2)
> loadPhoto fp = either reportError (return . image) =<< loadImageEmb fp
>   where reportError err = putStrLn err >> exitWith (ExitFailure 1)

> prepareFonts :: Fonts String -> IO (Fonts PreparedFont)
> prepareFonts fs = makeDirectory >> TV.sequence (fmap prepareFont fs) where
>   fontforge f f' 	= unwords ["fontforge --lang=ff -c 'Open($1); Generate($2)'", f, f']
>   fontDir        	= "svg-fonts"
>   makeDirectory  	= do
>     dirExists <- doesDirectoryExist fontDir
>     unless dirExists $ createDirectory fontDir
>   prepareFont f  	= do
>     let f' = fontDir </> filter (/= ' ') (takeBaseName f) <.> "svg"
>     fontAlreadyConverted <- doesFileExist f'
>     unless fontAlreadyConverted $ do
>       system $ fontforge f f'
>       stripNamespaceLineFrom f'
>     return $ PreparedFont f'

> stripNamespaceLineFrom :: FilePath -> IO ()
> stripNamespaceLineFrom f = TIO.readFile f >>= go where
>   go 	= T.words
>      	>>> filter (not . T.isInfixOf "xmlns")
>      	>>> T.unwords
>      	>>> TIO.writeFile f

`main`関数
----------

> main :: IO ()
> main = do
>   photo <- loadPhoto "static/images/2015-01-01-happy-new-year/beach-club.png"
>   fonts <- prepareFonts Fonts 	{ english  = "/Library/Fonts/Microsoft/Garamond"
>                               	, numbers  = "/Library/Fonts/Microsoft/Calisto MT"
>                               	, japanese = "/Library/Fonts/Microsoft/ＤＦＰ教科書体W3" }
>   mainWith . bg white . pad 1.1 $ nengajou fonts photo

[diagrams]:     http://projects.haskell.org/diagrams
[XQuartz]:      https://xquartz.macosforge.org/
[vector-space]: https://hackage.haskell.org/package/vector-space
[lens]:         https://hackage.haskell.org/package/lens
