---
date: 2015-01-01 00:00:00
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

５分の仕事が結局２日になったけど（笑）…
面白かったので、やり方を公開します。

概要
----

まず、上の画像をみて、デザインを考えます。

最初は、下記のやりかたでやろうと思っていました。

1. 写真の上に、真っ白のレイヤーを載せる。
2. その白いレイヤーから３つの三角形を切って、下の写真が見えるようになる。
3. 最後にメッセージを追加する。

ただ、diagramsでは、レイヤーを作って、そのレイヤーから切る機能がなかった
（というか、あるかもしれないけど、僕が分からなかったので）。

結局、「上の白いレイヤーから切る」という方法ではなく、
「そのまま５つの三角形を描く」という方法でやりました。

文章ではちょっと分かりづらいと思うので、絵で説明します。

<center>![](/images/2015-01-01-happy-new-year/triangles.png "５つの三角形")</center>

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
                    	diagrams            	== 1.2,
                    	diagrams-lib        	== 1.2.0.2,
                    	diagrams-rasterific 	== 0.1.0.1,
                    	text                	== 1.1.1.3,
                    	directory           	== 1.2.1.0,
                    	filepath            	== 1.3.0.2,
                    	process             	== 1.2.0.0,
                    	SVGFonts            	== 1.4.0.3
  default-language: 	Haskell2010	
```

`arithmoi`は最近`llvm`のバージョンによってエラーになるときがあります。
その場合は、下記のコマンドで`cabal install`してください。

```
$ cabal install --constrain "arithmoi -llvm"
```

フォントを`svg`に変換するため、`fontforge`が必要です。
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
diagramsのドキュメンテーションに推奨される`NoMonomorphismRestriction`。

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

`NoMonomorphismRestriction`は使った方がいいと書いてあったので
入れましたが必要なかったので、結果的に使いませんでした。

フォントを設定するためのデータ構造では、
`Functor`と`Traversable`の関数を使いたいと思いました。

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

> import Data.Foldable (Foldable)
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

自分で定義をしたのは一つだけです。それは、フォントを設定するための型です。

> data Fonts a 	= Fonts	
>              	{ english  	:: a
>              	, numbers  	:: a
>              	, japanese 	:: a
>              	} deriving 	(Functor, Foldable, Traversable)

「なぜ多相型で定義する必要があるの？」と思うかもしれませんが、
フォントは自動的に`.ttf`から`.svg`に変換できるようにしたいので、
同じストラクチャーで、ロードする前のファイル名と、
準備ができた使える状態のフォントを入れたいと思います。

> newtype PreparedFont = PreparedFont { fromPF :: String }

このnewtypeを使って、「フォントが変換された」と、型安全的に証明します。
（因みに最新版のSVGFontsでは`PreparedFont`は既に定義されているので、
最新版がHackageにアップされたらこれは必要なくなります。）

それでは、コード自体を始めましょう。

二等辺三角形
------------

もう一度下の画像を見てみましょう。

<center>![](/images/2015-01-01-happy-new-year/triangles.png "５つの三角形")</center>

この５つの三角形は全部二等辺三角形です！
diagramsは正三角形を作る関数が定義されていますが、
二等辺三角形はないため、自分で定義する必要があります。

> isosceles :: Angle -> Double -> Diagram B R2
> isosceles θ a = polygon (with &
>   polyType .~ PolySides
>   [ (180 @@ deg) ^-^ θ ]
>   [ legLength, legLength ]
>   ) where legLength = a / cosA (θ^/2)

`θ`は頂角、`a`は高度（altitude）です。

`(^-^)`、`(^/)`は[vector-space]というパッケージで定義され、
角度やベクトル等を引算、割算ができるような関数です。
意味が分からないときは`^`を消してみたら、だいたいの意味が見えてきます
（`(^-^)` → `(-)`、`(^/)` → `(/)`)。

`(&)`、`(.~)`は[lens]のオペレーターです。
今からlensの説明しようとすると話が終わらないので、今日は省きます。

画像のレイアウトをするため、底辺の長さが必要なときがあります。
それを計算するために下記のユーティリティ関数を使います。

> isoscelesBase :: Angle -> Double -> Double
> isoscelesBase θ a = 2 * tanA (θ^/2) * a

レイアウト
----------

簡単に言うと、このデザインは「上に画像があって、その下にメッセージ」と説明できます。

とりあえず、フォントや写真はもう既にロードされていると見なしましょう。
そうすると、下記のようになります。

> nengajou :: Fonts PreparedFont -> Diagram B R2 -> Diagram B R2
> nengajou fs photo = 	topImage θ photoShiftedRight # inViewport
>                     	===
>                     	message fs imageWidth (photoHeight / 3)

`(===)`は、２つの*`Diagram`*を上から下に並べる関数です。

分かりやすいでしょう！
`topImage`は上の画像、`message`は下のメッセージ。
並べると、年賀状になると。

あとは、ここで使った値を定義するだけです。

>   where
>     θ                 	= 	50 @@ deg
>     photoHeight       	= 	height photo
>     imageWidth        	= 	isoscelesBase θ photoHeight

まずは、写真や画像についての変数です。`θ`の値を変更したら、三角形の形を変えられます。
`photoHeight`は写真全体の高さですが、
`imageWidth`はできた画像（緑のメイン三角形）の幅となります。

>     photoShiftedRight 	= 	photo # translateX 40

実はもとの写真では人物は真ん中ではなかったので、
細かいことになりますが、これで調整しています。

>     inViewport = 	view 	(p2 ((-imageWidth) / 2, (-photoHeight) / 2))
>                  	     	(r2 (imageWidth, photoHeight))

色が付いている三角形の画像をみると、ちょっと外側が汚いので、
`inViewport`で、外の部分を消す関数を用意しました。

画像
----

画像自体も簡単に説明できます。まず、５つの三角形と合わせるため、
写真を切る必要があります。その切った写真の上に、三角形を載せていきます。

> topImage :: Angle -> Diagram B R2 -> Diagram B R2
> topImage θ photo = triangles <> clippedPhoto where

切るサイズは、写真の元の高さ✕緑の三角形の底辺の長さです。
それではちょっと幅が見えてしまうので、あと２ピクセルずつ、念の為に切ります。

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

になります。この画像では順番は関係ないけれど、
場合によっては必要になりますね。
*`Diagram`*を連結すると、
上から下の順番になります（`edgeTriangleLeft`の下に
`bottomTriangleLeft`を描いて、その下に
`centralTriangle`を描く…という形）。

この画像の三角形を見ると２種類があります。
まずは、真ん中の３つの三角形。

<center>![](/images/2015-01-01-happy-new-year/outline-triangles.png "Picture of 3 central triangles goes here")</center>

輪郭のみを描いて、下にある写真が見える三角形ですね。

>   outlineTriangle a 	= isosceles θ a
>                     	# centerXY
>                     	# lc white
>                     	# lw ultraThick

「なぜ`centerXY`が必要か？」ですが、Diagramsはデフォルトでは
原点は高度から見る真ん中ではなく、重心に設定してあります。
二等辺三角形を鏡映すると、ずれてしまう。
`centerXY`をしたら、簡単に鏡映できるようになります。

次は左と右の、真っ白の三角形です。

<center>![](/images/2015-01-01-happy-new-year/edge-triangles.png "Picture of 2 edge triangles goes here")</center>

この三角形の頂角は真ん中の三角形の反対角度になっています。

>   θ' = (180 @@ deg) ^-^ θ

高度は下の三角形の底辺の半分です。
この値はまた使うので変数に保存しておきましょう。

>   bottomTriangleHalfBase 	= centralTriangleBase / 4

あとは色と90°の回転です。

>   edgeTriangle 	= isosceles θ' bottomTriangleHalfBase
>                	# centerXY
>                	# lw none
>                	# fc white
>                	# rotate (90 @@ deg)

`outlineTriangle`と`edgeTriangle`の２種類を定義できました。
これで５つの三角形が描けます。まずは真ん中の三角形です。
高度は写真と一緒です。その三角形を、*y* 軸に鏡映します。

>   centralTriangle   	= outlineTriangle photoHeight # reflectY

あとは右側の下の三角形と真っ白の三角形。

>   bottomTriangleRight 	= outlineTriangle (photoHeight / 2)
>                       	# translateY (-(photoHeight / 4))
>                       	# translateX bottomTriangleHalfBase
>   edgeTriangleRight   	= edgeTriangle
>                       	# translateX (bottomTriangleHalfBase * 3 / 2)

最後は左側です。右側を、*x* 軸に鏡映するだけです。

>   bottomTriangleLeft 	= reflectX bottomTriangleRight
>   edgeTriangleLeft   	= reflectX edgeTriangleRight

画像の分はここまでです。

メッセージ
----------

最初に「型は*`Fonts`*以外は定義しない」と言いましたが、
実は、メッセージを定義するため、今回下記のユーティリティー型を定義しました。

> data MessagePart
>   = 	MsgText            	
>   { 	proportionalHeight 	:: Double
>   , 	proportionalWidth  	:: Double
>   , 	font               	:: Fonts PreparedFont -> PreparedFont
>   , 	outline            	:: Measure R2
>   , 	msgText            	:: String
>   } 	                   	
>   | 	MsgSpace           	
>   { 	proportionalHeight 	:: Double
>   } 	                   	

この型はメッセージのDiagramを定義するための、一瞬の型なので、
絶対必要とは言えません。
正直、もともとこのプログラムを書いたときはこの型は使わず作りました。
ただ、あった方が絶対分かりやすいと思って、
ブログのためにちょっとリファクタリングしてみました。

*`MessagePart`*はメッセージの一行です。
その一行はメッセージのテキスト（*`MsgText`*)か、
何も表記しない、ただスペース開けるため（*`MsgSpace`*）です。

この型について一つポイントがあります。
`proportionalHeight`と`proportionalWidth`は、「高さ」と「幅」の割合を意味します。
ただ、表記の仕方はそれぞれ違います。
`proportionalWidth`の方は、全体の幅に対しての割合ー
例えば、幅の半分としたいなら`1/2`と表記します。
一方、`proportionalHeight`は、表記スペースに対して一行の大きさを決るため、表記スペースに対するの「割合の分子」のみ表記します。
分母は、全部のメッセージの`proportionalHeight`の合計になるはずです。
結果、*`MessagePart`*を並べれば、スペースを１００％と使えていることになります。

この型があったらメッセージ自体は、ただ*`MessagePart`*のリストになります。

> messageParts :: [MessagePart]
> messageParts =
>   [ MsgSpace 	6
>   , MsgText  	10 (9 / 10) english thin
>              	(map toUpper "Happy New Year")
>   , MsgSpace 	1
>   , MsgText  	6 (1 / 6) numbers none "2015"
>   , MsgSpace 	4
>   , MsgText  	4 (33 / 40) japanese none
>              	"昨年はお世話になりました　今年もよろしくお願いします"
>   , MsgText  	5 (33 / 40) english none
>              	"Wishing you a fantastic New Year, from Aki & Dani"
>   ]

では、このメッセージをどうやって*`Diagram`*に変換しましょう？

> message :: Fonts PreparedFont -> Double -> Double -> Diagram B R2
> message fs w h = center messageText where
>   messageText  = foldr1 (===) . map drawMsgPart $ messageParts

*`MessagePart`*を一つ一つ`drawMsgPart`で描いて、
それから`foldr1 (===)`で上から下まで並べます。

>   drawMsgPart (MsgSpace ph)        	= strut $ r2 (0, getRealHeight ph)
>   drawMsgPart (MsgText ph pw f o t) 	= text' (w * pw, getRealHeight ph) f o t

*`MsgSpace`*だったら、ただ*y* 軸にスペースを開けます。
*`MsgText`*だったら`text'`としてレンダーします。

`getRealHeight`は、割合の分子から、実際の高さに変換する関数です。

>   getRealHeight ph 	= h * ph / totalHeight
>   totalHeight  	= sum $ map proportionalHeight messageParts

`text'`は、SVGFontsを使ってテキストをレンダーします。

>   text' (a, d) f o t 	= stroke (textPath t f a d)
>                      	# lw o # fc black <> strutY d
>   textPath t f a d 	= textSVG' $ 	TextOpts t (outlMap . fromPF $ f fs)
>                    	             	INSIDE_WH KERN False a d

不純なところ
------------

今までの関数は純粋的に定義しました。
これからは、実世界と繋がっている`IO`モナドを使って、
写真やフォントを準備するための関数を定義します。

写真は簡単です。Diagramsの`loadImageEmb`関数を呼んで、
エラーが返されたらそのまま出力して停止します。

> loadPhoto :: FilePath -> IO (Diagram B R2)
> loadPhoto fp = either reportError (return . image) =<< loadImageEmb fp
>   where reportError err = putStrLn err >> exitWith (ExitFailure 1)

フォントはもうちょっと複雑なんです。

> prepareFonts :: Fonts String -> IO (Fonts PreparedFont)
> prepareFonts fs = do 	makeDirectory
>                      	TV.sequence (fmap prepareFont fs) where

フォントは`.ttf`から`.svg`に変換しないと使えないんです。
変換されたフォントは`svg-fonts`というディレクトリーに出力します。
まず、そのディレクトリーがない場合は作成しないとダメです。

>   fontDir        	= "svg-fonts"
>   makeDirectory  	= do
>     dirExists <- doesDirectoryExist fontDir
>     unless dirExists $ createDirectory fontDir

変換自体はfontforgeを使って行います。
もしもう既に変換されたフォントがあるならまた変換する必要はありません。

>   fontforge f f' 	= unwords ["fontforge --lang=ff -c 'Open($1); Generate($2)'", f, f']
>   prepareFont f  	= do
>     let f' = fontDir </> filter (/= ' ') (takeBaseName f) <.> "svg"
>     fontAlreadyConverted <- doesFileExist f'
>     unless fontAlreadyConverted $ do
>       system $ fontforge f f'
>       stripNamespaceLineFrom f'
>     return $ PreparedFont f'

fontforgeが出すXMLはネームスペースに入っていますが、
SVGFontsがネームスペース無しのXMLしかサポートされていません。
下記の関数はネームスペース宣言を外します。

> stripNamespaceLineFrom :: FilePath -> IO ()
> stripNamespaceLineFrom f = TIO.readFile f >>= go
>   where go 	=   	T.words
>            	>>> 	filter (not . T.isInfixOf "xmlns")
>            	>>> 	T.unwords
>            	>>> 	TIO.writeFile f

`main`関数
----------

最後に、上記の関数を結んでいく`main`関数です。

写真とフォントの準備をし、`nengajou`の関数に渡して*`Diagram`*を作成します。
`pad`を使って枠を作ります。それから背景を白にしましょう。
最終、Diagramsの`mainWith`関数を使ってコマンドラインインターフェースが出来上がります。

> main :: IO ()
> main = do
>   photo <- loadPhoto "static/images/2015-01-01-happy-new-year/beach-club.png"
>   fonts <- prepareFonts Fonts
>     	{ english  	= "/Library/Fonts/Microsoft/Garamond"
>     	, numbers  	= "/Library/Fonts/Microsoft/Calisto MT"
>     	, japanese 	= "/Library/Fonts/Microsoft/ＤＦＰ教科書体W3" }
>   mainWith $ nengajou fonts photo # pad 1.1 # bg black

さあ、これで今年の年賀状が完成しました！

今年も楽しみながら、関数型言語で面白いものを作って行きましょう！

[diagrams]:     http://projects.haskell.org/diagrams
[XQuartz]:      https://xquartz.macosforge.org/
[vector-space]: https://hackage.haskell.org/package/vector-space
[lens]:         https://hackage.haskell.org/package/lens
