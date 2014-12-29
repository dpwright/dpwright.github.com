---
date: 2015-01-01 08:14:31
tags: 日本語, functional-programming, haskell, お正月, new year
title: 明けましておめでとうございます
subtitle: 今年もよろしくお願いします
---

<center>![](../images/2015-01-01-happy-new-year/nengajou.png "明けましておめでとうございます")</center>

Haskellで年賀状を作ってみました
-------------------------------

今年も関数型言語をがんばろ！という気持ちで、年賀状を作ってみました。
デザインはもともと妻がアプリで作ったんだけど、
これだったら自分で作れるぜー、と言ってHaskellの[diagrams]ライブラリを、`cabal install`しました。

結局５分の仕事が二日間かかりましたが、面白かった。

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

予備
----

`LANGUAGE`プラグマは、意外と多くなりました。
まず、いつもの`OverloadedStrings`と、
diagramsのドキュメンテーションにおすすめされる`NoMonomorphismRestriction`。

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

実は`NoMonomorphismRestriction`は今回はいらなかったけど、
あった方がいいと書いてあったから入れてみました。

フォントを設定するためのデータ構造で、`Functor`と`Traversable`の関数を使いたいです。

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}

（`Data.Map`でもよかったかもしれないけど…）

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

今回のプロジェクトは殆どの型はdiagramsで定義されている。

一つだけを自分で定義する。それは、フォントを設定するための型。

> data Fonts a 	= Fonts
>              	{ english  :: a
>              	, numbers  :: a
>              	, japanese :: a
>              	} deriving (Functor, Foldable, Traversable)

なぜわざわざ多相型を定義して、`String`ばっかりにinstantiateするのか？

実は、同じストラクチャーで、ロードする前のファイル名と、
準備ができた使える状況のフォントを入れたいのです。
今は両方とも`String`になるけど、SVGFonts 1.4.0.4から、
`PreparedFont`になる。

> loadPhoto :: FilePath -> IO (Diagram B R2)
> loadPhoto fp = either reportError (return . image) =<< loadImageEmb fp
>   where reportError err = putStrLn err >> exitWith (ExitFailure 1)

> isosceles :: Angle -> Double -> Diagram B R2
> isosceles θ h = polygon (with &
>   polyType .~ PolySides
>   [ (180 @@ deg) ^-^ θ ]
>   [ legLength, legLength ]
>   ) where legLength = h / cosA (θ^/2)

> isoscelesBase :: Angle -> Double -> Double
> isoscelesBase θ h = 2 * tanA (θ^/2) * h

> topImage :: Angle -> Diagram B R2 -> Diagram B R2
> topImage θ photo 	= triangles <> photo
>                  	# clipBy (rect (ttb - 2) (ph - 2)) where

>   ph   	= height photo
>   hh   	= ph / 2
>   qh   	= ph / 4

>   mt h 	= isosceles θ h
>        	# centerXY
>        	# lc white
>        	# lw ultraThick

>   tt   	= mt ph # reflectY
>   ttb  	= isoscelesBase θ ph

>   bt   	= mt hh # translateY (-qh)
>   brt  	= bt # translateX bto
>   blt  	= reflectX brt
>   bto  	= ttb / 4

>   θ'   	= (180 @@ deg) ^-^ θ
>   et   	= isosceles θ' bto
>        	# centerXY
>        	# lw none
>        	# fc white
>        	# translateY (bto / 2)
>        	# rotate (90 @@ deg)
>   etr  	= et # translateX (bto * 2)
>   etl  	= reflectX etr

>   triangles = etl <> blt <> tt <> brt <> etr

> message :: Fonts String -> Double -> Double -> Diagram B R2
> message fs w h = center messageText where
>   text' f (a, d) l s = stroke (textPath s f a d) # lw l # fc black <> strutY d
>   textPath s f a d = textSVG' $ TextOpts s (outlMap $ f fs) INSIDE_WH KERN False a d
>   proportions = [ (0, 6), (w * 9 / 10, 10), (0, 1), (w / 6, 6)
>                 , (0, 4), (w * 33 / 40, 4), (w * 33 / 40, 5) ]
>   totalHeight = sum (map snd proportions)
>   part n      = second ((/ totalHeight) . (* h)) $ proportions !! (n - 1)
>   space       = strut . r2
>   messageText = space   (part 1)
>     === text'  english  (part 2) thin (map toUpper "Happy New Year")
>     === space           (part 3)
>     === text'  numbers  (part 4) none "2015"
>     === space           (part 5)
>     === text'  japanese (part 6) none "昨年はお世話になりました　今年もよろしくお願いします"
>     === text'  english  (part 7) none "Wishing you a fantastic New Year, from Aki & Dani"

> nengajou :: Fonts String -> Diagram B R2 -> Diagram B R2
> nengajou fs photo = topImage θ photoShiftedRight # inViewport
>                   === message fs imageWidth (photoHeight / 3)
>   where θ                  = 50 @@ deg
>         photoHeight        = height photo
>         imageWidth         = isoscelesBase θ photoHeight
>         photoShiftedRight  = photo # translateX 40
>         inViewport         = view (p2 ((-imageWidth) / 2, (-photoHeight) / 2))
>                                   (r2 (imageWidth, photoHeight))

> prepareFonts :: Fonts String -> IO (Fonts String)
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
>     return f'

> stripNamespaceLineFrom :: FilePath -> IO ()
> stripNamespaceLineFrom f = TIO.readFile f >>= go where
>   go 	= T.words
>      	>>> filter (not . T.isInfixOf "xmlns")
>      	>>> T.unwords
>      	>>> TIO.writeFile f

> main :: IO ()
> main = do
>   photo <- loadPhoto "static/images/2015-01-01-happy-new-year/beach-club.png"
>   fonts <- prepareFonts Fonts 	{ english  = "/Library/Fonts/Microsoft/Garamond.ttf"
>                               	, numbers  = "/Library/Fonts/Microsoft/Calisto MT"
>                               	, japanese = "/Library/Fonts/Microsoft/ＤＦＰ教科書体W3" }
>   mainWith . bg white . pad 1.1 $ nengajou fonts photo

[diagrams]: http://projects.haskell.org/diagrams
[XQuartz]:  https://xquartz.macosforge.org/
