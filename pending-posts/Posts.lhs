---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part 2: Posts
---

> {-# LANGUAGE OverloadedStrings #-}
>
> module Posts where

> import           Data.Monoid         ((<>))
> import           Data.Maybe          (fromMaybe)
> import           Data.List           (intercalate)
> import           Data.Char           (toLower, isAlphaNum)

> import           Control.Applicative ((<$>), (<*>))
> import           Control.Monad       (msum)

> import           System.FilePath     (replaceBaseName)
> import           System.Locale       (defaultTimeLocale)
> import           Data.Time.Clock     (UTCTime (..))
> import           Data.Time.Format    (formatTime, parseTime)

> import           Text.Pandoc.Options (ReaderOptions(..), WriterOptions (..),
>                                       Extension (..), HTMLMathMethod(..),
>                                       pandocExtensions, def)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import           Hakyll
> import           Crossposting

> customCompiler :: Compiler (Item String)
> customCompiler = pandocCompilerWith readerOptions writerOptions
>   where readerOptions = def
>                       { readerSmart          = True
>                       }
>         writerOptions = def
>                       { writerHTMLMathMethod = MathJax ""
>                       , writerHighlight      = True
>                       , writerExtensions     = extensions
>                       }
>         extensions    = pandocExtensions `S.union` S.fromList
>                       [ Ext_literate_haskell
>                       ]

> postCtx :: Tags -> Context String
> postCtx tags = dateField "date" "%e %B, %Y"
>             <> tagsField "tags" tags
>             <> crosspostField "xp"
>             <> defaultContext

> postCompiler :: Tags -> Compiler (Item String)
> postCompiler tags = customCompiler
>                 >>= loadAndApplyTemplate "templates/post.html"    ctx
>                 >>= saveSnapshot "content"
>                 >>= loadAndApplyTemplate "templates/default.html" ctx
>                 >>= relativizeUrls
>   where ctx = postCtx tags

> posts :: Tags -> Rules ()
> posts tags = match "posts/*" $ do
>   route $ metadataRoute dateAndTitle `composeRoutes` setExtension ".html"
>   compile $ postCompiler tags

> dateAndTitle :: Metadata -> Routes
> dateAndTitle meta = fromMaybe idRoute $
>   constructName <$> getField "title" <*> getField "date"
>   where constructName t d = setBaseName $ date d ++ "-" ++ title t
>         getField  = (`M.lookup` meta)
>         date   = formatTime defaultTimeLocale "%Y-%m-%d" . readTime
>         title  = map toLower . intercalate "-" . map (filter isAlphaNum) . words

> readTime :: String -> UTCTime
> readTime t = fromMaybe empty' . msum $ attempts
>   where attempts = [parseTime defaultTimeLocale fmt t | fmt <- formats]
>         empty'   = error $ "Could not parse date field: " ++ t
>         formats  = [ "%a, %d %b %Y %H:%M:%S %Z"
>                    , "%Y-%m-%dT%H:%M:%S%Z"
>                    , "%Y-%m-%d %H:%M:%S%Z"
>                    , "%Y-%m-%d %H:%M"
>                    , "%Y-%m-%d"
>                    , "%B %e, %Y %l:%M %p"
>                    , "%B %e, %Y"
>                    , "%b %d, %Y"
>                    ]


> setBaseName :: String -> Routes
> setBaseName basename = customRoute $ (`replaceBaseName` basename) . toFilePath

