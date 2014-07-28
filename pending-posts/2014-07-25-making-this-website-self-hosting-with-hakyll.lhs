---
date: 2014-07-25 08:38:23
tags: literate-programs, hakyll
title: Making this website self-hosting with Hakyll
---

Preliminaries
-------------

> {-# LANGUAGE OverloadedStrings #-}
>
> module Main where
>
> import           Data.Monoid         ((<>))
> import           Data.Maybe          (fromMaybe)
> import           Data.List           (intercalate)
> import           Data.Char           (toLower, isAlphaNum)

> import           Control.Applicative (liftA2, (<$>), (<*>))
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

Site builder
------------

> main :: IO ()
> main = hakyll $ do
>   simpleRules
>   generateTags >>= taggedRules

> simpleRules :: Rules ()
> simpleRules = do
>   templates
>   images
>   static
>   css

> generateTags :: Rules Tags
> generateTags = buildTags "posts/*" $ fromCapture "tags/*.html"

> taggedRules :: Tags -> Rules ()
> taggedRules = posts & tagIndex & index & feed
>   where (&) = liftA2 (>>)

Some simple rules
-----------------

> templates, images, css, static :: Rules ()
>
> templates = match "templates/**" $ compile templateCompiler
>
> images = match "images/*" $ do
>   route   idRoute
>   compile copyFileCompiler
>
> css = match "css/*" $ do
>   route   idRoute
>   compile compressCssCompiler
>
> static = match "static/**" $ do
>   route $ gsubRoute "static/" (const "")
>   compile copyFileCompiler

Compiler Settings
-----------------

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

Posts
-----

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
>   constructName <$> field "title" <*> field "date"
>   where constructName t d = setBaseName $ date d ++ "-" ++ title t
>         field  = (`M.lookup` meta)
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

Index pages
-----------

> itemCtx :: Tags -> Context String
> itemCtx tags = dateField "date" "%e %B, %Y"
>             <> defaultContext

> postList :: Tags
>          -> Pattern
>          -> ([Item String] -> Compiler [Item String])
>          -> Compiler String
> postList tags pattern sortFilter = do
>   posts   <- sortFilter =<< loadAll pattern
>   itemTpl <- loadBody "templates/post-item.html"
>   applyTemplateList itemTpl (itemCtx tags) posts
>
> indexCtx :: Context String -> Tags -> String -> Context String
> indexCtx base tags list = constField "posts" list <> base
>
> indexCompiler :: Tags -> Pattern -> Context String -> Compiler (Item String)
> indexCompiler tags pattern baseCtx = do
>   ctx <- indexCtx baseCtx tags <$> postList tags pattern recentFirst
>   makeItem "" >>= loadAndApplyTemplate "templates/archive.html" ctx
>               >>= loadAndApplyTemplate "templates/default.html" ctx
>               >>= relativizeUrls
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \tag pattern -> do
>   route idRoute
>   compile $ indexCompiler tags pattern defaultContext
>
> index :: Tags -> Rules ()
> index tags = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler tags "posts/*" baseContext
>   where baseContext = constField "title" "" <> defaultContext

Atom feed
---------

> feedConfig :: FeedConfiguration
> feedConfig = FeedConfiguration
>            { feedTitle       = "Wright Access"
>            , feedDescription = "dpwright's notes on code, Japan, Japanese, and anything else"
>            , feedAuthorName  = "Daniel P. Wright"
>            , feedAuthorEmail = "dani@dpwright.com"
>            , feedRoot        = "http://dpwright.com"
>            }

> feed :: Tags -> Rules ()
> feed tags = create ["atom.xml"] $ do
>   route idRoute
>   compile $ do
>     let feedCtx = postCtx tags
>                <> bodyField "description"
>     posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
>     renderAtom feedConfig feedCtx posts

Cross-posting
-------------

> getCrosspostHeader :: String -> Identifier -> Compiler String
> getCrosspostHeader key n = getMetadata n >>= toHeader . M.lookup key
>   where loadHeader        = fmap itemBody . header
>         toHeader          = maybe (return "") loadHeader
>         header name       = makeItem ""
>                         >>= loadAndApplyTemplate (templatePath name) xpContext
>         templatePath name = fromFilePath $ "templates/xp/" ++ name ++ ".html"
>         xpContext         = defaultContext
>
> crosspostField :: String -> Context a
> crosspostField key = field key $ getCrosspostHeader key . itemIdentifier
