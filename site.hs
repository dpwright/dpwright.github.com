--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Text.Pandoc.Options
import           Hakyll

import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
customCompiler = pandocCompilerWith readerOptions writerOptions
  where readerOptions = def { readerSmart = True }
        writerOptions = def
                      { writerHTMLMathMethod = MathJax ""
                      , writerHighlight      = True
                      , writerExtensions     = writerExts
                      }
        writerExts    = writerExtensions def `S.union` S.fromList
                      [ Ext_literate_haskell
                      ]

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            let archiveCtx =
                    constField "posts" list                    `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html"
                        (constField "title" title `mappend`
                         constField "posts" list  `mappend`
                         defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            let indexCtx = constField "posts" list          `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    match "templates/xp/*" $ compile templateCompiler


--------------------------------------------------------------------------------
getCrosspostHeader :: String -> Identifier -> Compiler String
getCrosspostHeader key id' = getMetadata id' >>= toHeader . M.lookup key
  where loadHeader = fmap itemBody . header
        toHeader = maybe (return "") loadHeader
        header name = makeItem "" >>= loadAndApplyTemplate (templatePath name) xpContext
        templatePath name = fromFilePath $ "templates/xp/" ++ name ++ ".html"
        xpContext = defaultContext

crosspostField :: String -> Context a
crosspostField key = field key $ getCrosspostHeader key . itemIdentifier

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , crosspostField "xp"
    , defaultContext
    ]


--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl (postCtx tags) posts
