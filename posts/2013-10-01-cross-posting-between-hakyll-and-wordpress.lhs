---
date: 2013-10-01 10:41:25
tags: code, haskell, functional-programming, hakyll, wordpress
title: Cross-posting between Hakyll and Wordpress
---

> {-# LANGUAGE OverloadedStrings #-}

> import Text.Pandoc.Definition
> import Text.BlogLiterately

> import Control.Lens
> import Control.Monad
> import Control.Monad.State

> import System.IO
> import System.IO.Temp
> import System.IO.Silently
> import System.Directory

> import Data.List
> import Data.List.Split
> import Data.Char (isSpace)

> import           System.Console.CmdArgs        (cmdArgs)
> import qualified System.IO.UTF8                as U (readFile)

> trim :: String -> String
> trim = f . f where f = reverse . dropWhile isSpace

> hakyllHeaderXF :: Transform
> hakyllHeaderXF = Transform hakyllize always
>   where always = const True

> hakyllize :: StateT (BlogLiterately, Pandoc) IO ()
> hakyllize = modify $ removeMetadata . readAndApplyMetadata

> removeMetadata :: (BlogLiterately, Pandoc) -> (BlogLiterately, Pandoc)
> removeMetadata (bl, (Pandoc m (_:content))) = (bl, Pandoc m content)

> readAndApplyMetadata :: (BlogLiterately, Pandoc) -> (BlogLiterately, Pandoc)
> readAndApplyMetadata (bl, p) = (execState (applyOptionsFromMetadata meta) bl, p)
>   where meta = readMetadata p

> applyOptionsFromMetadata :: [(String, String)] -> State BlogLiterately ()
> applyOptionsFromMetadata meta = do
>     profile ?= "xp"
>     postid ?= "xpid"
>     title ?= "title"
>     
>     tagsInOptions <- liftM (view tags) get
>     let tagsInFile = maybe "" id $ the "tags"
>     let allTags = union tagsInOptions $ unpackTags tagsInFile
>     modify $ set tags allTags
>   where
>     the = flip lookup meta
>     l ?= n = modify $ l `over` maybe (the n) Just
>     unpackTags s = map trim $ splitOn "," s

> getMetadataTable :: Pandoc -> [[TableCell]]
> getMetadataTable (Pandoc _ ((Table _ _ _ _ cells):_)) = cells

> inlineExtract :: Inline -> String
> inlineExtract Space = " "
> inlineExtract (Str s) = s
> inlineExtract c = show c

> readMetadata :: Pandoc -> [(String, String)]
> readMetadata p = map (getMeta . getMetaText) $ getMetadataTable p
>   where getMetaText = head . head
>         getMeta (Plain metaText) = parseMeta metaText
>         parseMeta (n:c) = (getName n, getContent c)
>         getName = init . inlineExtract
>         getContent = trim . extractAll
>         extractAll l = foldl' concatContent "" l
>         concatContent s c = s ++ (inlineExtract c)

> blogLiteratelyVeryCustom :: [Transform] -> IO String
> blogLiteratelyVeryCustom ts =
>       cmdArgs blOpts
>   >>= \bl -> U.readFile (file' bl)
>   >>= xformDoc bl ts
>   >>= uncurry postIt
>   >>  return (file' bl)

> main :: IO ()
> main = do
>     (output,f) <- capture $ blogLiteratelyVeryCustom xforms
>     case output of
>       x | postIdPrefix `isPrefixOf` x -> writeIdToFile f $ getPostId x
>         | otherwise                 -> putStr x
>   where xforms = hakyllHeaderXF:standardTransforms
>         postIdPrefix = "Post ID: "
>         getPostId = read . drop (length postIdPrefix)

> writeIdToFile :: FilePath -> Integer -> IO ()
> writeIdToFile f pid = withSystemTempDirectory "hakyll" $ \tmpDir -> do
>     (fTmp, hTmp) <- openTempFile tmpDir "post"
>     withFile f ReadMode $ \hIn -> do
>       inp <- hGetContents hIn
>       let (hdr, body) = splitHeader inp
>       let hdr' = if (tagDefined "xpid" hdr) then hdr else addPostId hdr pid
>       hPutStr hTmp $ hdr' ++ body
>     hClose hTmp
>     copyFile fTmp f

> splitHeader :: String -> (String, String)
> splitHeader contents = (hdr, body)
>   where hdr = unlines $ take hdrLength contentLines
>         body = unlines $ drop hdrLength contentLines
>         hdrLength = (elemIndices "---" contentLines) !! 1
>         contentLines = lines contents

> tagDefined :: String -> String -> Bool
> tagDefined tag hdr = not $ null $ filter (tag `isPrefixOf`) $ lines hdr

> addPostId :: String -> Integer -> String
> addPostId hdr pid = unlines $ init hdrLines ++ [postIdTag, last hdrLines]
>   where postIdTag = "xpid: " ++ show pid
>         hdrLines = lines hdr
