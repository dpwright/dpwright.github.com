---
date: 2014-12-29 14:59:26
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Elastic Tabstops
---

> {-# LANGUAGE OverloadedStrings #-}
> module ElasticTabstops where

> import Text.Pandoc.Definition
> import Text.Pandoc.Walk

> import Data.List
> import Data.List.Split

> elasticTabstops :: Pandoc -> Pandoc
> elasticTabstops = walk $ ifCodeBlock elasticate

> ifCodeBlock :: (Attr -> String -> Block) -> Block -> Block
> ifCodeBlock f (CodeBlock a s)	= f a s
> ifCodeBlock _ b              	= b

> elasticate :: Attr -> String -> Block
> elasticate a s =
>   Div ([], ["elastic-tabstops"], []) $ map makeTable groups
>   where
>     groups                      	= foldr groupMaker [] $ lines s
>     makeTable (n, g)            	= Table [] (allLeft n) (columnWidths n) [] $ map makeRow g
>     makeRow                     	= map (:[]) . zipWith ($) codeRow . splitOn "\t"
>     allLeft n                   	= replicate (n+1) AlignLeft
>     columnWidths n              	= replicate (n) 0 ++ [1]
>     removeLiterate (i, cs, kvs) 	= (i, delete "literate" cs, kvs)
>     codeRow                     	= map CodeBlock $ a:map removeLiterate (repeat a)

> countNumTabs :: String -> Int
> countNumTabs = length . filter (== '\t')

> groupMaker :: String -> [(Int, [String])] -> [(Int, [String])]
> groupMaker l []	= [(countNumTabs l, [l])]
> groupMaker l ((n, ls):gs)
>   | lts == n 	= (n, l:ls):gs
>   | otherwise	= (lts, [l]):(n, ls):gs
>   where lts = countNumTabs l
