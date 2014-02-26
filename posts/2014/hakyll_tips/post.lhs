---
title: Hakyll tips
date:  2014-02-26T16:10:05Z
tags:  haskell, hakyll, tips
---

lhs文書のテストも兼ねて、ここを作るときに使ったあれこれを書き出しておきます。随時更新。

もくじ
====
* [ハイライトのcssどうするの](#ハイライトのcssどうするの)
* [元々minifyされているcssはライセンス書いてるしcomplessしたくない](#元々minifyされているcssはライセンス書いてるしcomplessしたくない)
* [タグごとのページもpaginateしたい](#タグごとのページもpaginateしたい)
* [表にはtableクラスを付けないとbootstrapが！](#表にはtableクラスを付けないとbootstrapが！)

 <!-- more -->

> {-#LANGUAGE OverloadedStrings #-}
> import           Hakyll
> import qualified Text.Highlighting.Kate as Kate
> import qualified Text.HTML.TagSoup      as TS
> import           Data.List(partition)
>
> main :: IO ()
> main = hakyll $ highlightCss >> cssRules >> posts

ハイライトのcssどうするの
====
`pandocCompiler`で文書をコンパイルすると、コードにはハイライト用のマークアップを付けてくれますが、cssは自分で用意する必要があります。
このハイライト機能は[`highlighting-kate`](http://hackage.haskell.org/package/highlighting-kate)により提供されているものなので、その中に定義されている[スタイル](http://hackage.haskell.org/package/highlighting-kate-0.5.6.1/docs/Text-Highlighting-Kate-Styles.html)を使って、以下の様なルールを書いておけば良いと思います。

> highlightCss :: Rules ()
> highlightCss = create ["css/highlight.css"] $ do
>     route idRoute
>     compile $ makeItem (Kate.styleToCss Kate.kate)

元々minifyされているcssはライセンス書いてるしcomplessしたくない
====
`Patterns`用の論理演算を使いましょう。

Bool   Patterns
------ ---------------
&&     .&&.
||     .||.
not    complement

> cssRules :: Rules ()
> cssRules = do
>     match "css/*.min.css" $ do
>         route idRoute
>         compile copyFileCompiler
>     match ("css/*" .&&. complement "css/*.min.css") $ do
>         route idRoute
>         compile compressCssCompiler

タグごとのページもpaginateしたい
=====

普通にネストできます。すごい。
長いので[ソースコード](https://github.com/philopon/philopon.github.io/blob/sources/site.hs)の107行目あたり参照

表にはtableクラスを付けないとbootstrapが！
====

withTagsを使いましょう。

> addClass :: String -> TS.Tag String -> TS.Tag String
> addClass cls (TS.TagOpen name attr) = case partition ((== "class") . fst) attr of
>     ([],         _)     -> TS.TagOpen name $ ("class", cls) : attr
>     ((_,cls'):_, attr') -> TS.TagOpen name $ ("class", cls ++ ' ': cls') : attr'
> addClass _ tag = tag
> 
> postCompiler :: Compiler (Item String)
> postCompiler = fmap (withTags process) `fmap` pandocCompiler
>   where process tag | TS.isTagOpenName "table" tag = addClass "table" tag
>                     | otherwise                    = tag
>
> posts :: Rules ()
> posts = match "posts/**.md" $ do
>     route $ setExtension "html"
>     compile postCompiler
>







