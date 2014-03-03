---
title: Haskyllで引用機能を使う
date:  2014-03-04T01:33:19Z
tags:  hakyll, bibtex, haskell, pandoc
---

結論
=========
やめておきましょう。footnote使いましょう。

...だけではアレなので、供養を兼ねて一応使い方を書いておきます。

<!-- more -->

CSLファイルを取得する
-----------
HakyllでHTMLへの変換に使用しているpandocでは、Citation Style Language(CSL)を使って引用文献の表示をカスタマイズできます。
[citation-style-language/styles](https://github.com/citation-style-language/styles)にしこたまあるので、適当な物をコピーするなり、cloneするなり、submoduleにするなりしましょう。

ルールを追加する
----------
`templateCompiler`と同じように適当にルールを追加します。

```{.haskell .numberLines}
main = hakyll $ do
    match "style.csl"        $ cslCompiler
    match "bibliography.bib" $ biblioCompiler
```

コンパイラを書き換える
---------
`pandocCompiler`のソースを参考に、`readPandocBiblio`を使って適当にコンパイラを書き換えましょう。

```{.haskell .numberLines startFrom="4"}
     match "posts/*.md" $ do
        route idRoute
        compile $ do
            csl <- load "style.csl"
            bib <- load "bibliography.bib"
            cached "myPostCompiler" $ getResourceBody
                >>= readPandocBiblio defaultHakyllReaderOptions csl bib
                >>= return . writePandoc
```

完成！....ではなく
----------
これでよいならまだ「やめておきましょう」とまでは言わないのですが、このままだと`Reference`を再読込する時にread関数でコケます[^when][^can]。

[^when]: 2回目以降のbuildとか、watch中とか。
[^can]: 毎回再コンパイルするなら、このまま使えます。

これは`Biblio`の`Binary`インスタンスが`Reference`のshow/readを使用して実装されており[@binary]、`Reference`のread/showが逆関数になっていないためです[^where]。

[^where]: どこが腐っているかの判断は各自にお任せします。

そこで、取り敢えずshow/readではなくAesonインスタンスを使ってこれを回避します。

回避
------------
[Bib.hs](https://github.com/philopon/philopon.github.io/blob/sources/Bib.hs)
これをプロジェクトルートにでも置いて`import qualified Bib`{.haskell}しましょう。
趣味で`Monoid`インスタンスも足してますが、本筋は`Aeson`を使うようにした以外は変わりません。

で、`Biblio`の部分と、

```{.haskell .numberLines startFrom="3"}
     match "bibliography.bib" $ Bib.compiler
```

`compile`の部分を書き換えましょう。

```{.haskell .numberLines startFrom="10"}
                >>= readPandocBiblio defaultHakyllReaderOptions csl (Bib.cast bib)
```

完成！
------------
これだけ汚く回避してもハイパーリンク張れないのが(私にとっては)致命的なのでfootnote使った方が幸せでは、って感じます。

以下に引用のサンプルを付けておきます。

# 参考


