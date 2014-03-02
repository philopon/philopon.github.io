---
title: runsandbox作った
date:  2014-02-26T00:53:15Z
tags:  haskell, cabal
---

<div class="alert alert-danger">
  [zshでcabal-sandboxの状態を表示する](/posts/2014/cabal-sandbox-zsh.html)を見た方が良いかも知れません。
</div>

概要
====

cabal runの立ち上がりが遅いのでrunhaskellを使うrunsandboxを作った

<!-- more -->

動機
====

`cabal sandbox`使ってますか？私は使ってませんでした。

プロジェクト毎にコンパイルするの、特にyesodとかの巨大パッケージだと死ねます。まぁそれは最初の一回だけなので我慢しましょう。ぐっと我慢して使い始めてさあ実行してみよう、と`cabal run`すると……

```{.sh}
$ time cabal run clean
Package has never been configured. Configuring with default flags. If this
fails, please run configure manually.
Resolving dependencies...
Configuring githubio-0.1.0.0...
Preprocessing executable 'githubio' for githubio-0.1.0.0...
[1 of 1] Compiling Main             ( site.hs, dist/build/githubio/githubio-tmp/Main.o )
Linking dist/build/githubio/githubio ...
Removing _site...
Removing _cache...
Removing _cache/tmp...
cabal run clean  10.36s user 1.25s system 92% cpu 12.613 total
```

なんと言うことでしょう。10秒以上待たされました。なんといちいちコンパイルしているのです。
これでは私のようなちょっと書いて実行してみて考える、残念な人にとっては使い物になりません。

解決法
==========

幸いにして`cabal.sandbox.config`のpackage-dbを食わせてやれば`runhaskell`でもsandbox中のパッケージを使えるようです。
```{.sh}
$ grep package-db cabal.sanbox.config
package-db: `pwd`/.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d

$ time runhaskell -no-user-package-db -package-db=`pwd`/.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d site.hs clean
Removing _site...
Removing _cache...
Removing _cache/tmp...
runhaskell -no-user-package-db  site.hs clean  1.63s user 0.30s system 96% cpu 1.989 total
```

ので、これを簡単に実行するプログラムを書きました。便利！

```{.sh}
$ time runsandbox site.hs clean
Removing _site...
Removing _cache...
Removing _cache/tmp...
runsandbox site.hs clean  1.59s user 0.27s system 98% cpu 1.894 tota
```

ソースは[こちら](https://github.com/philopon/runsandbox)

