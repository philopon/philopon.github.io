---
title: Hakyllでgithub.ioにブログ作った
date:  2014-02-25T23:50:52Z
tags:  hakyll, haskell 
---

テスト投稿を兼ねてこの記事を書いている時の操作について書いておきます。

1. Hakyllに`watch`引数を与えて起動する。
2. おもむろに記事を書く。`localhost:8000`でプレビューできる。
3. 完成したら[デプロイスクリプト](https://github.com/philopon/philopon.github.io/blob/sources/deploy.sh)を実行して記事&元のmarkdownファイルをデプロイ。

デプロイスクリプトでは、

1. `site.hs rebuild`で再ビルド
2. `_site`から`rsync`で`deploy`ディレクトリにコピー
3. `deploy`ディレクトリ(remote originをphilopon.github.ioに、branchをmasterにしてる)に移動して全ファイルをgithub pagesのmasterブランチにpush
4. ルートディレクトリに戻って`posts`以下をsourcesブランチにpush

みたいな操作をしております。`set -e`してるので3で新しいファイルが無かったりするとそこで止まってくれます。`set -e`便利。
