---
title: LlamaでNFCタグが使えないときの対処法
date:  2014-03-12T00:22:03Z
tags:  Android, Llama, NFC
---

Llama便利ですね！
もっと便利に使うためにNFCタグを買ってみたものの、初期化出来なくて約2,000円が無駄になったとさめざめと泣いていましたが、なんとか解決出来たので記しておきます。

![初期化画面](cant_format.png)

<!-- more -->

と言っても、任意のNFCタグライターでURLとして`llamaloc://llama.location.profiles/nfc`を書き込むだけです。

![書き込み](writing.png)

その後同様に以下の画面を表示させてNFCタグをタッチすると、

![タッチ待ち](please_touch.png)

おめでとうございます！タグを見つけられたようです！

![タグの命名](naming.png)

これを"テストタグ"と命名し、トーストメッセージを出す条件を入れてみます。

![テスト](test_condition.png)

無事、トーストメッセージが表示されました！

![OK](ok.png)

以上です。
