#Haskellの単体テスト最前線

[[To English]](../en/tutorial.md)

これは[Haskell Advent Calendar 2012](http://partake.in/events/45a01d39-af5e-42f1-91c7-e8fcc91db244)の5日目の記事です。

Haskellで作成したパッケージに対して、単体テストを書くための最新情報をお届けします。

##要約

要点は4つです。

+ 利用者に見せたい振る舞いは、[doctest](https://github.com/sol/doctest-haskell#readme) で書く
+ 利用者に見せたくない振る舞いは、[hspec](http://hspec.github.com/) で書く
+ テストを自動化するフレームワークとしては Cabal を使う
+ doctest でも hspec でも、純粋なコードに対しては、できるだけ QuickCheck などの性質テストを書く

この記事で一番伝えたいのは、3) です。例題としては、Base64 という符号化を取り上げます。Base64 は知っていると仮定して話を進めますので、知らない人はあらかじめ [Wikipedia の Base64 の説明](http://ja.wikipedia.org/wiki/Base64)でも読んで下さい。

この記事で利用するコードの全体は、[unit-test-example](https://github.com/kazu-yamamoto/unit-test-example)というパッケージ名でgithubに置いてあります。以降の例では、このパッケージのトップディレクトリにいると想定しています。

##doctest

利用者に見せたい振る舞いは、doctest で書きます。そうすれば、マニュアルにもなりますし、テストにも使えます(本当は設計にも利用して欲しいのですが、今回は踏み込みません)。

Haskell のドキュメントツールは Haddock です。コメントに、マークアップを使って説明を書きます。">>>" というマークアップが、利用例のために用意されています。

unit-test-example にある Codec.Base64 の例を見てみましょう。

```haskell
-- |
-- Base64 encoding.
--
-- >>> encode "foo bar"
-- "Zm9vIGJhcg=="
encode :: String -> String
encode = ...

-- |
-- Base64 decoding.
--
-- >>> decode "Zm9vIGJhcg=="
-- "foo bar"
decode :: String -> String
decode = ...
```

">>>" の次の行には、結果を書きます。">>>" は GHCi のプロンプトだと思って構いません。すでに実装がある場合は、GHCi で対話的に動かした例をコピー＆ペーストして("Prelude>" などといった)プロンプトを ">>>" へ変更するだけです。

GHCiなので、let とかも使えます。

```haskell
-- >>> let xs="Zm9vIGJhcg=="
-- >>> decode xs
-- "foo bar"
```

これまでの例は純粋ですが、もちろん IO でもOKです。GHCi でできることは、すべてできます。

```haskell
-- >>> doesFileExist "/foo"
-- False
```

テストは単なる文字列比較なので、例外も書けます。

```haskell
-- >>> 1 `div` 0
-- *** Exception: divide by zero
```

詳しくは doctest のマニュアルを読んで下さい。

以下のコマンドで、"dist" の下に HTML のマニュアルを作成できます。

    % cabal haddock --hyperlink-source

マニュアルはこんな感じになります。

![マニュアルの例](https://raw.github.com/kazu-yamamoto/unit-test-example/master/markdown/img/haddock.png)

doctest でテストを実行してみましょう。

    % doctest Codec/Base64.hs 
    Examples: 2  Tried: 2  Errors: 0  Failures: 0

doctest をインストールしていない人は、以下のコマンドを実行してみて下さい。

    % cabal install --enable-test --only-dependencies

こうすれば、現在のパッケージをインストールすることなく、必要なライブラリ(コマンド)をインストールできます。

##hspec

利用者に見せてもしょうがない振る舞いに関しては、hspec で記述します。モジュールファイル1つに対し、1つのテストファイルを書くのが基本です。たとえば、Base64.hs に対しては Base64Spec.hs というファイルにテストを記述します。

ここで注意が必要なのは、テスト用のファイルは、ソースとは異なるディレクトリに置くべきであるということです。こうすると、後で説明する Cabal で、テストが依存するパッケージとして自分自身が書けるようになります。

意味が分からない人は、とにかく "test" というディレクトリを作って、テスト関係のファイルはそこに置くのだと覚えて下さい。

"test/Base64Spec" の記述例を以下に示します。

```haskell
spec :: Spec
spec = do
    describe "encode" $ do
        it "encodes multiples of 3" $
            encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"
        it "encodes multiples of 3 + 1" $
            encode "padding  2" `shouldBe` "cGFkZGluZyAgMg=="
        it "encodes multiples of 3 + 2" $
            encode "padding1" `shouldBe` "cGFkZGluZzE="

     describe "decode" $ do
        it "decodes no padding" $
            decode "bm8tcGFkZGluZyEh" `shouldBe` "no-padding!!"
        it "dncodes one padding" $
            decode "cGFkZGluZyAgMg==" `shouldBe` "padding  2"
        it "encodes two paddings" $
            decode "cGFkZGluZzE=" `shouldBe` "padding1"
```

このように hspec では、shouldBe など分かり易い単語を使って、テストケースを楽しく書けます。この「楽しい」という感覚がとても大事です。

上記の例は純粋ですが、もちろん IO も書けます。詳しい使い方は hspec のマニュアルを読んで下さい。shouldBe、shouldReturn、shouldThrow 要チェックです。

Spec は、hspec 関数で実行できます。

    % ghci test/Base64Spec.hs
    > hspec spec
    encode
      - encodes multiples of 3
      - encodes multiples of 3 + 1
      - encodes multiples of 3 + 2
    
    decode
      - decodes no padding
      - dncodes one padding
      - encodes two paddings
    
    Finished in 0.0956 seconds
    6 examples, 0 failures

##QuickCheck

hspec には QuickCheck の性質テストも記述できます。it を prop に変えれば OK です。

```hspec
spec :: Spec
spec = do
    describe "encode" $ do
        ...
        prop "reverses decoded string" $ \(Base64 xs) ->
            encode (decode xs) == xs
    
    describe "decode" $ do
        ...
        prop "reverses encoded string" $ \xs ->
            decode (encode xs) == xs
```

##Cabal

テストを自動実行するには、Cabal を使います。準備として、テストのための情報を Cabal ファイルに書く必要があります。

    Test-Suite doctest
      Type:                 exitcode-stdio-1.0
      Default-Language:     Haskell2010
      HS-Source-Dirs:       test
      Ghc-Options:          -threaded -Wall
      Main-Is:              doctests.hs
      Build-Depends:        base
                          , doctest >= 0.9.3
    
    Test-Suite spec
      Type:                 exitcode-stdio-1.0
      Default-Language:     Haskell2010
      Hs-Source-Dirs:       test
      Ghc-Options:          -Wall
      Main-Is:              Spec.hs
      Other-Modules:        Base64Spec
      Build-Depends:        base
                          , hspec >= 1.3
                          , QuickCheck
                          , unit-test-example

hspec のところで、依存するパッケージに自分自身(unit-test-example)が書けているのが分かりますか？ ディレクトリを分けないと、このパッケージが依存するライブラリを繰り返し書くはめになりますよ。

doctest では、これに加えて以下のような内容を "test/doctest.hs" ファイルに用意します。

```haskell
module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Codec/Base64.hs"]
```

doctest 関数の引数は、doctest コマンドに渡した引数を Haskell のリスト表記に直したものです。

hspec では、以下の一行を "test/Spec.hs" ファイルに書き込みます。

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Cabal で自動的にテストする手順は以下の通りです。

    % cabal configure --enable-tests 
    % cabal build
    % cabal test
    Running 2 test suites...
    Test suite spec: RUNNING...
    Test suite spec: PASS
    Test suite logged to: dist/test/unit-test-example-0.0.0-spec.log
    Test suite doctest: RUNNING...
    Test suite doctest: PASS
    Test suite logged to: dist/test/unit-test-example-0.0.0-doctest.log
    2 of 2 test suites (2 of 2 test cases) passed.

失敗したときは、表示されているログファイルを見て、失敗したケースとそのときの値を探して下さい。

Hspec のデフォルトでは QuickCheck の性質テストで乱数が100個生成されます。これを変更するには、以下のようにします。

    % cabal test --test-option=--maximum-generated-tests=1000

##Travis CI

Travis CI は、github に push すると、自動的にテストを走らせてくれるサービスです。利用するには、以下のような手順を踏みます。

- [Travis CI](https://travis-ci.org/)にgithubアカウントでログインします
- 右上にある自分の名前をクリックして "Accounts" を選び、アカウントページに行きます
- "Sync now" ボタンをクリックし、github のパッケージ(リポジトリ)情報を取ってきます
- Travis CI と連携させたいパッケージを選んで On にします

次に Travis CI と連携させたいパッケージのトップディレクトリで、
以下の一行を ".travis.yml" というファイルに書き込み、github へ push します。

    language: haskell

これで、このパッケージに push するごとに、Travis CI 上でテストが走り、結果がメールで通知されます。

##雑多なこと

###doctest と Mac

doctest は GHCi を利用して実装されています。残念ながら、Mac 上では GHCi が不安定です。このせいでテストの実行自体が失敗することがあります。以下に Mac ユーザーとしての僕の経験を書いておきます。

- doctestコマンドよりも cabal build してコンパイルした方が安定してる
- GHCi は、64ビット版よりも32ビット版の方が安定している
- GHCi 7.4.x よりも GHCi 7.6.x の方が安定している

安全な方を組み合わせてお使い下さい。

###doctest の引数

doctest の引数は、GHCi の引数とまったく同じです。doctest をうまく動かすには、いろいろな引数が必要になることがあります。よくある例としては、"-XOverloadedStrings" が挙げられます。

自分で書いたCのコードを、FFI でリンクしている場合は、指定すべき引数が分からなくて泣きそうになるかもしれません。その場合、[unix-time](https://github.com/kazu-yamamoto/unix-time) がまさにそういう例なので、参考にするとよいでしょう。

###doctest と haddock と QuickCheck

すでに haddock には "prop>" マークアップが用意されており、QuickCheck の性質を書けるようになっています。また、doctest もこれに対応しています。問題は、Haskell Platform に "prop>" 対応の haddock がいつ入るかです。

実は、haddock は GHC とともに配布されています。GHC は [haddock の "master" ブランチ](https://github.com/ghc/haddock)を機械的に利用します。"prop>" が実装されている haddock のブランチは、"ghc-7.6" です。ですので、"ghc-7.6" を "master" へマージしないといけません。現在、いくつかのテストが通らないのでマージされていませんが、近い将来マージされるでしょう。

###内部モジュール

テストのために非公開のコンストラクタを利用したくなることがあります。そのための常套手段は、内部モジュール(Internal.hs という名前が多い)を用意することです。この内部モジュールを読み込めば、コンストラクタが利用できるようになります。

Cabal のテスト機能で、この内部モジュールを利用するには、まず HS-Source-Dirs に親ディレクトリを加えます。

      HS-Source-Dirs:       test,..

これで test 以下のモジュールから、内部モジュールを import できるようになります。ただし、テストがライブラリ自身に依存できなくなるので、Build-Depends にはライブラリ自身が依存するライブラリを書かなければならなくなります。

##最後に

I wish you a merry Christmas and happy unit-testing!
