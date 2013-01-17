#Unit test in Haskell at the front

This article is a tutorial about unit test in Haskell using doctest, hspec and Cabal.

##Abstract

Important points are summarized as follows:

- The behaviors to show the users of your library should be written with [doctest](https://github.com/sol/doctest-haskell#readme)
- The behaviors not to show the users of your library should be written with [hspec](http://hspec.github.com/)
- As a test framework to automate testing, Cabal is used
- For pure code, properties test cases (with doctest and/or hspec) should be written as much as possible

In this tutorial,
we use the Base64 encoding/decoding as an example.
You are supposed to know what they are.
If you don't know them,
please read [the explanation about Base64 in wikipedia](http://en.wikipedia.org/wiki/Base64) beforehand.

The package includes the examples of this tutorial is available on github.
Its package name is [unit-test-example](https://github.com/kazu-yamamoto/unit-test-example).
It is assumed that we are at the unit-test-example directory henceforth.

##doctest

The behaviors to show the users of your library should be written with doctest.
If you do so,
they can be documentations and can be used for testing.
(I hope you also use doctest for design
but this article does not talk about this.)

The documentation tool of Haskell is Haddock.
With Haddock, documentations are written in comments with markups.
For usage examples, the ">>>" markup is prepared.

Let's look at an example of Codec.Base64 in unit-test-example:

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

In the next line of ">>>",
its result value is written.
You can consider that ">>>" is the prompt of GHCi.
If a function is already defined,
you can copy & paste the interactive session of GHCi
then change the prompt of GHCi (such as "Prelude>") to ">>>".

You can use "let" because it is GHCi's session.

    -- >>> doesFileExist "/foo"
    -- False

Since test is done by simply comparing strings,
you can describe exceptions:

    -- >>> 1 `div` 0
    -- *** Exception: divide by zero

For more information,
please refer to the manual of doctest.

You can create an HTML manual under the "dist" directory
with the following command:

    % cabal haddock --hyperlink-source

Here is an example of image of a manual:

![An example of manual](https://raw.github.com/kazu-yamamoto/unit-test-example/master/markdown/img/haddock.png)

Let's run test with the doctest command:

    % doctest Codec/Base64.hs
    Examples: 2  Tried: 2  Errors: 0  Failures: 0

If you don't install doctest yet, please execute the following command:

    % cabal install --enable-test --only-dependencies

With this command,
you can install necessary libraries and commands
without installing the current package.

##hspec

The behaviors not to show the users of your library
should be written with hspec.
Basically, one test file should be prepared for earch module.
For instance, Base64Spec.hs is created for Base64.hs.

It is important to note that
test files should be placed
in the different directory from that of source files.
If you obey this rule,
you can specify your library as a dependency of test suite in a Cabal file,
which is described later.

If you don't understand what this means,
please remember one rule:
making the "test" directory and
all test files are placed there.

The following is an example of "test/Base64Spec":

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

As you see,
you can write test cases with easy-to-understand words such as shouldBe
in a fun manner.
The sense of fun is crucial.

Though the examples above are pure,
you can write test cases for IO.
For more information,
please refer to the manual of hspec.
You should carefully check shouldBe, shouldReturn, and shouldThrow
(they are called "matcher" in RSpec terminologies).

You can run Spec with the hspec function:

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

Properties of QuickCheck can be specified in hspec, too.
Just use "prop" instead of "it":

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

##Cabal

To automate running test suites, use Cabal.
You need to specify information about test suites
in a Cabal file:

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

Please note that the library itself (unit-test-example in this case) can
be specified as dependency in the test suite for hspec.
If you store test files in the same directory of source code,
you have to repeat the dependencies of the library here.

For doctest,
"test/doctest.hs" should be created in addition to the Cabal file:

    module Main where
    
    import Test.DocTest
    
    main :: IO ()
    main = doctest ["Codec/Base64.hs"]

The arguments of the "doctest" function is
the same as that of the "doctest" command
(which is also identical to that of GHCi).
They should be stored in a list literal of Haskell.

For hspec,
the following one line should be stored in "test/Spec.hs":

    {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

The procedure to automatically run test suites is as follows:

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

If tests fail, please read the indicated log files
and search the failure cases and their values.

100 randam values are generated for one property with the default of hspec.
If you want to change it, do as follows:

    % cabal test --test-option=--maximum-generated-tests=1000

##Travis CI

Travis CI is a service to automatically run test suites
when you push your commits to github.
To use Travis CI, you need to set it up as follows:

- Login into [Travis CI](https://travis-ci.org/) with you gihub account
- Select "Accounts" by clicking your name on the right top to go to your account page
- Click the "Sync now" button to retrieve package (repository) information from github
- Enabling the service for necessary packages

Then you need to create ".travis.yml" which stores the following one line:

    language: haskell

Then push this file to github.
After that,
test suites will be run on Travis CI when you push your commits
and a result is delivered to you by e-mail.

##Miscellaneous things

###doctest and Mac

###The arguments of doctest

###doctest, haddock and QuickCheck

##Internal modules

##Remark

Happy unit-testing in Haskell!
