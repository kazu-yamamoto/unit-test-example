#Up-front Unit Testing in Haskell

This article is a tutorial about unit testing in Haskell using doctest, hspec and Cabal.

##Abstract

Important points are summarized as follows:

- The behaviors documenting the use of your library should be written with [doctest](https://github.com/sol/doctest-haskell#readme)
- The behaviors documenting functionality rather than usage of your library should be written with [hspec](http://hspec.github.com/)
- Cabal is used to automate testing with the frameworks
- For pure code, QuickCheck property test cases (with doctest and/or hspec) should be written as much as possible

In this tutorial,
we use Base64 encoding/decoding as an example.
It is assumed thay you know what it is.
If you don't know,
please read [the explanation about Base64 in wikipedia](http://en.wikipedia.org/wiki/Base64) beforehand.

The package included the examples of this tutorial is available on github.
The package name is [unit-test-example](https://github.com/kazu-yamamoto/unit-test-example).
It is assumed henceforth that we are at the "unit-test-example" directory.

    % git clone git://github.com/kazu-yamamoto/unit-test-example.git
    % cd unit-test-example

##doctest

The behaviors documenting the use of your library should be written with doctest.
If you do so,
they can be documentation and can also be used for testing.
(I hope that you also use doctest for design
but this article does is not about that.)

The Haskell documentation tool is [Haddock](http://www.haskell.org/haddock/doc/html/).
With Haddock, documentation is written in comments with markup.
For doctest usage examples, the `>>>` markup is used.

Let's look at an example of "Codec.Base64" in "unit-test-example":

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

The result value is placed in the line following `>>>`.
You can treat `>>>` as the GHCi prompt.
If a function is already defined,
you can copy & paste the interactive session from GHCi
and then change the GHCi prompt (e.g. `Prelude>`) to `>>>`.

You can use `let` because it is a GHCi session.

```haskell
-- >>> let xs="Zm9vIGJhcg=="
-- >>> decode xs
-- "foo bar"
```

Though all examples above are pure, you can use IO.
You can specify whatever GHCi can do.

```haskell
-- >>> doesFileExist "/foo"
-- False
```

Since test is done by simply comparing strings,
you can describe exceptions:

```haskell
-- >>> 1 `div` 0
-- *** Exception: divide by zero
```

For more information,
please refer to [the doctest manual](https://github.com/sol/doctest-haskell#readme).

You can create an HTML manual under the "dist" directory
with the following command:

    % cabal haddock --hyperlink-source

Here is an image of an example manual:

![An example of manual](https://raw.github.com/kazu-yamamoto/unit-test-example/master/markdown/img/haddock.png)

Let's run the tests with the "doctest" command:

    % doctest Codec/Base64.hs
    Examples: 2  Tried: 2  Errors: 0  Failures: 0

If you haven't installed doctest yet, please execute the following command:

    % cabal install --enable-test --only-dependencies

This command will install the necessary libraries and commands
without installing the current package.

##hspec

The behaviors documenting functionality rather than usage 
of your library should be written with hspec.
Basically, one test file should be prepared for each module.
For instance, "Base64Spec.hs" is created for "Base64.hs".

It is important to note that
test files should be placed
in a different directory from that of source files.
If you follow this rule,
you can specify your library as a dependency of the test suite in a Cabal file,
which is described later.

If you don't understand what this means,
please remember one rule:
Making a "test" directory and
place all test files there.

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

As you can see,
you can write test cases with easy-to-understand words such as `shouldBe`
in a fun manner.
The sense of fun is crucial.

Though the examples above are pure,
you can write test cases for IO.
For more information,
please refer to [the hspec manual](http://hspec.github.com/).
You should carefully check `shouldBe`, `shouldReturn` and `shouldThrow`
(they are called "matcher" in RSpec terminologies).

You can run Spec with the "hspec" function:

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

QuickCheck Properties can be specified in hspec, too.
Just use `prop` instead of `it`:

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
in the Cabal file:

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

Please note that your library itself ("unit-test-example" in this case) can
be specified as dependency in the test suite for hspec.
If you store test files in the same directory of source code,
you have to repeat the dependencies of your library here.

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
    
The "test/Spec.hs" file above will trigger a process to pick up 
all the tests in all the  hspec files in the same directory.

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

100 random values are generated for one property by default in hspec.
If you want to change it, do so as follows:

    % cabal test --test-option=--maximum-generated-tests=1000

##Travis CI

Travis CI is a service to automatically run test suites
when you push your commits to github.
To use Travis CI, you need to set it up as follows:

- Login into [Travis CI](https://travis-ci.org/) with your github account
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

doctest is implemented based on GHCi.
Unfortunately, GHCi on Mac is unstable.
Due to this,
running doctest itself sometime fails.
The following is my experience as a Mac user:

- "cabal build + test" is more stable than executing the "doctest" command
- 32 bit GHCi is more stable than 64 bit GHCi
- GHCi 7.6.x is more stable than GHCi 7.4.x

You should choose the combination of more stable ones if necessary.

###The doctest arguments

The doctest arguments (function and command) is
exactly the same as that of GHCi.
To run doctest well,
various arguments are sometime necessary.
One common case is "-XOverloadedStrings".

If you write C code by yourself
and use it from Haskell with FFI,
you may wonder what arguments should be specified.
In this case,
please refer to [unix-time](https://github.com/kazu-yamamoto/unix-time).

###doctest, haddock and QuickCheck

To describe QuickCheck properties in documents,
the `prop>` markup is already defined in latest haddock (version 2.13.0 or later).
Also, doctest already supports it.
An open question is when the Haskell Platform will include
a version of haddock which supports this notation.

haddock is packaged with GHC.
GHC automatically uses
[the "master" branch of haddock](https://github.com/ghc/haddock).
The branch supporting the `prop>` markup is "ghc-7.6".
So, "ghc-7.6" should be merged into "master".
Since some test cases fail at this moment,
the merge has not been carried out yet.
But I hope that "ghc-7.6" will be merged into "master" in the near future.

##Internal modules

You may want to use hidden constructors in test cases.
Common practice for this is to prepare
an internal module (whose typical name is "Internal.hs").
If a test file includes this internal module,
hidden constructors can be used.

To use internal modules from test suites specified in a Cabal file,
add the parent directory to HS-Source-Dirs:

      HS-Source-Dirs:       test,..

Now test code can import the internal modules.
Unfortunately,
test suites cannot depend on your library itself in this case.
So, you need to repeat that dependencies of your library
in Build-Depends of test suites.

##Remark

I thank Alan Zimmerman for his proofreading.

Please feel free to point out grammar or contents errors.
Pull requests are welcome.

Happy unit testing in Haskell!
