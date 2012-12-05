module Base64Spec where

import Codec.Base64
import Data.Bits
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

newtype Base64 = Base64 String deriving (Eq, Show)

base64 :: String
base64 = ['A'..'Z']++['a'..'z']++['0'..'9']++"+/"

instance Arbitrary Base64 where
    arbitrary = do
        xs <- listOf $ elements base64
        case length xs `mod` 4 of
            0 -> return (Base64 xs)
            1 -> do
                c <- elements "AQgw"
                return (Base64 $ xs ++ c : "==")
            2 -> do
                i <- choose (0,64)
                let c = base64 !! (i .&. 0x3c)
                return (Base64 $ xs ++ c : "=")
            _ -> do
                c <- elements base64
                return (Base64 $ xs ++ [c])

spec :: Spec
spec = do
    describe "encode" $ do
        it "encodes multiples of 3" $
            encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"
        it "encodes multiples of 3 + 1" $
            encode "padding  2" `shouldBe` "cGFkZGluZyAgMg=="
        it "encodes multiples of 3 + 2" $
            encode "padding1" `shouldBe` "cGFkZGluZzE="

        prop "reverses decoded string" $ \(Base64 xs) ->
            encode (decode xs) == xs

    describe "decode" $ do
        it "decodes no padding" $
            decode "bm8tcGFkZGluZyEh" `shouldBe` "no-padding!!"
        it "dncodes one padding" $
            decode "cGFkZGluZyAgMg==" `shouldBe` "padding  2"
        it "encodes two paddings" $
            decode "cGFkZGluZzE=" `shouldBe` "padding1"

        prop "reverses encoded string" $ \xs ->
            decode (encode xs) == xs
