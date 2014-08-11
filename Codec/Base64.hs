module Codec.Base64 (encode, decode) where

import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Word

-- |
-- Base64 encoding.
--
-- >>> encode "foo bar"
-- "Zm9vIGJhcg=="
--
-- prop> decode (encode xs) == xs
encode :: String -> String
encode = map (base64array !) . encode' . map (fromIntegral . ord)

encode' :: [Word8] -> [Word8]
encode' []         = []
encode' [a]        = e1 a : e2 a 0 : pad    : pad  : []
encode' [a,b]      = e1 a : e2 a b : e3 b 0 : pad  : []
encode' (a:b:c:xs) = e1 a : e2 a b : e3 b c : e4 c : encode' xs

e1,e4 :: Word8 -> Word8
e2,e3 :: Word8 -> Word8 -> Word8
e1 a   = shiftR a 2
e2 a b = shiftL (a .&. 0x03) 4 .|. shiftR b 4
e3 b c = shiftL (b .&. 0x0f) 2 .|. shiftR c 6
e4   c = c .&. 0x3f

base64 :: String
base64 = ['A'..'Z']++['a'..'z']++['0'..'9']++"+/="

base64array :: UArray Word8 Char
base64array = array (0,pad) $ zip [0..pad] base64

pad :: Word8
pad = 64

-- |
-- Base64 decoding.
--
-- >>> decode "Zm9vIGJhcg=="
-- "foo bar"
decode :: String -> String
decode = map (chr . fromIntegral)
       . decode'
       . map ((base128array !) . fromIntegral . ord)

decode' :: [Word8] -> [Word8]
decode' []     = []
decode' (a:b:c:d:xs)
  | c == pad   = d1 a b : []
  | d == pad   = d1 a b : d2 b c : []
  | otherwise  = d1 a b : d2 b c : d3 c d : decode' xs
decode' _      = error "decode'"

d1,d2,d3 :: Word8 -> Word8 -> Word8
d1 a b = shiftL a 2            .|. shiftR b 4
d2 b c = shiftL (b .&. 0x0f) 4 .|. shiftR c 2
d3 c d = shiftL (c .&. 0x03) 6 .|. d

base128array :: UArray Word8 Word8
base128array = array (0,255) $ zip [0..255] base128

ng :: Word8
ng = 0

base128 :: [Word8]
base128 =
    [ ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng
    , ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng
    , ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, ng, 62, ng, ng, ng, 63
    , 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, ng, ng, ng, 64, ng, ng
    , ng,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14
    , 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, ng, ng, ng, ng, ng
    , ng, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40
    , 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, ng, ng, ng, ng, ng
    ]
