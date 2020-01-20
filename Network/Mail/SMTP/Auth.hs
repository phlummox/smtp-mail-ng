{-|
Description: terms for doing SMTP authorization.
-}
module Network.Mail.SMTP.Auth
  (
    authLogin
  )
  where

import           Crypto.Hash (MD5)
import           Crypto.MAC.HMAC (hmac, HMAC)

import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64  (encode)
import qualified Data.ByteString.Char8 as B8
import           Data.List (intercalate )

import Network.Mail.SMTP.SMTP
import Network.Mail.SMTP.Types

-- | Do LOGIN authentication.
authLogin :: UserName -> Password -> SMTP ()
authLogin username password = do
  -- There's no Command constructor for AUTH. TODO implement one? It has a
  -- volatile form I think; varies for each AUTH type I believe.
  reply <- bytes (B8.pack "AUTH LOGIN")
  -- TBD do we need to check that it gives the right text?
  -- I thought the RFCs say that only the codes matter...
  expectCode 334
  bytes $ b64Encode username
  expectCode 334
  bytes $ b64Encode password
  -- TODO need a mechanism to specify the error in case the code is bad.
  -- Or, maybe a mechanism to expect multiple codes and handle things
  -- differently based upon the code.
  expectCode 235 

toAscii :: String -> ByteString
toAscii = B.pack . map (toEnum.fromEnum)

b64Encode :: String -> ByteString
b64Encode = B64.encode . toAscii

encodePlain :: UserName -> Password -> ByteString
encodePlain user pass = b64Encode $ intercalate "\0" [user, user, pass]

encodeLogin :: UserName -> Password -> (ByteString, ByteString)
encodeLogin user pass = (b64Encode user, b64Encode pass)

cramMD5 :: String -> UserName -> Password -> ByteString
cramMD5 challenge user pass =
    B64.encode $ B8.unwords [user', convertToBase Base16 hmac']
  where
    challenge' = toAscii challenge
    user'      = toAscii user
    pass'      = toAscii pass

    hmac' :: HMAC MD5
    hmac'      = hmac challenge' pass'

{- Code from before the fork which is now dead, but I'll leave it around as
 - a reference for when we implement CRAM_MD5
auth :: AuthType -> String -> UserName -> Password -> ByteString
auth PLAIN    _ u p = encodePlain u p
auth LOGIN    _ u p = let (u', p') = encodeLogin u p in B8.unwords [u', p']
auth CRAM_MD5 c u p = cramMD5 c u p
-}
