{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Description: definition of the SMTP monad and its terms.
-}

module Network.Mail.SMTP.SMTP 
  (

    SMTP

  , smtp

  , command
  , bytes
  , expect
  , expectCode

  , SMTPContext
  , getSmtpContext

  , getSMTPServerHostName
  , getSMTPClientHostName

  , startTLS

  , SMTPError(..)

  ) 
  where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.State

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Default

import Network.BSD (getHostName)
import Network.Connection

import Network.Mail.SMTP.Types
import Network.Mail.SMTP.ReplyLine
import Network.Mail.SMTP.SMTPRaw
import Network.Mail.SMTP.SMTPParameters


-- | An SMTP client EDSL: it can do effects, things can go wrong, and
--   it carries state.
newtype SMTP a = SMTP {
    runSMTP :: ExceptT SMTPError (StateT SMTPContext IO) a
  } 
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an expression in the SMTP monad.
--   Should be exception safe, but I am not confident in this.
smtp :: SMTPParameters -> SMTP a -> IO (Either SMTPError a)
smtp smtpParameters smtpValue = do
  smtpContext <- makeSMTPContext smtpParameters
  case smtpContext of
    Left err -> return $ Left err
    Right smtpContext -> do
      x <- evalStateT (runExceptT (runSMTP smtpValue)) smtpContext
      closeSMTPContext smtpContext
      return x

-- | Attempt to make an SMTPContext.
makeSMTPContext :: SMTPParameters -> IO (Either SMTPError SMTPContext)
makeSMTPContext smtpParameters = do
    clientHostname <- getHostName
    result <- liftIO $ try (smtpConnect serverHostname (fromIntegral port))
    return $ case result :: Either SomeException (SMTPRaw, Maybe Greeting) of
      Left _err -> Left ConnectionFailure
      Right (smtpRaw, _) -> Right $ 
                         SMTPContext smtpRaw serverHostname clientHostname debug
  where
    serverHostname = smtpHost smtpParameters
    port           = smtpPort smtpParameters
    debug          = if smtpVerbose smtpParameters
                     then putStrLn
                     else const (return ())

-- | Attempt to close an SMTPContext, freeing its resource.
closeSMTPContext :: SMTPContext -> IO ()
closeSMTPContext smtpContext = 
  -- hClose (smtpConn (smtpRaw smtpContext))
  -- ?? why not just:
  smtpClose $ smtpRaw smtpContext

-- | Send a command, without waiting for the reply.
command :: Command -> SMTP ()
command cmd = SMTP $ do
  ctxt <- lift get
  liftIO $ smtpDebug ctxt $ "Send command: " ++ show (toByteString cmd)
  result <- liftIO $ try $ smtpSendCommand (smtpRaw ctxt) cmd
  case result :: Either SomeException () of
    Left _err -> throwE UnknownError
    Right () -> return ()

-- | Send some bytes, with a crlf inserted at the end, without waiting for
--   the reply.
bytes :: B.ByteString -> SMTP ()
bytes bs = SMTP $ do
    ctxt    <- lift get
    liftIO $ smtpDebug ctxt ("Send bytes: " ++ show bs)
    result  <- liftIO $ try (smtpSendRaw (smtpRaw ctxt) (B.append bs crlf))
    case result :: Either SomeException () of
      Left _err -> throwE UnknownError
      Right () -> return ()
  where
    crlf = pack "\r\n"

-- | Pull a response from the server, passing it through a function which
--   checks that it's an expected response. If the response doesn't parse as
--   an SMTP response, we give an UnexpectedResponse.
expect :: ([ReplyLine] -> Maybe SMTPError) -> SMTP ()
expect ok = SMTP $ do
  ctxt  <- lift get
  let smtpraw = smtpRaw ctxt
  reply <- liftIO $ smtpGetReplyLines smtpraw
  liftIO $ smtpDebug ctxt $ "Receive response: " ++ show reply
  case reply of
    Nothing -> throwE UnexpectedResponse
    Just reply -> case ok reply of
      Just err -> throwE err
      Nothing -> return ()

-- | Like expect, but you give only the ReplyCode that is expected. Any other
--   reply code, or an unexpected reponse, is considered a failure.
expectCode :: ReplyCode -> SMTP ()
expectCode code = expect hasCode
  where
    hasCode [] = Just UnexpectedResponse
    hasCode (reply : _) =
      if replyCode reply == code
      then Nothing
      else Just UnexpectedResponse

-- | Grab the SMTPContext.
getSmtpContext :: SMTP SMTPContext
getSmtpContext = SMTP $ lift get

-- | Try to get TLS going on an SMTP connection.
startTLS :: SMTP ()
startTLS = do
  command STARTTLS
  expectCode 220
  tlsUpgrade
  -- TODO: add a utility func to get hostname easily
  clientHostName <- SMTP $ smtpClientHostName <$> lift get
  command $ EHLO $ pack clientHostName
  expectCode 250

-- | Upgrade to TLS.
tlsUpgrade :: SMTP ()
tlsUpgrade = 
  SMTP $ do
    conn <- smtpConn . smtpRaw <$> lift get
    ctx  <- smtpConnContext . smtpRaw <$> lift get
    -- negotiate the TLS context
    liftIO $ connectionSetSecure ctx conn def


-- | Description of an error in the SMTP monad evaluation.
data SMTPError
  = UnexpectedResponse
  | ConnectionFailure
  | EncryptionError
  | UnknownError
  deriving (Show, Eq)

-- | Description of the state which an SMTP term needs in order to be
--   evaluated.
data SMTPContext = SMTPContext {
    smtpRaw :: SMTPRaw
    -- ^ Necessary for push/pull to/from some reasource (probably a Connection).
  , smtpServerHostName :: HostName
  , smtpClientHostName :: HostName
  , smtpDebug :: String -> IO ()
    -- ^ A pipe into which we can throw debug messages. const (return ()) for
    -- squelching, or maybe putStrLn for verbosity.
  }

-- | Access the Connection datatype buried beneath the SMTP abstraction.
--   Use with care!
getSMTPConn :: SMTPContext -> Connection
getSMTPConn = smtpConn . smtpRaw

getSMTPServerHostName :: SMTPContext -> HostName
getSMTPServerHostName = smtpServerHostName

getSMTPClientHostName :: SMTPContext -> HostName
getSMTPClientHostName = smtpClientHostName
