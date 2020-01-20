{- |
Description: parameters for an SMTP session.
-}
module Network.Mail.SMTP.SMTPParameters
  (
    HostName
  , PortNumber
  , SMTPParameters(..)
  , defaultSMTPParameters
  )
  where

import Network.Socket (PortNumber)

type HostName = String

-- | Data necessary to kick-start an SMTP session, plus a flag to indicate
--   verbosity (actually a misnomer I though; should be smtpQuiet, since we
--   have only two options: verbose or not verbose).
data SMTPParameters = SMTPParameters {
    smtpHost :: HostName
  , smtpPort :: PortNumber
  , smtpVerbose :: Bool
  } 
  deriving Show

-- | Default SMTP parameters for some hostname. Uses port 25, non-verbose.
defaultSMTPParameters :: HostName -> SMTPParameters
defaultSMTPParameters hostname = SMTPParameters {
    smtpHost = hostname
  , smtpPort = 25
  , smtpVerbose = False
  }
