{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Scientific
import qualified Data.Text as T
import Network.Wreq

recentCompletedUrl :: String
recentCompletedUrl = "https://apps.fedoraproject.org/datagrepper/raw?delta=\
                     \172800&topic=org.fedoraproject.prod.ansible.playbook.\
                     \complete"

recentStartUrl :: String
recentStartUrl = "https://apps.fedoraproject.org/datagrepper/raw?delta=\
                 \172800&topic=org.fedoraproject.prod.ansible.playbook.\
                 \start"

data Playbook = Updating | Rebooting deriving (Eq, Ord, Show)
data MsgData = MsgData { _user :: T.Text
                       , _playbook :: Playbook
                       , _hosts :: [T.Text]
                       , _timestamp :: Double   -- TODO: UTCTime
                       } deriving (Eq, Ord, Show)
data PlaybookStatus = Start | Complete deriving (Eq, Ord, Show)
data PlaybookRun = PlaybookRun { _msgdata :: MsgData
                               , _status :: PlaybookStatus
                               } deriving (Eq, Ord, Show)

valueToPlaybookRun :: Value -> Maybe PlaybookRun
valueToPlaybookRun v = do
  topic <- join (startOrStop <$> v ^? key "topic" . _String)
  ts <- toRealFloat <$> v ^? key "timestamp" . _Number
  user <- v ^? key "msg" . key "userid" . _String
  playbook <- join (determinePlaybook <$> v ^? key "msg" . key "playbook" . _String)
  let target = case topic of
                Start -> v ^.. key "msg" . key "extra_vars" . key "target" . _String
                Complete -> v ^. key "msg" . key "results" . _Object . to HM.keys
  return $ PlaybookRun (MsgData user playbook target ts) topic

  where
    determinePlaybook p =
      if "vhost_reboot" `T.isInfixOf` p
      then Just Rebooting
      else if "vhost_update" `T.isInfixOf` p
           then Just Updating
           else Nothing

    startOrStop t =
      if "ansible.playbook.start" `T.isSuffixOf` t
      then Just Start
      else if "ansible.playbook.complete" `T.isSuffixOf` t
           then Just Complete
           else Nothing

getRecent :: String -> IO [PlaybookRun]
getRecent s = do
  msgs <- get s
  -- TODO: I can probably make this prettier with more lens familiarity.
  return . catMaybes . map valueToPlaybookRun $ msgs ^.. responseBody . key "raw_messages" . values

getRecentStarted :: IO [PlaybookRun]
getRecentStarted = getRecent recentStartUrl

getRecentCompleted :: IO [PlaybookRun]
getRecentCompleted = getRecent recentCompletedUrl

main :: IO ()
main = error "hi!"
