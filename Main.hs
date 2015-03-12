{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Scientific
import qualified Data.Set as S
import qualified Data.Text as T
import Network.Wreq

recentCompletedUrl :: String
--recentCompletedUrl = "https://apps.fedoraproject.org/datagrepper/raw?delta=\
--                     \172800&topic=org.fedoraproject.prod.ansible.playbook.\
--                     \complete"

recentStartUrl :: String
--recentStartUrl = "https://apps.fedoraproject.org/datagrepper/raw?delta=\
--                 \172800&topic=org.fedoraproject.prod.ansible.playbook.\
--                 \start"

recentCompletedUrl = "http://localhost:8000/out.json"
recentStartUrl = "http://localhost:8000/out1.json"

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
makeLenses ''MsgData
makeLenses ''PlaybookRun

valueToPlaybookRun :: Value -> Maybe PlaybookRun
valueToPlaybookRun v = do
  topic <- join (startOrStop <$> v ^? key "topic" . _String)
  ts <- toRealFloat <$> v ^? key "timestamp" . _Number
  user' <- v ^? key "msg" . key "userid" . _String
  playbook' <- join (determinePlaybook <$> v ^? key "msg" . key "playbook" . _String)
  let target = case topic of
                Start -> v ^.. key "msg" . key "extra_vars" . key "target" . _String
                Complete -> v ^. key "msg" . key "results" . _Object . to HM.keys
  return $ PlaybookRun (MsgData user' playbook' target ts) topic

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

main :: IO ()
main = do
  recentStarted <- getRecent recentStartUrl
  recentCompleted <- getRecent recentCompletedUrl
  let (updC, rebC) = partition (\x -> x ^. msgdata . playbook == Updating) recentCompleted
      (updS, rebS) = partition (\x -> x ^. msgdata . playbook == Updating) recentStarted

      -- We start with all starting playbook runs and all completed playbook runs.
      -- For each starting playbook run, try to find a matching completed playbook run.
      -- Do this by:
      --   - Filtering completed playbook runs to find a run of the same type (update or reboot)
      --   - Taking that list and filtering it to find targets that match the starting playbook
      --   - Taking *that* list and filtering it to find completions that happened *after* the start time
      --   - Map over it to get a set of hostnames
      --   - If any of them match the starting playbook target, then return False and move on
      --   - If none of them match, the starting playbook is still likely being run.
      matchingPlaybookType p1 p2 = p1 ^. msgdata . playbook == p2 ^. msgdata . playbook
      matchingPlaybookTargets p1 p2 = p1 ^. msgdata . hosts == p2 ^. msgdata . hosts
      correctPlaybookTimestamps p1 p2 = p1 ^. msgdata . timestamp > p2 ^. msgdata . timestamp
      playbookPredicate p1 p2 =
        matchingPlaybookType p1 p2 &&
        matchingPlaybookTargets p1 p2 &&
        correctPlaybookTimestamps p1 p2
  error "hi!"
