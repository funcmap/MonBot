{- MonBot  v1
Copyright (c) 2016 by Alexander Diemand

see LICENCE.
-}

{-# LANGUAGE OverloadedStrings #-}

module MonBot 
  ( list_of_valid_senders
  , exec_cmd
  , Cmd, Sh, MonitSub, Service
  )
where

import System.Process (readProcess, readCreateProcess, shell)
import Data.Char (toLower)
import Text.Read (readMaybe)


-- 
-- here provide the users which may use this service
--
list_of_valid_senders :: [String]
list_of_valid_senders = [ "masteruser@im.there.com", "adminadmin@outer.space" ]


exec_cmd :: String -> IO String
exec_cmd c@(a:b:r) = eval_cmd c
exec_cmd c = return "minimalist"

-- these are the commands that MonBot understands
-- they start with a leading capital letter on every word
data Cmd = Date
         | Df
         | Free
--         | Who
         | Uptime
--         | Pwd
--         | Id
--         | Whoami
--         | Printenv
           deriving (Show,Read)

data Sh = Monit MonitSub
          deriving (Read)

-- we rewrite this as a shell script
instance Show Sh where
    show (Monit ms) = "./monit.sh " ++ show ms

data MonitSub = Summary
              | Start Service
--              | Stop Service
              | Restart Service
              | Status Service
                deriving (Show,Read)

data Service = Hostname
             | Java
             | Nginx
               deriving (Show,Read)

-- evaluate string as a typed command
-- execute if it is a representation of a "Cmd" type
-- (before, change to lower case)
eval_cmd :: String -> IO String
eval_cmd c = case (readMaybe c)::(Maybe Cmd) of
             Just cmd -> readProcess (map toLower $ show $ cmd) [] []
             _        -> case (readMaybe c)::(Maybe Sh) of
                         -- otherwise try whether it can be parsed as a shell command
                         Just sh -> readCreateProcess (shell (map toLower $ show $ sh)) ""
                         _       -> return "unknown"   

