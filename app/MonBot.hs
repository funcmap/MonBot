{- MonBot  v1
Copyright (c) 2016 by Alexander Diemand

see LICENCE.
-}

{-# LANGUAGE OverloadedStrings #-}

module MonBot 
  ( Config.list_of_valid_senders
  , exec_cmd
  , Config.user, Config.password
  , Config.Cmd, Config.MonitCmd, Config.MonitSub, Config.Service
  )
where

import System.Process (readProcess, readCreateProcess, shell)
import Data.Char (toLower)
import Text.Read (readMaybe)

import qualified ConfigHost1 as Config
--import qualified ConfigHost2 as Config

-- 
-- here provide the users which may use this service
--
--list_of_valid_senders :: [String]
--list_of_valid_senders = Config.list_of_valid_senders


exec_cmd :: String -> IO String
exec_cmd c@(a:b:r) = eval_cmd c
exec_cmd c = return "minimalist"

-- evaluate string as a typed command
-- execute if it is a representation of a "Cmd" type
-- or, if it is of a "MonitCmd" type
eval_cmd :: String -> IO String
eval_cmd c = case (readMaybe c)::(Maybe Config.Cmd) of
             Just cmd -> readProcess (map toLower $ show $ cmd) [] []
             _        -> case (readMaybe c)::(Maybe Config.MonitCmd) of
                         -- otherwise try whether it can be parsed as a shell command
                         Just sh -> readCreateProcess (shell (map toLower $ show $ sh)) ""
                         _       -> return "unknown"   

