
module ConfigHost1
  ( list_of_valid_senders
  , user, password
  , Cmd
  , MonitCmd, MonitSub
  , Service
  )
where

import qualified Data.Text as T

-- 
-- here provide the users which may use this service
--
list_of_valid_senders :: [String]
list_of_valid_senders = [ "masteruser@im.there.com", "adminadmin@outer.space" ]


--
-- authenticate with server using these credentials
--
user :: T.Text
user = "me@im.there.com"

password :: T.Text
password = "asdfasdf93asldf"


-- these are the commands that MonBot understands and passes to a shell
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

-- this is the monit command
data MonitCmd = Monit MonitSub
          deriving (Read)

-- we rewrite this as a shell script
instance Show MonitCmd where
    show (Monit ms) = "./monit.sh " ++ show ms

data MonitSub = Summary
              | Start Service
--              | Stop Service
              | Restart Service
              | Status Service
                deriving (Show,Read)

-- list here the services that are available on the host
data Service = Hostname
             | Java
             | Nginx
               deriving (Show,Read)

