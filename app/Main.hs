{- MonBot  v1
Copyright (c) 2016 by Alexander Diemand

see LICENSE.
-}
{-
 - this program builds on the library network-protocol-xmpp
 - by J. Millikin whose Copyright is repeated below.
 -
-- Copyright (c) 2010 John Millikin
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

-- XMPP imports
import Network
import Network.Protocol.XMPP
import Data.XML.Types

-- other imports
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E
import qualified Data.Text as T
import System.Environment
--import Data.List
import Data.Maybe

import qualified MonBot as MB


runBot :: String -> IO ()
runBot hostname = do
      -- Verify that the user provided a valid JID, and that it contains a username
      -- (AKA a "node").
      jid <- case parseJID MB.user of
            Just x -> return x
            Nothing -> error $ "Invalid JID: " ++ show MB.user
      username <- case strNode `fmap` jidNode jid of
            Just x -> return x
            Nothing -> error $ "JID must include a username"
      
      -- 'Server' values record what host the connection will be opened to. Normally
      -- the hostname and JID will be the same; however, in some cases the hostname is
      -- something special (like "jabber.domain.com" or "localhost").
      -- 
      -- The port number is hardcoded to 5222 in this example, but in the wild there
      -- might be servers with a jabberd running on alternative ports.
      let server = Server
            { serverHostname = hostname
            , serverJID = JID Nothing (jidDomain jid) Nothing
            , serverPort = PortNumber 5222
            }
      
      -- 'runClient' and 'runComponent' open a connection to the remote server and
      -- establish an XMPP session.
      -- 
      -- It is possible to run an XMPP session over multiple IO chunks using the
      -- 'getSession' computation. The returned session value can be used to run
      -- 'runXMPP'.
      -- 
      -- Unusual conditions like socket errors or async exceptions might cause this
      -- computation to raise an exception, but in normal operation all XMPP errors
      -- are returned via a 'Left' value.
      -- 
      -- 'XMPP' is an instance of 'MonadError', so you can use the standard
      -- 'throwError' and 'catchError' computations to handle errors within an XMPP
      -- session.
      res <- runClient server jid username MB.password $ do
            -- When running a client session, most servers require the user to
            -- "bind" their JID before sending any stanzas.
            boundJID <- bindJID jid
            
            -- Some servers will close the XMPP connection after some period
            -- of inactivity. For this example, we'll simply send a "ping" every
            -- 60 seconds
            getSession >>= liftIO . forkIO . sendPings 60
            
            -- 'XMPP' is an instance of 'MonadIO', so any IO may be performed
            -- within.
            liftIO $ putStrLn $ "Server bound our session to: " ++ show boundJID
            isSecure <- sessionIsSecure
            liftIO $ putStrLn $ " is secure: " ++ show isSecure
            
            -- This is a simple loop which will echo received messages back to the
            -- sender; additionally, it prints *all* received stanzas to the console.
            forever $ do
                  stanza <- getStanza
                  liftIO $ putStr "\n" >> print stanza >> putStrLn "\n"
                  case stanza of
                        ReceivedMessage msg -> if messageType msg == MessageError
                              then return ()
                              else do 
                                            liftIO $ putStrLn $ " got sender = " ++ extract_sender msg
                                            liftIO $ display_msg $ messagePayloads msg
                                            if valid_sender msg then do m' <- liftIO $ talktalk msg
                                                                        putStanza m'
                                                                else putStanza $ byebye msg
                        ReceivedPresence msg -> if presenceType msg == PresenceSubscribe
                              then putStanza (subscribe msg)
                              else return ()
                        _ -> return ()
      
      -- If 'runClient' terminated due to an XMPP error, propagate it as an exception.
      -- In non-example code, you might want to show this error to the user.
      case res of
            Left err -> error $ show err
            Right _ -> return ()

-- test for valid sender address (incl. domain)
valid_sender :: Message -> Bool
valid_sender m = (extract_sender m) `elem` MB.list_of_valid_senders

extract_sender :: Message -> String
extract_sender m = (extract_user $ messageFrom m) ++ "@" ++ (extract_domain $ messageFrom m)
    where 
    extract_user :: Maybe JID -> String
    extract_user (Just n) = T.unpack $ fromMaybe T.empty $ strNode `fmap` jidNode n
    extract_user _ = "unkn"
    extract_domain :: Maybe JID -> String
    extract_domain (Just n) = T.unpack $ strDomain $ jidDomain n
    extract_domain _ = "nowhere"

talktalk :: Message -> IO Message
talktalk m = do
    response <- respond $ messagePayloads m
    return Message
      { messageType = MessageNormal
      , messageTo   = messageFrom m
      , messageFrom = Nothing
      , messageID   = Nothing
      , messageLang = Nothing
      , messagePayloads = response
      }

byebye :: Message -> Message
byebye msg = 
   Message
      { messageType = MessageError --MessageNormal
      , messageTo   = messageFrom msg
      , messageFrom = Nothing
      , messageID   = Nothing
      , messageLang = Nothing
      , messagePayloads = [ Element { elementName="{jabber:client}body", elementAttributes=[], elementNodes=[(NodeContent (ContentText (T.pack "bye bye!")))] } ]
      }

respond :: [Element] -> IO [Element]
respond [] = return []
respond es = mapM interpret es

interpret :: Element -> IO Element
interpret e =
    let n = T.unpack $ nameLocalName (elementName e) in
    do
    case n of "body" -> do e' <- mapM interpret_body (elementNodes e)
                           return $ e{elementNodes=e'}
              _ -> return e

interpret_body :: Data.XML.Types.Node -> IO Data.XML.Types.Node
interpret_body (NodeContent (ContentText t)) = do
    t' <- E.catch (MB.exec_cmd (T.unpack t)) ((\_ -> return "error occured") :: E.IOException -> IO String)
    return (NodeContent (ContentText (T.pack t')))
interpret_body _ = return (NodeContent (ContentText (T.pack "unknown")))


display_msg [] = return ()
display_msg (m:ms) = do
    putStrLn $ "message name " ++ (T.unpack $ nameLocalName $ elementName m)
    display_msg_nodes $ elementNodes m
    display_msg ms

display_msg_nodes [] = return ()
display_msg_nodes (n:ns) = do
    putStrLn $ "node content: " ++ ( case n of NodeContent (ContentText t) -> T.unpack t
                                               _ -> "???" )
    display_msg_nodes ns

subscribe :: Presence -> Presence
subscribe p = Presence
      { presenceType = PresenceSubscribed
      , presenceTo = presenceFrom p
      , presenceFrom = Nothing
      , presenceID = Nothing
      , presenceLang = Nothing
      , presencePayloads = []
      }

-- Send a "ping" occasionally, to prevent server timeouts from
-- closing the connection.
sendPings :: Integer -> Session -> IO ()
sendPings seconds s = forever send where
      send = do
            -- Ignore errors
            runXMPP s $ putStanza ping
            threadDelay $ fromInteger $ 1000000 * seconds
      ping = (emptyIQ IQGet)
            { iqPayload = Just (Element pingName [] [])
            }

pingName :: Name
pingName = Name "ping" (Just "urn:xmpp:ping") Nothing

main :: IO ()
main = do
       args <- getArgs
       case args of
            (server:_) -> runBot server
            _ -> do
                  name <- getProgName
                  error $ "Use: " ++ name ++ " <server>"
