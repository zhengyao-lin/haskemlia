{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Network.Kademlia.RPC where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.Conc.Class

import Network.Kademlia.Types

data Method
    -- request
    = Ping
    | FindValue KeyID
    | FindNode KeyID
    | Store KeyID Value

    -- response
    | Pong
    | Value Value
    | Nodes [Node]
    | Stored Bool

    | Error String

isRequest Ping = True
isRequest (FindValue _) = True
isRequest (FindNode _) = True
isRequest (Store _ _) = True
isRequest _ = False

data Message =
    Message {
        sender :: Node,
        method :: Method
    }

class MonadConc m => Network m n where
    call :: n -> Node -> Message -> m (Maybe Message)
    receive :: n -> m (Message, Message -> m ())
