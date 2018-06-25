{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Network.Kademlia.RPC where

import Data.ByteString
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.Conc.Class

import Network.Kademlia.Types

data Method
    = Ping
    | Pong
    | FindValue KeyID
    | FindNode KeyID
    | Value ByteString
    | Nodes [Node]

data Message =
    Message {
        sender :: Node,
        method :: Method,
        msg_id :: ByteString
    }

class MonadConc m => Network m n where
    call :: n -> Message -> m Message
    response :: n -> Message -> m ()
    receive :: n -> m Message
