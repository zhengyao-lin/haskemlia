{-# LANGUAGE MultiParamTypeClasses #-}

-- UDP instance for Network class

module Network.Kademlia.UDP where

import Data.ByteString
import qualified Data.ByteString as BSR
import qualified Data.HashTable.IO as TAB

import Control.Monad

import Debug.Trace

import System.Random
import System.Timeout

import Network.Socket hiding (sendTo, recvFrom, recv)
import Network.Socket.ByteString

import Network.Kademlia.RPC
import Network.Kademlia.Util
import Network.Kademlia.Types

{-

:l
:m + Network.Socket

let localhost port = head <$> getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just "localhost") (Just (show port))
let new addr = socket (addrFamily addr) Datagram (addrProtocol addr)

info0 <- localhost 3160
info1 <- localhost 3161
info2 <- localhost 3162

sock0 <- new info0
sock1 <- new info1
sock2 <- new info2

-}

type Peer = SockAddr

peerToSockAddr = id
sockAddrToPeer = id

data UDPNetwork =
    UDPNetwork {
        self_addr :: SockAddr,
        net_param :: NetworkParam,
        node_map  :: TAB.CuckooHashTable Node Peer
    }

lookupNode :: UDPNetwork -> Node -> IO (Maybe Peer)
lookupNode udp node = TAB.lookup (node_map udp) node

insertNode :: UDPNetwork -> Node -> Peer -> IO ()
insertNode udp node peer = TAB.insert (node_map udp) node peer

param :: (NetworkParam -> a) -> UDPNetwork -> a
param = (. net_param)

buildSocket :: UDPNetwork -> IO Socket
buildSocket udp = do
    let hints = defaultHints { addrSocketType = Datagram }
        flags = [ NI_NUMERICHOST, NI_NUMERICSERV ]

    (Just host, Just port) <- getNameInfo flags True True (self_addr udp)
    info:_ <- getAddrInfo (Just hints) (Just host) (Just port)

    sock <- socket (addrFamily info) (addrSocketType info) (addrProtocol info)

    bind sock (self_addr udp)

    return sock

callPeer :: UDPNetwork -> Peer -> Message -> IO (Maybe Message)
callPeer udp peer msg = do
    let remote = peerToSockAddr peer

    id <- BSR.pack <$> replicateM 4 randomIO -- generate a random string as id

    sock <- buildSocket udp
    connect sock remote -- to listen only from the specific peer

    raw <- encodeMessage udp (RawMessage msg id)
    sendTo sock raw remote

    let timeout_ms = param net_timeout udp * 10 ^ 6

        -- keep retrying until a matching result is received
        tryRecv = do
            mres <- timeout timeout_ms (recv sock (param net_max_recv udp))
            
            case mres of
                Nothing -> return Nothing
                Just res -> do
                    RawMessage msg rid <- decodeMessage udp res

                    if rid == id then return (Just msg)
                    else tryRecv

    resp <- tryRecv
    close sock

    return resp

data RawMessage = RawMessage Message ByteString

encodeMessage :: UDPNetwork -> RawMessage -> IO ByteString
encodeMessage = undefined

decodeMessage :: UDPNetwork -> ByteString -> IO RawMessage -- could be Error
decodeMessage = undefined

instance Network IO UDPNetwork where
    -- call :: UDPNetwork -> Node -> Message -> IO (Maybe Message)
    call udp node msg = do
        res <- lookupNode udp node

        case res of
            Nothing -> error "peer not found"
            Just peer -> callPeer udp peer msg

    -- receive :: UDPNetwork -> IO (Message, Message -> IO ())
    receive udp = do
        sock <- buildSocket udp
        (res, addr) <- recvFrom sock (param net_max_recv udp)

        RawMessage msg id <- decodeMessage udp res

        if isRequest (method msg) then do

            -- add node-peer pair
            insertNode udp (sender msg) (sockAddrToPeer addr)

            let resp msg = void $ do
                    enc <- encodeMessage udp (RawMessage msg id)
                    sendTo sock enc addr

            return (msg, resp)
        else case method msg of
            Error err -> error ("error in decoding: " ++ err)
            _ -> -- skip non-request messages and retry
                receive udp
