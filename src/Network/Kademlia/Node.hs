module Network.Kademlia.Node where

import Control.Monad.State
import Control.Monad.Conc.Class

import Network.Kademlia.RPC
import Network.Kademlia.Types
import Network.Kademlia.KBucket

data Env n =
    Env {
        env_node_set :: NodeSet,
        env_net      :: n,
        env_self     :: Node,
        env_param    :: NetworkParam
    }

type EnvT m n = StateT (Env n) m

netE :: Network m n => EnvT m n n
netE = env_net <$> get

nodeSetE :: Network m n => EnvT m n NodeSet
nodeSetE = env_node_set <$> get

selfE :: Network m n => EnvT m n Node
selfE = env_self <$> get

paramE :: Network m n => (NetworkParam -> a) -> EnvT m n a
paramE p = p . env_param <$> get

responseE :: Network m n => Message -> EnvT m n ()
responseE msg = do
    net <- netE
    lift (response net msg)

receiveE :: Network m n => EnvT m n Message
receiveE = do
    net <- netE
    lift (receive net)

reply :: Message -> Method -> Message
reply msg method = msg { method = method }

handle :: Network m n => Message -> Method -> EnvT m n ()
handle msg Ping = responseE (reply msg Pong)

handle msg (FindNode id) = do
    set <- nodeSetE
    k <- paramE net_k
    let nodes = findBestNodes set k id
    responseE (reply msg (Nodes nodes))

handle _ _ = error "unimplemented method"

route :: Network m n => EnvT m n ()
route = do
    msg <- receiveE
    fork (handle msg (method msg))
    route
