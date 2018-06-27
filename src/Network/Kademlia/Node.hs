module Network.Kademlia.Node where

import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as MAP
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.Conc.Class
import qualified Control.Monad.State as ST

import Network.Kademlia.RPC
import Network.Kademlia.Types
import Network.Kademlia.KBucket

data Env n =
    Env {
        env_node_set :: NodeSet,
        env_net      :: n,
        env_self     :: Node,
        env_store    :: MAP.HashMap KeyID Value,
        env_param    :: NetworkParam
    }

type Instance m n = CRef m (Env n)

type InstanceT m n = ST.StateT (Instance m n) m

get :: Network m n => InstanceT m n (Env n)
get = ST.get >>= (lift . readCRef)

put :: Network m n => Env n -> InstanceT m n ()
put e = ST.get >>= (lift . flip writeCRef e)

lift :: Network m n => m a -> InstanceT m n a
lift = ST.lift

netE :: Network m n => InstanceT m n n
netE = env_net <$> get

nodeSetE :: Network m n => InstanceT m n NodeSet
nodeSetE = env_node_set <$> get

nodeSetAppE :: Network m n => (NodeSet -> NodeSet) -> InstanceT m n ()
nodeSetAppE f = do
    env <- get
    put env {
        env_node_set = f (env_node_set env)
    }

selfE :: Network m n => InstanceT m n Node
selfE = env_self <$> get

paramE :: Network m n => (NetworkParam -> a) -> InstanceT m n a
paramE p = p . env_param <$> get

lookupE :: Network m n => KeyID -> InstanceT m n (Maybe Value)
lookupE k = MAP.lookup k . env_store <$> get

insertE :: Network m n => KeyID -> Value -> InstanceT m n ()
insertE k v = do
    env <- get
    put env {
        env_store = MAP.insert k v (env_store env)
    }

sizeE :: Network m n => InstanceT m n Int
sizeE = MAP.size . env_store <$> get

nBestNodesE :: Network m n => Int -> KeyID -> InstanceT m n [Node]
nBestNodesE n id = do
    set <- nodeSetE
    let nodes = findBestNodes set n id
    return nodes

kBestNodesE :: Network m n => KeyID -> InstanceT m n [Node]
kBestNodesE id = paramE net_k >>= flip nBestNodesE id

reply :: Message -> Node -> Method -> Message
reply msg sender method = msg { sender = sender, method = method }

route :: Network m n => InstanceT m n ()
route = do
    net <- netE
    self <- selfE
    (msg, resp) <- lift (receive net)
    fork (handle (lift . resp . reply msg self) (method msg))
    route

handle :: Network m n => (Method -> InstanceT m n ()) -> Method -> InstanceT m n ()
handle resp Ping = resp Pong

handle resp (FindNode id) =
    Nodes <$> kBestNodesE id >>= resp

handle resp (FindValue id) = do
    res <- lookupE id

    case res of
        Just v -> resp (Value v)
        Nothing ->
            Nodes <$> kBestNodesE id >>= resp

handle resp (Store k v) = do
    max <- paramE net_max_store
    size <- sizeE

    if size >= max then resp (Stored False)
    else do
        insertE k v
        resp (Stored True)

handle resp _ = resp (Error "unimplemented method")

newInstance :: Network m n => NetworkParam -> n -> String -> m (Instance m n)
newInstance param net name = do
    let self = Node (hash (net_hash param) (BS.pack name))

    newCRef Env {
        env_node_set = initSet param self,
        env_net = net,
        env_self = self,
        env_store = MAP.empty,
        env_param = param
    }

-- interface: get, set

networkLookup :: Network m n => [Node] -> [Node] -> KeyID -> Bool -> InstanceT m n (Either [Node] Value)
networkLookup init traversed id for_value = do
    net <- netE
    self <- selfE
    k <- paramE net_k

    if length traversed >= k then return (Left traversed)
    else do
        let find = if for_value then FindValue id else FindNode id

        mvars <- forM init $ \node -> do
            mvar <- newEmptyMVar

            fork $ do
                res <- lift $ call net node Message {
                    sender = self,
                    method = find
                }

                case res of
                    Just msg -> putMVar mvar (Just (method msg))
                    Nothing -> putMVar mvar Nothing

            return mvar

        res <-
            (<$> (catMaybes <$> mapM takeMVar mvars)) $
            map $ \res ->
                case res of
                    Nodes nodes ->
                        -- filter out known nodes
                        Left (filter (\n -> n `notElem` traversed && n /= self) nodes)

                    Value v ->
                        if for_value then Right v
                        else Left []

                    _ -> Left []

        -- if nodes, add to init if not in traversed & is not self
        -- if value && for_value == True, return Right Value
        -- others, skip

        let (next, mv) = foldl (\(l0, v) r -> case r of
                Left l1 -> (l0 ++ l1, v)
                Right v -> (l0, Just v)) ([], Nothing) res

            min_dist nodes = minimum (map (distance id . node_id) nodes)

        -- try adding new nodes
        mapM_ (nodeSetAppE . addNode) next

        case mv of
            Just v -> return (Right v)
            Nothing -> do
                if min_dist next >= min_dist traversed then
                    let all = map (\n -> (n, distance id (node_id n))) (traversed ++ next)
                        final = map fst (take k (sortOn snd all))
                    in return (Left final)
                else
                    networkLookup next (traversed ++ next) id for_value

getE :: Network m n => KeyID -> InstanceT m n (Maybe Value)
getE id = do
    res <- lookupE id

    case res of
        Just v -> return (Just v)
        Nothing -> do
            alpha <- paramE net_alpha
            init <- nBestNodesE alpha id
            either (const Nothing) Just <$> networkLookup init [] id True

setE :: Network m n => KeyID -> Value -> InstanceT m n ()
setE id v = do
    net <- netE
    self <- selfE

    alpha <- paramE net_alpha
    init <- nBestNodesE alpha id
    Left nodes <- networkLookup init [] id False

    lift $ forM_ nodes $ \node ->
        fork $ void $ call net node Message {
            sender = self,
            method = Store id v
        }
