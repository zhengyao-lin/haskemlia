module Network.Kademlia.KBucket where

import Data.List

import Network.Kademlia.Util
import Network.Kademlia.Types

type Bucket = [Node]
data NodeSet =
    NodeSet {
        set_self :: Node,
        set_k    :: Int,
        set_buck :: [Bucket]
    }

initSet :: NetworkParam -> Node -> NodeSet
initSet param self =
    NodeSet {
        set_self = self,
        set_k = net_k param,
        set_buck = replicate (netLength param) []
    }

-- the position of the left most 1
toBucketID :: NodeSet -> KeyID -> Int
toBucketID set id =
    leftmost (distance self id)
    where self = node_id (set_self set)

updateBucket :: Int -> (Bucket -> Bucket) -> NodeSet -> NodeSet
updateBucket idx f set =
    set {
        set_buck = replaceApp idx f (set_buck set)
    }

addNode :: Node -> NodeSet -> NodeSet
addNode node set =
    if node == set_self set then set
    else
        -- remove the node first(if it exists) then add it to the top
        -- if the bucket is full, remove the last one
        updateBucket idx (\buck ->
            let new = node : delete node buck in
            if length new > set_k set then init new
            else new) set

    where idx = toBucketID set (node_id node)

findBestNodes :: NodeSet -> Int -> KeyID -> [Node]
findBestNodes set n id =
    map fst $
    take n $
    concatMap list route
    where
        idx = toBucketID set id
        bucks = set_buck set
        route = bucks !! idx : drop (idx + 1) bucks ++ take idx bucks
        
        list buck =
            zip buck $
            sort $
            map (distance id . node_id) buck
