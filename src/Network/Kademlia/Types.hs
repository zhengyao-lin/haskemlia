module Network.Kademlia.Types where

import Data.Bits
import qualified Data.ByteString as BSR

type Hash = BSR.ByteString
type KeyID = Hash

distance :: KeyID -> KeyID -> KeyID
distance i1 i2 = BSR.pack (BSR.zipWith xor i1 i2)

leftmost :: KeyID -> Int
leftmost =
    (+(-1)) .
    length .
    dropWhile not .
    concatMap bits .
    BSR.unpack
    where bits w = map (testBit w) [ 7, 6 .. 0 ]

-- instance Ord KeyID where
--     compare i1 i2 =
--         case res of
--             r:_ -> r
--             [] -> EQ

--         where res = dropWhile (== EQ) (BSR.zipWith compare i1 i2)

data HashFunction =
    HashFunction (BSR.ByteString -> Hash) Int -- in bits

hashLength (HashFunction _ l) = l
hash (HashFunction f _) = f

data Node =
    Node {
        node_id :: KeyID
    }

instance Eq Node where
    n1 == n2 = node_id n1 == node_id n2

data NetworkParam =
    NetworkParam {
        net_k     :: Int,
        net_alpha :: Int,
        net_hash  :: HashFunction
    }

netLength = hashLength . net_hash
