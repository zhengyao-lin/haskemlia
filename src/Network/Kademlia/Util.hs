module Network.Kademlia.Util where

class Default a where
    def :: a

replace :: Int -> a -> [a] -> [a]
replace i e l =
    take i l ++ e : drop (i + 1) l

replaceApp :: Int -> (a -> a) -> [a] -> [a]
replaceApp i f l =
    take i l ++ f (l !! i) : drop (i + 1) l
