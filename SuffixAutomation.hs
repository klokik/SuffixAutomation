module SuffixAutomation
    (
    SState(),
    SuffixAutomation(),
    empty,
    fromList,
    addItem,
    contains
    )where

import qualified Data.Map as M
import Prelude
import Data.Maybe


data SState a = SState {    len :: Int
                        ,   link :: Maybe Int
                        ,   next :: M.Map a Int
                        }   deriving (Show)

data SuffixAutomation a = SuffixAutomation {    states :: M.Map Int (SState a)
                                            ,   root :: Int
                                            ,   last_i :: Int
                                            }   deriving (Show)


empty :: SuffixAutomation a
empty = SuffixAutomation {states = M.fromList [(0,(SState {len = 0, link = Nothing, next = M.empty}))], root = 0, last_i = 0}


linkedList :: Int -> SuffixAutomation a -> [(Int,SState a)]
linkedList cid aut = case link current of   Nothing -> [(cid,current)]
                                            Just lp -> (cid,current):(linkedList lp aut)
    where current = (states aut)M.!cid

filter' :: (SState a -> Bool) -> [(Int,SState a)] -> [(Int,SState a)]
filter' f xs = zip (map (fst) xs) $ filter f $ map (snd) xs

takeWhile' :: (SState a -> Bool) -> [(Int,SState a)] -> [(Int,SState a)]
takeWhile' f xs = zip (map (fst) xs) $ takeWhile f $ map (snd) xs

map' :: (SState a -> b) -> [(c,SState a)] -> [(c,b)]
map' f xs = zip (map (fst) xs) $ map f $ map (snd) xs

applyList :: [(Int,SState a)] -> SuffixAutomation a -> SuffixAutomation a
applyList [] aut = aut
applyList ((k,v):xs) aut = applyList xs $ SuffixAutomation { states = M.insert k v (states aut), root = (root aut), last_i = (last_i aut)}

addItem :: (Ord a,Show a) => SuffixAutomation a -> a -> SuffixAutomation a
addItem aut x = SuffixAutomation { states = result, root = (root aut), last_i = cid }
                 where  patch = (map' (\a -> SState {len = (len a)
                                                    ,link = (link a)
                                                    ,next = M.insert x cid (next a)}) $ path) ++ [(cid,cur)] ++ clone_list
                        path = takeWhile' (\a -> x `M.notMember` (next a)) (linkedList (last_i aut) aut)
                        path_p = init $ takeWhile' (\a -> x `M.member` (next a)) (linkedList ip aut)
                        result = (states (applyList patch aut))
                        cid = M.size (states aut)
                        cur = SState { len = (len ((states aut)M.!(last_i aut)) + 1), link = Just new_link, next = M.empty }
                        ip = maybe (-1) (id) (link . snd . last $ path)
                        p = (states aut) M.! ip
                        q = (states aut) M.! ((next p)M.!x)
                        clone = (M.size (states aut) + 1,SState { len = len p + 1, link = link q, next = next q})
                        clone_list = if ip /= -1 && len p + 1 /= len q then [clone] ++ (map' (\a -> SState {len=len a,link=Just $ fst clone,next=next a}) path_p) else []
                        new_link = if ip == -1  then    0
                                                else    if len p + 1 == len q
                                                            then (next p)M.!x
                                                            else fst clone

fromList :: (Ord a,Show a) => [a] -> SuffixAutomation a
fromList xs = foldl (addItem) empty xs

contains :: (Ord a,Show a) => SuffixAutomation a -> [a] -> Bool
contains aut xs = lookup' xs 0
    where   lookup' [] _ = True
            lookup' (y:ys) id = if y `elem` (M.keys next_map)   then lookup' ys (next_map M.! y)
                                                                else False
                where next_map = next $ (states aut) M.! id