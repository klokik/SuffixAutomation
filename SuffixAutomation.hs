module SuffixAutomation where

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


emptyAut :: SuffixAutomation a
emptyAut = SuffixAutomation {states = M.fromList [(0,(SState {len = 0, link = Nothing, next = M.empty}))], root = 0, last_i = 0}


linkedList :: Int -> SuffixAutomation a -> [(Int,SState a)]
linkedList id aut = case link current of    Nothing -> [(id,current)]
                                            Just lp -> (id,current):(linkedList lp aut)
    where current = (states aut)M.!id

filter' :: (SState a -> Bool) -> [(Int,SState a)] -> [(Int,SState a)]
filter' f xs = zip (map (fst) xs) $ filter f $ map (snd) xs

takeWhile' :: (SState a -> Bool) -> [(Int,SState a)] -> [(Int,SState a)]
takeWhile' f xs = zip (map (fst) xs) $ takeWhile f $ map (snd) xs

map' :: (SState a -> b) -> [(c,SState a)] -> [(c,b)]
map' f xs = zip (map (fst) xs) $ map f $ map (snd) xs

applyList :: [(Int,SState a)] -> SuffixAutomation a -> SuffixAutomation a
applyList [] aut = aut
applyList ((k,v):xs) aut = applyList xs $ SuffixAutomation { states = M.insert k v (states aut), root = (root aut), last_i = (last_i aut)}

addItem :: (Ord a,Show a) => a -> SuffixAutomation a -> SuffixAutomation a
addItem x aut = SuffixAutomation { states = (states aut), root = (root aut), last_i = id }
                 where  st1 = map' (\a -> SState {   len = (len a)
                                                    ,link = (link a)
                                                    ,next = M.insert x id (next a)}) $ path
                        path = (takeWhile' (\a -> x `M.notMember` (next a)) (linkedList (last_i aut) aut))
                        id = M.size (states aut)
                        cur = SState { len = (len ((states aut)M.!(last_i aut)) + 1), link = Just new_link, next = M.empty }
                        new_link = case (link . snd . last $ path) of   Nothing ->  0
                                                                        Just ip ->  if len p + 1 == len q
                                                                                    then (next p)M.!x
                                                                                    else fst clone
                                                                                        where   clone = (M.size (states aut) + 1,SState { len = len p + 1, link = link q, next = next q})
                                                                                                p = (states aut)M.!ip
                                                                                                q = (states aut)M.!((next p)M.!x)
