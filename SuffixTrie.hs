module SuffixTrie where

import qualified Data.Map as M
import Prelude
import Data.Maybe
import Data.IORef
import System.IO


data TrieState a = TrieState {  len :: Int
                            ,   link :: Maybe ( IORef (TrieState a) )
                            ,   next :: M.Map a (TrieState a)  }

type SuffixTrie = TrieState


emptyTrie :: SuffixTrie a
emptyTrie = TrieState { len = 0
                    ,   link = Nothing
                    ,   next = M.empty }

instance Show x => Show (TrieState x) where
    show t = "{" ++ show (len t) ++ ",[" ++ unlink (link t) ++ "]," ++ show (next t) ++ "}"
        where unlink a = case a of
                            Just b -> do
                                val <- readIORef b
                                return show val
                            Nothing -> do
                                return IO "null"