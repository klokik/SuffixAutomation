module SuffixAutomation where

import qualified Data.Map as M
import Prelude
import Data.Maybe
import Data.IORef


data SState a = SState {  len :: Int
                            ,   link :: Maybe Int
                            ,   next :: M.Map a Int    }

type SuffixAutomation a = SuffixAutomation { states :: M.Map a Int }


emptyAut :: SuffixAutomation a
emptyAut = SuffixAutomation { states = M.fromList [(0,SState { len = 0, link = Nothing, next = M.empty })] }

