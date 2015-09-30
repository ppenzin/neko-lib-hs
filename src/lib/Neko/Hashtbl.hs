{-|
Module      : Neko.Hashtbl
Description : Minimal hashtable support
Copyright   : (c) Petr Penzin, 2015
License     : BSD2
Maintainer  : penzin.dev@gmail.com
Stability   : experimental
Portability : cross-platform

A minimal support for Neko hashtable type needed for assembly and disassembly

-}
module Neko.Hashtbl where

import qualified Data.Map.Strict as M
import Data.ByteString.Char8 as BC
import Data.ByteString as B
import Data.Word

-- | A minimal hashtable type. Neko supports hashing of all types,
--   but we will focus on strings, since those are necessary for assembly and disassembly
type Hashtbl = M.Map Word32 String

-- | Neko hash function for strings
hash :: String -> Word32
hash s = hash' bs
    where bs = B.reverse $ BC.pack s
          hash' b | B.null b  = 0
          hash' b | otherwise = (hash' $ B.tail b) * 19 + (fromIntegral $ B.head b)

-- | Create an empty Hashtbl, wrapper for corresponding Map function
empty :: Hashtbl
empty = M.empty

-- | Check whether there is a member for a given key, wrapper for corresponding Map function
member :: Word32 -> Hashtbl -> Bool
member = M.member

-- | Check whether there is a member for a given value
memberString :: String -> Hashtbl -> Bool
memberString s h = M.member (hash s) h

-- | Insert a string to Hashtbl
insertString :: String -> Hashtbl -> Hashtbl
insertString s h = M.insert (hash s) s h

-- | Lookup a value by its key, wrapper for corresponding Map function
lookup :: Word32 -> Hashtbl -> Maybe String
lookup = M.lookup

-- | Convert entire Hashtbl ot a list of key-value pairs, wrapper for corresponding Map function
toList :: Hashtbl -> [(Word32, String)]
toList = M.toList

-- | Convert a list of strings into a Hashtbl, generating hash values for them
fromStringList :: [String] -> Hashtbl
fromStringList ss = M.fromList $ Prelude.map (\s -> (hash s, s)) ss

-- | Get a list of elements (values) in the Hashtbl, wrapper for corresponding Map function
elems :: Hashtbl -> [String]
elems = M.elems
