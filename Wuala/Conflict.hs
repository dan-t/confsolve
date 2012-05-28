{-# LANGUAGE OverloadedStrings #-}

module Wuala.Conflict where
import FileConflict
import qualified Wuala.FileNameParser as P
import qualified Data.Text as T

data Parser = Parser

(<++>) = T.append

instance ConflictParser Parser where
   parseConflict _ baseName =
      case P.parse baseName of
           Just (P.FileInfo realBaseName version host) ->
              Just (realBaseName, "Version " <++> version <++> " from " <++> host)

           Nothing -> Nothing
