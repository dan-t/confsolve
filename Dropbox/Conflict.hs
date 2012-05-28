{-# LANGUAGE OverloadedStrings #-}

module Dropbox.Conflict where
import FileConflict
import qualified Dropbox.FileNameParser as P
import qualified Data.Text as T

data Parser = Parser

(<++>) = T.append

instance ConflictParser Parser where
   parseConflict _ baseName =
      case P.parse baseName of
           Just (P.FileInfo realBaseName host date) ->
              Just (realBaseName, "Version " <++> date <++> " from " <++> host)

           Nothing -> Nothing
