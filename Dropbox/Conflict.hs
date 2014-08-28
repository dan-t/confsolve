{-# LANGUAGE OverloadedStrings #-}

module Dropbox.Conflict where
import FileConflict
import qualified Dropbox.FileNameParser as P
import Data.Monoid ((<>))

parse :: ConflictParser
parse baseName =
   case P.parse baseName of
        Just (P.FileInfo realBaseName host date) ->
           Just (realBaseName, "Version " <> date <> " from " <> host)

        Nothing -> Nothing
