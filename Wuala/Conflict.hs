{-# LANGUAGE OverloadedStrings #-}

module Wuala.Conflict where
import FileConflict
import qualified Wuala.FileNameParser as P
import Data.Monoid ((<>))

parse :: ConflictParser
parse baseName =
   case P.parse baseName of
        Just (P.FileInfo realBaseName version host) ->
           Just (realBaseName, "Version " <> version <> " from " <> host)

        Nothing -> Nothing
