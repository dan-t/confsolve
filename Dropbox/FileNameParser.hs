{-# LANGUAGE OverloadedStrings #-}

module Dropbox.FileNameParser where

import Control.Applicative ((<*>), (<$>), (*>), (<*), (<|>))
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T

parse :: T.Text -> Maybe FileInfo
parse fileName =
   case P.feed (P.parse fileInfo fileName) T.empty of
        P.Fail _ _ _ -> Nothing
        P.Partial _  -> Nothing
        P.Done _ r   -> Just r

fileInfo :: P.Parser FileInfo
fileInfo = FileInfo <$> (T.strip <$> tillLeftPar)
                    <*> (P.char '(' *> till')
                    <*> (P.string "'s conflicted copy " *> tillRightPar <* P.char ')')
   where
      tillLeftPar  = P.takeWhile1 (\c -> c /= '(')
      till'        = P.takeWhile1 (\c -> c /= '\'')
      tillRightPar = P.takeWhile1 (\c -> c /= ')')

data FileInfo = FileInfo {
   fileName :: T.Text,
   host     :: T.Text,
   date     :: T.Text
   } deriving (Show)
