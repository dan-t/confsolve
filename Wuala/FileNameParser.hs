{-# LANGUAGE OverloadedStrings #-}

module Wuala.FileNameParser where

import Control.Applicative ((<*>), (<$>), (*>), (<*), (<|>), pure)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Data.Char (isNumber)

parse :: T.Text -> Maybe FileInfo
parse fileName =
   case P.feed (P.parse fileInfo fileName) T.empty of
        P.Fail _ _ _ -> Nothing
        P.Partial _  -> Nothing
        P.Done _ r   -> Just r

fileInfo :: P.Parser FileInfo
fileInfo = FileInfo <$> (T.strip <$> tillLeftPar)
                    <*> (P.string "(conflicting version " *> version)
                    <*> maybeHost
   where
      maybeHost    = (P.string " from " *> tillRightPar) <|> (pure "" <* P.char ')')
      version      = P.takeWhile1 isNumber
      tillLeftPar  = P.takeWhile1 (/= '(')
      tillRightPar = P.takeWhile1 (/= ')')

data FileInfo = FileInfo {
   fileName :: T.Text,
   version  :: T.Text,
   host     :: T.Text
   } deriving (Show)
