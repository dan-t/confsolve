{-# LANGUAGE OverloadedStrings #-}

module FileConflict where
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>), (<.>))

-- base name possibly containing conflict info
type BaseName     = T.Text

-- base name without conflict info
type RealBaseName = T.Text

-- conflicting file details displayed to the user
type Details      = T.Text

class ConflictParser a where
   parseConflict :: a -> BaseName -> Maybe (RealBaseName, Details)

-- the details and filePath of a conflicting file
data ConflictingFile = ConflictingFile {
   details  :: Details,
   filePath :: FP.FilePath } deriving (Show)

-- file path having a conflict
data Conflict = Conflict {
   origFilePath     :: FP.FilePath,
   conflictingFiles :: [ConflictingFile]
   } deriving (Show)
