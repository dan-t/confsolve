
module ParseInput where
import Data.Char (toLower)

type FileNum = Int
data UserReply = TakeFile FileNum
               | MoveToTrash
               | ShowDiff
               | ShowDiffWith FileNum
               | ShowDiffBetween FileNum FileNum
               | Skip
               | Quit
               | Help
               deriving Show


type NumFiles = Int
parseInput :: String -> NumFiles -> Maybe UserReply
parseInput str numFiles =
   case map toLower str of
        ('t':rest) | [num] <- parseInts rest -> if validFileNum num 
                                                   then Just $ TakeFile num
                                                   else Nothing
                   | otherwise               -> Nothing

        ('m':[])   -> Just MoveToTrash

        ('d':rest) -> case parseInts rest of
                           []           -> if numFiles == 1 then Just ShowDiff else Nothing
                           [num]        -> if validFileNum num
                                              then Just $ ShowDiffWith num
                                              else Nothing
                           [num1, num2] -> if validFileNum num1 && validFileNum num2
                                              then Just $ ShowDiffBetween num1 num2
                                              else Nothing
                           _            -> Nothing

        ('s':[])   -> Just Skip
        ('q':[])   -> Just Quit
        ('h':[])   -> Just Help
        ('?':[])   -> Just Help

        _          -> Nothing

   where
      validFileNum num = num > 0 && num <= numFiles


parseInts :: String -> [Int]
parseInts str = go str []
   where
      go str ints
         | [(int, rest)] <- reads str :: [(Int, String)] = go rest (ints ++ [int])
         | otherwise                                     = ints
