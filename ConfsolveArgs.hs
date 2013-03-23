{-# LANGUAGE DeriveDataTypeable #-}

module ConfsolveArgs where
import System.Console.CmdArgs

data Confsolve = Confsolve {
   dropbox   :: Bool,
   wuala     :: Bool,
   directory :: FilePath
   } deriving (Data, Typeable, Show, Eq)

confsolve = Confsolve {
   dropbox   = def &= help "Resolve Dropbox file conflicts",
   wuala     = def &= help "Resolve Wuala file conflicts",
   directory = def &= args &= typ "DIRECTORY"
   }
   &= summary summaryInfo
   &= help "A command line tool for resolving conflicts of file synchronizers."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo ]

versionInfo = "confsolve version 0.3.4"
summaryInfo = ""

confsolveArgs :: IO Confsolve
confsolveArgs = cmdArgs confsolve
