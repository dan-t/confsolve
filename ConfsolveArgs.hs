{-# LANGUAGE DeriveDataTypeable, CPP #-}

module ConfsolveArgs where
import System.Console.CmdArgs

#ifdef CABAL
import Data.Version (showVersion)
import Paths_confsolve (version)
#endif

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
   &= summary ""
   &= help "A command line tool for resolving conflicts of file synchronizers."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo ]

versionInfo :: String
versionInfo =
#ifdef CABAL
   "confsolve version " ++ showVersion version
#else
   "confsolve version unknown (not built with cabal)"
#endif

confsolveArgs :: IO Confsolve
confsolveArgs = cmdArgs confsolve
