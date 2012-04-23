-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD3
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- A tool to upload photos to Twitter


module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO (stderr, hPutStrLn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr(Option), ArgDescr(ReqArg, NoArg), ArgOrder(Permute))
import Web.Twitter (uploadImage)
import Web.Twitter.OAuth (readToken)


version :: String
version = "0.1"


-- command line options
data Options = Options { tokenFile :: String }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { tokenFile = error "no token file specified..." }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options =
   [
     Option "t" ["tokenFile"] 
         (ReqArg (\arg opt -> return opt { tokenFile = arg }) "FILE")
         "name of a file where the token is saved"

   , Option "h" ["help"] 
         (NoArg $ \_ ->
            do
               prg <- getProgName
               hPutStrLn stderr $ usageInfo prg options
               exitWith ExitSuccess)
         "display help"

   , Option "v" ["version"] 
         (NoArg $ \_ ->
            do
               me <- getProgName
               hPutStrLn stderr $ me ++ " version " ++ version
               exitWith ExitSuccess)
         "display version"
   ]


main :: IO ()
main =
   do 
      args <- getArgs

      -- call getOpt, ignoring errors
      let (actions, nonOptions, _) = getOpt Permute options args

      -- process the defaults with those actions
      opts <- foldl (>>=) (return defaultOpts) actions

      do
         token  <- readToken (tokenFile opts)
         let status    = nonOptions !! 0
         let imageName = nonOptions !! 1

         uploadImage token status imageName

