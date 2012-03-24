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

import Control.Monad (when)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Parse
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Console.GetOpt
import Web.Twitter         -- provided by askitter
import Web.Twitter.OAuth   -- provided by askitter


version = "0.1"


-- command line options
data Options = Options { tokenFile        :: String }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { tokenFile        = error "no token file specified..." }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options =
   [
     -- REGULAR OR GEN MODE
     Option "t" ["token"] 
         (ReqArg (\arg opt -> return opt { tokenFile = arg }) "FILE")
         "name of a file where the token is saved"

     -- HELP
   , Option "h" ["help"] 
         (NoArg $ \_ ->
            do
               prg <- getProgName
               hPutStrLn stderr $ usageInfo prg options
               exitWith ExitSuccess)
         "display help"

     -- VERSION
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
         putStrLn "images given were:"
         mapM_ putStrLn nonOptions


