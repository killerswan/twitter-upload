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

import Data.List.Split
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO (stderr, hPutStrLn)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr(Option), ArgDescr(ReqArg, NoArg), ArgOrder(Permute))
import Web.Twitter (uploadImage, uploadImageWithParams)
import Web.Twitter.OAuth (readToken)


version :: String
version = "0.1"


-- command line options
data Options = Options { tokenFile :: String
                       , image :: String
                       , status :: String
                       , coord :: Maybe (Double, Double)
                       }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { tokenFile = error "no token file specified..."
                      , image = error "no image file specified..."
                      , status = ""
                      , coord = Nothing
                      }

handleCoordinate coordStr opt =
   return opt { coord = Just latLon }
   where
      latLon = (toDbl $ coordStr' !! 0, toDbl $ coordStr' !! 1)
      toDbl x = read x :: Double
      coordStr' = splitOn "," coordStr
   

--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options =
   [ Option "t" ["tokenFile"] 
         (ReqArg (\arg opt -> return opt { tokenFile = arg }) "FILE")
         "file where the oauth token is saved"

   , Option "i" ["image"] 
         (ReqArg (\arg opt -> return opt { image = arg }) "FILE")
         "image to be uploaded"

   , Option "s" ["status"] 
         (ReqArg (\arg opt -> return opt { status = arg }) "MESSAGE")
         "status tweet to be posted"

   , Option "c" ["coordinates"] 
         (ReqArg handleCoordinate "lat,lon")
         "latitude and longitude to associate with this image"

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
      let (actions, _, _) = getOpt Permute options args

      -- process the defaults with those actions
      opts <- foldl (>>=) (return defaultOpts) actions

      do
         token  <- readToken (tokenFile opts)
         case coord opts
            of Nothing  -> uploadImage token (status opts) (image opts)
               Just loc -> uploadImageWithParams token (status opts) (image opts) (Just True) Nothing (Just loc) Nothing (Just True)  -- making this function call sucks, too many args

