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
import Web.Twitter (updateStatus, updateStatusWithAttr, StatusAttr(..), uploadImage, uploadImageWithAttr, ImageAttr(..))
import Web.Twitter.OAuth (readToken)


version :: String
version = "0.1"


-- command line options
data Options = Options { tokenFile :: String
                       , image :: Maybe String
                       , coord :: Maybe (Double, Double)
                       , status :: String }


-- command line defaults
defaultOpts :: Options
defaultOpts = Options { tokenFile = error "no token file given..."
                      , image = Nothing
                      , coord = Nothing
                      , status = ""
                      }

-- given a position "43.0,-119.67", parse the option
getCoordinate :: String -> Options -> IO Options
getCoordinate latlon opt =
   return opt { coord = Just (lat, lon) }
      where
         lat = read (fst coord') :: Double
         lon = read (drop 1 $ snd coord') :: Double
         coord' = break (==',') latlon
         

-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options =
   [ Option "t" ["tokenFile"] 
         (ReqArg (\arg opt -> return opt { tokenFile = arg }) "FILE")
         "file where the oauth token is saved"

   , Option "i" ["image"] 
         (ReqArg (\arg opt -> return opt { image = Just arg }) "FILE")
         "image to be uploaded"

   , Option "s" ["status"] 
         (ReqArg (\arg opt -> return opt { status = arg }) "MESSAGE")
         "status tweet to be posted"

   , Option "c" ["coord"] 
         (ReqArg getCoordinate "lat,lon")
         "coordinates for this tweet"

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
         let tweet = status opts
         new <- case (coord opts, image opts)
                of (Nothing,     Just imagePath) -> uploadImage token tweet imagePath
                   (Just latlon, Just imagePath) -> uploadImageWithAttr token tweet imagePath
                                                      [ ImageDisplayCoords
                                                      , ImageLatLon (fst latlon) (snd latlon) ]
                   (Just latlon, Nothing)        -> updateStatusWithAttr token tweet
                                                      [ StatusDisplayCoords
                                                      , StatusLatLon (fst latlon) (snd latlon) ]
                   (Nothing, Nothing)            -> updateStatus token tweet

         print new

