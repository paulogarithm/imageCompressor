module Main (main) where

import Compressor(compressor)

import System.Environment
import System.Exit
import GetConfArgs
import Options.Applicative

main :: IO ()
main = do
    args <- getArgs
    case (execParserPure defaultPrefs confCommandParser args) of
        Success conf -> displayConf conf
        Failure _ -> exitWith (ExitFailure 84)
        CompletionInvoked _ -> exitWith (ExitFailure 84)

displayDouble :: Double -> IO()
displayDouble n = print n

displayConf :: Conf -> IO()
displayConf (Conf nbCluster convergeLimit fileName) =
    print nbCluster >>
        displayDouble convergeLimit >>
            putStrLn fileName
