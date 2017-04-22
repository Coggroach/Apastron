module Main where

import System.Environment
import Crawler
import Search

crawlerStringCompare :: String
crawlerStringCompare = "0"

runSelectedApp :: String -> IO()
runSelectedApp s = 
    if s == crawlerStringCompare then
        mkCrawler
    else
        mkSearch

main :: IO ()
main = do
    args <- getArgs
    runSelectedApp (head args)
