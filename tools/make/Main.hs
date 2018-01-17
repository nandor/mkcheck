-- This file is part of the mkcheck project.
-- Licensing information can be found in the LICENSE file.
-- (C) 2017 Nandor Licker. All rights reserved.

module Main where

import           Control.Monad
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import Makefile
import Parser



absolute :: FilePath -> IO FilePath
absolute ('~' : path) = do
  cwd <- getHomeDirectory
  return $ normalise (addTrailingPathSeparator cwd ++ path)
absolute path = do
  cwd <- getCurrentDirectory
  return $ normalise (cwd </> path)


main :: IO ()
main = getArgs >>= \case
  [path] -> do
    makePath <- absolute path
    source <- Text.readFile makePath
    case parseMakefile source >>= expand of
      Left error -> do
        putStrLn $ "Parse error: " ++ error
        exitFailure
      Right make -> do
        let phony = ".PHONY" : concat [ndInputs | Node{..} <- make, ndOutput == ".PHONY"]
        forM_ make $ \Node{..} -> do
          unless (ndOutput `elem` phony) $ do
            putStrLn (ndOutput ++ ": " ++ intercalate ", " ndInputs)
  _ -> do
    putStrLn "Missing argument"
    exitFailure
