-- This file is part of the mkcheck project.
-- Licensing information can be found in the LICENSE file.
-- (C) 2017 Nandor Licker. All rights reserved.

module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import Makefile
import Parser

import Debug.Trace

mkEnv :: [(String, String)]
mkEnv = [("MAKE", "make")]


absolute :: FilePath -> IO FilePath
absolute ('~' : path) = do
  cwd <- getHomeDirectory
  return $ normalise (addTrailingPathSeparator cwd ++ path)
absolute path = do
  cwd <- getCurrentDirectory
  return $ normalise (cwd </> path)


resolveIncludes :: FilePath -> Makefile -> IO Makefile
resolveIncludes rootPath Makefile{..}
  = Makefile . concat <$> mapM resolve mkRules
  where
    resolve Include{ miPath } = do
      Makefile{..} <- parseSource rootPath (rootPath </> miPath)
      return mkRules
    resolve e =
      return [e]


parseSource :: FilePath -> FilePath -> IO Makefile
parseSource rootPath path = do
  source <- Text.readFile path
  case parseMakefile source of
    Left error -> do
      putStrLn $ "Parse error: " ++ error ++ " in " ++ path
      exitFailure
    Right make -> do
      resolveIncludes rootPath make


parseTree :: FilePath -> FilePath -> IO [Node]
parseTree rootPath path = do
  make <- parseSource rootPath path
  case expand mkEnv make of
    Left error -> do
      putStrLn $ "Expand error: " ++ error
      exitFailure
    Right make' ->
      return make'


type Rule = (FilePath, String)


explore :: FilePath -> Map FilePath [Node] -> Set Rule -> [Rule] -> IO [Node]
explore _ files _ [] =
  return $ nub (concatMap snd (Map.toList files))
explore rootPath files visited rules@((path, rule):rs)
  | (path, rule) `Set.member` visited =
    explore rootPath files visited rs
  | Just file <- Map.lookup path files = do
    case find (\Node{..} -> ndOutput == rule) file of
      Nothing ->
        explore rootPath files visited rs
      Just Node{..} -> do
        let deps = [(path, rule) | rule <- ndInputs]
        let recs = concatMap parse ndCommands
        let rules' = rs ++ deps ++ recs
        let visited' = Set.insert (path, rule) visited
        explore rootPath files visited' rules'
  | otherwise = do
    traceShowM path
    nodes <- parseTree rootPath path
    explore rootPath (Map.insert path nodes files) visited rules
  where
    parse ('@':cmd) = parse cmd
    parse cmd = case splitOn " " cmd of
      "make" : "-f" : path : rule : [] -> [(rootPath </> path, rule)]
      _ -> []


main :: IO ()
main = getArgs >>= \case
  [path] -> do
    makePath <- absolute path
    let rootPath = takeDirectory makePath
    nodes <- explore rootPath Map.empty Set.empty [(makePath, "all")]
    let phony = ".PHONY" : concat [ndInputs | Node{..} <- nodes, ndOutput == ".PHONY"]
    forM_ nodes $ \Node{..} -> do
      unless (ndOutput `elem` phony) $ do
        putStrLn (ndOutput ++ ": " ++ intercalate ", " (ndInputs \\ phony))
  _ -> do
    putStrLn "Missing argument"
    exitFailure
