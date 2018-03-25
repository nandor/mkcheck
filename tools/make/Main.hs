-- This file is part of the mkcheck project.
-- Licensing information can be found in the LICENSE file.
-- (C) 2017 Nandor Licker. All rights reserved.

module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import Makefile
import Parser



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


expandRule :: FilePath -> String -> [Node] -> IO [Node]
expandRule root rule make
  = case find (\Node{..} -> ndOutput == rule) make of
      Nothing -> return make
      Just Node{..} -> do
        -- TODO: detect cycles
        depNodes <- concat <$> mapM (\dep -> expandRule root dep make) ndInputs

        recs <- forM (concatMap parse ndCommands) $ \(path, rule) -> do
          rec <- absolute path >>= parseTree root
          next <- expandRule root rule rec
          return next

        return $ nub (depNodes ++ concat recs)
  where
    parse cmd = case splitOn " " cmd of
      "make" : "-f" : path : rule : [] -> [(root </> path, rule)]
      _ -> []


main :: IO ()
main = getArgs >>= \case
  [path] -> do
    makePath <- absolute path
    let rootPath = takeDirectory makePath
    make <- parseTree rootPath makePath
    make' <- expandRule rootPath "all" make
    let phony = ".PHONY" : concat [ndInputs | Node{..} <- make', ndOutput == ".PHONY"]
    forM_ make' $ \Node{..} -> do
      unless (ndOutput `elem` phony) $ do
        putStrLn (ndOutput ++ ": " ++ intercalate ", " (ndInputs \\ phony))
  _ -> do
    putStrLn "Missing argument"
    exitFailure
