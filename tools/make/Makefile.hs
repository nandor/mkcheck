-- This file is part of the mkcheck project.
-- Licensing information can be found in the LICENSE file.
-- (C) 2017 Nandor Licker. All rights reserved.

module Makefile where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace



data Entry
  = Assignment
    { maKey :: String
    , maValue :: String
    }
  | Rule
    { mrOutput :: String
    , mrInputs :: [String]
    , mrCommands :: [String]
    }
  deriving (Eq, Ord, Show)


data Makefile
  = Makefile { mkRules :: [Entry] }
  deriving (Eq, Ord, Show)


data Node
  = Node
    { ndOutput :: String
    , ndInputs :: [String]
    , ndCommands :: [String]
    }
  deriving (Eq, Ord, Show)



expand :: Makefile -> Either String [Node]
expand Makefile{ mkRules } =
  sequence [toNode mrOutput mrInputs mrCommands | Rule{..} <- mkRules]
  where
    vars = Map.fromList [(maKey, maValue) | Assignment{..} <- mkRules]

    replace = \case
      '$' : '(' : str ->
        case break (==')') str of
          (key, ')' : rest) ->
            case Map.lookup key vars of
              Nothing ->
                Left "No such variable"
              Just val ->
                replace rest >>= \chs -> Right (val ++ chs)
          _ ->
            Left "Malformed template"
      ch : chs ->
        replace chs >>= \chs' -> Right (ch : chs')
      [] ->
        Right []

    -- Convers an AST rule to a node.
    toNode output inputs commands = do
      output' <- replace output
      inputs' <- sequence . map replace $ inputs
      commands' <- sequence . map replace $ commands
      Right (Node output' inputs' commands')
