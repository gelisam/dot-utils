{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Dot.Sinkify where

import Data.Set (Set)
import Options.Generic
import Test.DocTest
import qualified Data.Set as Set
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)

-- $setup
-- >>> let targetList = ["sink"]
-- >>> let targetSet = Set.fromList targetList
-- >>> let Right inputGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput


testInput :: String
testInput = unlines
  [ "strict digraph {"
  , "  a -> sink;"
  , "  sink -> b;"
  , "  a -> b;"
  , "  sink -> c;"
  , "}"
  ]

test :: IO ()
test = doctest ["src/Dot/Sinkify.hs"]


sinkifyStmt :: Set Vertex -> Dot.Statement -> [Dot.Statement]
sinkifyStmt targetSet stmt = case stmt of
  Dot.EdgeStatement [Dot.ENodeId Dot.NoEdge node1, Dot.ENodeId Dot.DirectedEdge _] _
    | Set.member (vertexFromNodeId node1) targetSet
      -> []
    | otherwise
      -> [stmt]
  _ -> [stmt]


-- |
-- >>> mapM_ print $ sinkifyStmts targetSet inputStmts
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a") Nothing),ENodeId DirectedEdge (NodeId (NameId "sink") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a") Nothing),ENodeId DirectedEdge (NodeId (NameId "b") Nothing)] []
sinkifyStmts :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
sinkifyStmts targetSet = concatMap (sinkifyStmt targetSet)

-- |
-- >>> putStr $ Dot.renderDot $ sinkifyGraph targetList inputGraph
-- strict digraph {
--   a -> sink
--   a -> b
-- }
sinkifyGraph :: [Vertex] -> Dot.Graph -> Dot.Graph
sinkifyGraph targetList (Dot.Graph x y z stmts)
  = Dot.Graph x y z (sinkifyStmts targetSet stmts)
  where
    targetSet :: Set Vertex
    targetSet = Set.fromList targetList


data Options = Options
  { targets :: String
  }
  deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  Options {targets = words -> targets_} <- getRecord "dot-sinkify"
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = sinkifyGraph targets_ inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
