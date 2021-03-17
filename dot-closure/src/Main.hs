{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Main where

import Data.Set (Set)
import Options.Generic
import Test.DocTest
import System.Environment
import qualified Data.Graph.Wrapper as Graph
import qualified Data.Set as Set
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)

-- $setup
-- >>> let roots = ["a1", "b2"]
-- >>> let subset = Set.fromList ["a1", "b1", "b2", "z"]
-- >>> let Right inputGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput
-- >>> let g = graphFromGraph inputGraph

testInput :: String
testInput = unlines
  [ "digraph {"
  , "  a1;"
  , "  b1;"
  , "  a2;"
  , "  b2;"
  , "  a3;"
  , "  b3;"
  , "  z;"
  , "  a1 -> b1;"
  , "  b1 -> z;"
  , "  a2 -> b2;"
  , "  b2 -> z;"
  , "  a3 -> b3;"
  , "  b3 -> z;"
  , "}"
  ]

test :: IO ()
test = doctest ["dot-closure/src/Main.hs"]


isNodeIdAbout :: Set Vertex -> Dot.NodeId -> Bool
isNodeIdAbout subset nodeId
  = vertexFromNodeId nodeId `elem` subset

isNodeStatementAbout :: Set Vertex -> Dot.Statement -> Bool
isNodeStatementAbout subset = \case
  Dot.NodeStatement nodeId _
    -> isNodeIdAbout subset nodeId
  _ -> False

-- |
-- >>> mapM_ print $ nodeStatementsAbout subset inputStmts
-- NodeStatement (NodeId (NameId "a1") Nothing) []
-- NodeStatement (NodeId (NameId "b1") Nothing) []
-- NodeStatement (NodeId (NameId "b2") Nothing) []
-- NodeStatement (NodeId (NameId "z") Nothing) []
nodeStatementsAbout :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
nodeStatementsAbout subset = filter (isNodeStatementAbout subset)

isEdgeStatementFrom :: Set Vertex -> Dot.Statement -> Bool
isEdgeStatementFrom subset = \case
  Dot.EdgeStatement [Dot.ENodeId _ nodeId, _] _
    -- TODO: what about "node1 -> root -> node2 -> node3;"?
    -> isNodeIdAbout subset nodeId
  _ -> False

-- |
-- >>> mapM_ print $ edgeStatementsFrom subset inputStmts
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a1") Nothing),ENodeId DirectedEdge (NodeId (NameId "b1") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b1") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b2") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
edgeStatementsFrom :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
edgeStatementsFrom subset = filter (isEdgeStatementFrom subset)

-- |
-- >>> mapM_ print $ stmtsSubset subset inputStmts
-- NodeStatement (NodeId (NameId "a1") Nothing) []
-- NodeStatement (NodeId (NameId "b1") Nothing) []
-- NodeStatement (NodeId (NameId "b2") Nothing) []
-- NodeStatement (NodeId (NameId "z") Nothing) []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a1") Nothing),ENodeId DirectedEdge (NodeId (NameId "b1") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b1") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b2") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
stmtsSubset :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
stmtsSubset subset stmts
  = nodeStatementsAbout subset stmts
 ++ edgeStatementsFrom subset stmts
 -- TODO: what about AttributeStatement, AttributeStatement, AttributeStatement?

-- |
-- >>> putStr $ Dot.renderDot $ graphSubset subset inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   z
--   a1 -> b1
--   b1 -> z
--   b2 -> z
-- }
graphSubset :: Set Vertex -> Dot.Graph -> Dot.Graph
graphSubset subset (Dot.Graph x y z stmts)
  = Dot.Graph x y z (stmtsSubset subset stmts)

-- |
-- >>> mapM_ print $ reachableFromRoots roots g
-- "a1"
-- "b1"
-- "b2"
-- "z"
reachableFromRoots :: [Vertex] -> Graph -> Set Vertex
reachableFromRoots roots g
  = Set.fromList
  . concatMap (Graph.reachableVertices g)
  $ roots

-- |
-- >>> putStr $ Dot.renderDot $ graphClosure roots inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   z
--   a1 -> b1
--   b1 -> z
--   b2 -> z
-- }
graphClosure :: [String] -> Dot.Graph -> Dot.Graph
graphClosure roots dotGraph
  = graphSubset subset dotGraph
  where
    g :: Graph
    g = graphFromGraph dotGraph

    subset :: Set Vertex
    subset = reachableFromRoots roots g


data Options = Options
  { roots :: String
  }
  deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  Options {roots = words -> roots} <- getRecord "dot-closure"
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = graphClosure roots inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
