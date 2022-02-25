{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Data.Set (Set)
import Options.Generic
import Test.DocTest
import qualified Data.Graph.Wrapper as Graph
import qualified Data.Set as Set
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)

-- $setup
-- >>> let roots = ["a1", "b2"]
-- >>> let subset = Set.fromList ["a1", "b1", "b2", "y", "z"]
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
  , "  b1 -> y;"
  , "  a2 -> b2;"
  , "  b2 -> y;"
  , "  a3 -> b3;"
  , "  b3 -> y;"
  , "  y -> z;"
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

isEdgeStatementAbout :: Set Vertex -> Dot.Statement -> Bool
isEdgeStatementAbout subset = \case
  Dot.EdgeStatement [Dot.ENodeId _ node1, Dot.ENodeId _ node2] _
    -- TODO: what about "node1 -> root -> node2 -> node3;"?
    -> isNodeIdAbout subset node1
    && isNodeIdAbout subset node2
  _ -> False

-- |
-- >>> mapM_ print $ edgeStatementsAbout subset inputStmts
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a1") Nothing),ENodeId DirectedEdge (NodeId (NameId "b1") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b1") Nothing),ENodeId DirectedEdge (NodeId (NameId "y") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b2") Nothing),ENodeId DirectedEdge (NodeId (NameId "y") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "y") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
edgeStatementsAbout :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
edgeStatementsAbout subset = filter (isEdgeStatementAbout subset)

-- |
-- >>> mapM_ print $ stmtsSubset subset inputStmts
-- NodeStatement (NodeId (NameId "a1") Nothing) []
-- NodeStatement (NodeId (NameId "b1") Nothing) []
-- NodeStatement (NodeId (NameId "b2") Nothing) []
-- NodeStatement (NodeId (NameId "z") Nothing) []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a1") Nothing),ENodeId DirectedEdge (NodeId (NameId "b1") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b1") Nothing),ENodeId DirectedEdge (NodeId (NameId "y") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b2") Nothing),ENodeId DirectedEdge (NodeId (NameId "y") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "y") Nothing),ENodeId DirectedEdge (NodeId (NameId "z") Nothing)] []
stmtsSubset :: Set Vertex -> [Dot.Statement] -> [Dot.Statement]
stmtsSubset subset stmts
  = nodeStatementsAbout subset stmts
 ++ edgeStatementsAbout subset stmts
 -- TODO: what about AttributeStatement, AttributeStatement, AttributeStatement?

-- |
-- >>> putStr $ Dot.renderDot $ graphSubset subset inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   z
--   a1 -> b1
--   b1 -> y
--   b2 -> y
--   y -> z
-- }
graphSubset :: Set Vertex -> Dot.Graph -> Dot.Graph
graphSubset subset (Dot.Graph x y z stmts)
  = Dot.Graph x y z (stmtsSubset subset stmts)

-- |
-- >>> mapM_ print $ reachableFromRoots roots Nothing g
-- "a1"
-- "b1"
-- "b2"
-- "y"
-- "z"
-- >>> mapM_ print $ reachableFromRoots roots (Just 0) g
-- "a1"
-- "b2"
-- >>> mapM_ print $ reachableFromRoots roots (Just 1) g
-- "a1"
-- "b1"
-- "b2"
-- "y"
-- >>> mapM_ print $ reachableFromRoots roots (Just 2) g
-- "a1"
-- "b1"
-- "b2"
-- "y"
-- "z"
reachableFromRoots :: [Vertex] -> Maybe Int -> Graph -> Set Vertex
reachableFromRoots roots Nothing g
  = Set.fromList
  . concatMap (Graph.reachableVertices g)
  $ roots
reachableFromRoots roots (Just 0) _
  = Set.fromList roots
reachableFromRoots roots (Just depth) g
  = vertices
 <> (Set.fromList . concatMap (Graph.successors g) $ vertices)
  where
    vertices :: Set Vertex
    vertices = reachableFromRoots roots (Just (pred depth)) g
  -- TODO: use a proper shortest-paths algorithm, e.g. Dijkstra's

-- |
-- >>> putStr $ Dot.renderDot $ graphClosure roots Nothing inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   z
--   a1 -> b1
--   b1 -> y
--   b2 -> y
--   y -> z
-- }
-- >>> putStr $ Dot.renderDot $ graphClosure roots (Just 0) inputGraph
-- digraph {
--   a1
--   b2
-- }
-- >>> putStr $ Dot.renderDot $ graphClosure roots (Just 1) inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   a1 -> b1
--   b1 -> y
--   b2 -> y
-- }
-- >>> putStr $ Dot.renderDot $ graphClosure roots (Just 2) inputGraph
-- digraph {
--   a1
--   b1
--   b2
--   z
--   a1 -> b1
--   b1 -> y
--   b2 -> y
--   y -> z
-- }
graphClosure :: [String] -> Maybe Int -> Dot.Graph -> Dot.Graph
graphClosure roots depth dotGraph
  = graphSubset subset dotGraph
  where
    g :: Graph
    g = graphFromGraph dotGraph

    subset :: Set Vertex
    subset = reachableFromRoots roots depth g


data Options = Options
  { roots :: String
  , depth :: Maybe Int
  }
  deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  Options {roots = words -> roots, depth = depth} <- getRecord "dot-closure"
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = graphClosure roots depth inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
