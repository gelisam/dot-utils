{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Main where

import Test.DocTest
import qualified Data.Graph.Wrapper as Graph
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)

-- $setup
-- >>> let Right inputDotGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput
-- >>> let g = graphFromDotGraph inputDotGraph


testInput :: String
testInput = unlines
  [ "strict digraph {"
  , "  a -> a;"
  , "  a -> b;"
  , "  a -> c;"
  , "  b -> c;"
  , "}"
  ]

test :: IO ()
test = doctest ["dot-transitive/src/Main.hs"]


isRedundantEdgeStatement :: Graph -> Dot.Statement -> Bool
isRedundantEdgeStatement g = \case
  Dot.EdgeStatement [Dot.ENodeId _ (vertexFromNodeId -> v1), Dot.ENodeId _ (vertexFromNodeId -> v3)] _
    -- TODO: what about "v1 -> v2 -> v3 -> v4;"?
    -> v1 == v3
    || or
         [ v2 /= v3 && Graph.hasPath g v2 v3
           -- TODO: precompute all reachability facts, e.g. using Dijkstra's_algorithm?
         | v2 <- Graph.successors g v1
         , v2 /= v1
         ]
  _ -> False

-- |
-- >>> mapM_ print $ simplifyStmts inputStmts
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "a") Nothing),ENodeId DirectedEdge (NodeId (NameId "b") Nothing)] []
-- EdgeStatement [ENodeId NoEdge (NodeId (NameId "b") Nothing),ENodeId DirectedEdge (NodeId (NameId "c") Nothing)] []
simplifyStmts :: [Dot.Statement] -> [Dot.Statement]
simplifyStmts stmts
  = filter (not . isRedundantEdgeStatement g) stmts
  where
    g = graphFromStatements stmts

-- |
-- >>> putStr $ Dot.renderDot $ simplifyGraph inputDotGraph
-- strict digraph {
--   a -> b
--   b -> c
-- }
simplifyGraph :: Dot.Graph -> Dot.Graph
simplifyGraph (Dot.Graph x y z stmts)
  = Dot.Graph x y z (simplifyStmts stmts)

main :: IO ()
main = do
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = simplifyGraph inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
