{-# LANGUAGE LambdaCase #-}
module Dot.Graph where

import Test.DocTest
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Graph.Wrapper as Graph
import qualified Language.Dot as Dot

-- $setup
-- >>> let Right inputGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput


testInput :: String
testInput = unlines
  [ "strict digraph {"
  , "  a -> b;"
  , "  b -> c;"
  , "}"
  ]

test :: IO ()
test = doctest ["dot-graph/src/Dot/Graph.hs"]


type Vertex = String
type Edge = Graph.Edge Vertex
type Graph = Graph.Graph Vertex Vertex

vertexFromNodeId :: Dot.NodeId -> Vertex
vertexFromNodeId = \case
  Dot.NodeId (Dot.NameId s) _
    -> s
  Dot.NodeId (Dot.StringId s) _
    -> s
  Dot.NodeId (Dot.IntegerId n) _
    -> show n

verticesFromEntity :: Dot.Entity -> [Vertex]
verticesFromEntity = \case
  Dot.ENodeId _ nodeId
    -> [vertexFromNodeId nodeId]
  _ -> []

verticesFromStatement :: Dot.Statement -> [Vertex]
verticesFromStatement = \case
  Dot.NodeStatement nodeId _
    -> [vertexFromNodeId nodeId]
  Dot.EdgeStatement entities _
    -> concatMap verticesFromEntity entities
  _ -> []

-- |
-- >>> mapM_ print $ verticesFromStatements inputStmts
-- "a"
-- "b"
-- "c"
verticesFromStatements :: [Dot.Statement] -> [Vertex]
verticesFromStatements = nubOrd . concatMap verticesFromStatement

-- |
-- >>> mapM_ print $ edgesFromStatements inputStmts
-- ("a","b")
-- ("b","c")
edgesFromStatements :: [Dot.Statement] -> [Edge]
edgesFromStatements stmts
  = [ (vertexFromNodeId from, vertexFromNodeId to)
    | Dot.EdgeStatement [Dot.ENodeId _ from, Dot.ENodeId _ to] _ <- stmts
    ]

-- |
-- >>> graphFromStatements inputStmts
-- fromVerticesEdges [("a","a"),("b","b"),("c","c")] [("a","b"),("b","c")]
graphFromStatements :: [Dot.Statement] -> Graph
graphFromStatements stmts
  = Graph.fromVerticesEdges
      (fmap (\x -> (x, x)) vertices)
      edges
  where
    vertices = verticesFromStatements stmts
    edges = edgesFromStatements stmts

-- |
-- >>> graphFromGraph inputGraph
-- fromVerticesEdges [("a","a"),("b","b"),("c","c")] [("a","b"),("b","c")]
graphFromGraph :: Dot.Graph -> Graph
graphFromGraph (Dot.Graph _ _ _ stmts)
  = graphFromStatements stmts
