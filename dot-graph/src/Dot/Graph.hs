{-# LANGUAGE LambdaCase #-}
module Dot.Graph where

import Test.DocTest
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Graph.Wrapper as Graph
import qualified Language.Dot as Dot

-- $setup
-- >>> let Right inputDotGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput


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
  Dot.NodeId (Dot.FloatId x) _
    -> show x
  Dot.NodeId (Dot.XmlId x) _
    -> show x

nodeIdFromVertex :: Vertex -> Dot.NodeId
nodeIdFromVertex v
  = Dot.NodeId (Dot.StringId v) Nothing

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

statementFromVertex :: Vertex -> Dot.Statement
statementFromVertex v
  = Dot.NodeStatement (nodeIdFromVertex v) []

-- |
-- >>> mapM_ print $ verticesFromStatements inputStmts
-- "a"
-- "b"
-- "c"
verticesFromStatements :: [Dot.Statement] -> [Vertex]
verticesFromStatements = nubOrd . concatMap verticesFromStatement

-- |
-- >>> mapM_ print $ statementsFromVertices ["a", "b", "c"]
-- NodeStatement (NodeId (StringId "a") Nothing) []
-- NodeStatement (NodeId (StringId "b") Nothing) []
-- NodeStatement (NodeId (StringId "c") Nothing) []
statementsFromVertices :: [Vertex] -> [Dot.Statement]
statementsFromVertices = map statementFromVertex

-- |
-- >>> mapM_ print $ edgesFromStatements inputStmts
-- ("a","b")
-- ("b","c")
edgesFromStatements :: [Dot.Statement] -> [Edge]
edgesFromStatements stmts
  = [ (vertexFromNodeId from, vertexFromNodeId to)
    | Dot.EdgeStatement [Dot.ENodeId _ from, Dot.ENodeId _ to] _ <- stmts
    ]

statementsFromEdges :: [Edge] -> [Dot.Statement]
statementsFromEdges edges
  = [ Dot.EdgeStatement
        [ Dot.ENodeId Dot.NoEdge       (nodeIdFromVertex from)
        , Dot.ENodeId Dot.DirectedEdge (nodeIdFromVertex to)
        ] []
    | (from, to) <- edges
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
-- >>> graphFromDotGraph inputDotGraph
-- fromVerticesEdges [("a","a"),("b","b"),("c","c")] [("a","b"),("b","c")]
graphFromDotGraph :: Dot.Graph -> Graph
graphFromDotGraph (Dot.Graph _ _ _ stmts)
  = graphFromStatements stmts

-- |
-- >>> putStr $ Dot.renderDot $ dotGraphFromGraph $ graphFromDotGraph inputDotGraph
-- digraph {
--   "a"
--   "b"
--   "c"
--   "a" -> "b"
--   "b" -> "c"
-- }
dotGraphFromGraph :: Graph -> Dot.Graph
dotGraphFromGraph g
  = Dot.Graph Dot.UnstrictGraph Dot.DirectedGraph Nothing stmts
  where
    stmts
      = statementsFromVertices (Graph.vertices g)
     ++ statementsFromEdges (Graph.edges g)
