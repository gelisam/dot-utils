{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Dot.Subset where

import Options.Generic
import Test.DocTest
import qualified Data.Graph.Wrapper as Graph
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)

-- $setup
-- >>> let subset = ["a", "b", "c"]
-- >>> let Right inputGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput
-- >>> let g = graphFromDotGraph inputGraph

testInput :: String
testInput = unlines
  [ "digraph {"
  , "  x0;"
  , "  x1;"
  , "  x2;"
  , "  x3;"
  , "  x4;"
  , "  x5;"
  , "  a;"
  , "  b;"
  , "  c;"
  , "  x0 -> a;"
  , "  a -> x1;"
  , "  a -> x2;"
  , "  x1 -> b;"
  , "  x2 -> b;"
  , "  b -> x3;"
  , "  x3 -> x4;"
  , "  x4 -> c;"
  , "  c -> x5;"
  , "}"
  ]

test :: IO ()
test = doctest ["dot-subset/src/Dot/Subset.hs"]


toDotGraph :: Graph -> Dot.Graph
toDotGraph = undefined

graphSubset :: [String] -> Graph -> Graph
graphSubset subset g
  = Graph.fromListSimple
      [ (from, filter (Graph.hasPath g from) subset)
      | from <- subset
      ]

-- | Remove edges implied by the transitive closure of the existing edges
simplifyGraph :: Graph -> Graph
simplifyGraph g
  = Graph.fromListSimple
      [ (v1, filter (not . isRedundant) (Graph.successors g v1))
      | v1 <- Graph.vertices g
      , let -- TODO: deduplicate from dot-transitive's 'isRedundantEdgeStatement'
            isRedundant
              :: Vertex -> Bool
            isRedundant v3
              = v1 == v3
             || or
                  [ v2 /= v3 && Graph.hasPath g v2 v3
                    -- TODO: precompute all reachability facts, e.g. using Dijkstra's_algorithm?
                  | v2 <- Graph.successors g v1
                  , v2 /= v1
                  ]
      ]

-- |
-- >>> putStr $ Dot.renderDot $ dotGraphSubset subset inputGraph
-- digraph {
--   "a"
--   "b"
--   "c"
--   "a" -> "b"
--   "b" -> "c"
-- }
dotGraphSubset :: [String] -> Dot.Graph -> Dot.Graph
dotGraphSubset subset
  = -- TODO: preserve vertex attributes? we can't preserve the attributes of the edges.
    dotGraphFromGraph
  . simplifyGraph
  . graphSubset subset
  . graphFromDotGraph


data Options = Options
  { subset :: String
  }
  deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  Options {subset = words -> subset} <- getRecord "dot-subset"
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = dotGraphSubset subset inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
