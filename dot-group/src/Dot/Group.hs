{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Dot.Group where

import Data.List.Extra (mconcatMap, wordsBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Options.Generic
import Test.DocTest
import qualified Data.Graph.Wrapper as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Dot as Dot

import Dot.Graph hiding (test, testInput)
import qualified Dot.Subset as Subset

-- $setup
-- >>> let testGroupList = [Group "[1]" ["a1","b1","c1"], Group "[2]" ["a2","b2","c2"]]
-- >>> let testGroup = head testGroupList
-- >>> let Right inputGraph@(Dot.Graph _ _ _ inputStmts) = Dot.parseDot "testInput" testInput
-- >>> let g = graphFromDotGraph inputGraph

testGroupInput :: String
testGroupInput = unlines
  [ "[1]"
  , "a1"
  , "b1"
  , "c1"
  , ""
  , "[2]"
  , "a2"
  , "b2"
  , "c2"
  ]

testInput :: String
testInput = unlines
  [ "digraph {"
  , "  a1;"
  , "  a2;"
  , "  b1;"
  , "  b2;"
  , "  c1;"
  , "  c2;"
  , "  d;"
  , ""
  , "  a1 -> a2;"
  , "  b1 -> c2;"
  , "  b2 -> d;"
  , "}"
  ]

test :: IO ()
test = doctest ["dot-group/src/Dot/Group.hs"]


data Group = Group
  { groupLeader :: Vertex
  , groupNonLeaders :: [Vertex]
  }
  deriving (Eq, Show)

-- |
-- >>> mapM_ (print . groupMembers) testGroupList
-- ["[1]","a1","b1","c1"]
-- ["[2]","a2","b2","c2"]
groupMembers :: Group -> [Vertex]
groupMembers (Group {..})
  = groupLeader : groupNonLeaders

-- |
-- >>> parseGroup ["[1]","a1","b1","c1"]
-- Right (Group {groupLeader = "[1]", groupNonLeaders = ["a1","b1","c1"]})
parseGroup :: [String] -> Either String Group
parseGroup [] = do
  Left "empty group"
parseGroup (leader:nonLeaders) = do
  pure $ Group leader nonLeaders

-- |
-- >>> Right parsedGroupList = parseGroups testGroupInput
-- >>> parsedGroupList == testGroupList
-- True
parseGroups :: String -> Either String [Group]
parseGroups = traverse parseGroup . wordsBy (== "") . lines

-- |
-- >>> :{
-- putStr
--   $ Dot.renderDot
--   $ dotGraphFromGraph
--   $ addVerticesAndEdges (Set.fromList ["a","b"]) (Map.singleton "a" ["b"])
--   $ addVerticesAndEdges (Set.fromList ["b","c"]) (Map.singleton "b" ["c"])
--   $ Graph.fromListSimple [("c",[])]
-- :}
-- digraph {
--   "a"
--   "b"
--   "c"
--   "a" -> "b"
--   "b" -> "c"
-- }
addVerticesAndEdges
  :: Set Vertex
  -> Map Vertex [Vertex]
  -> Graph.Graph Vertex Vertex
  -> Graph.Graph Vertex Vertex
addVerticesAndEdges newVerticesSet newEdgesMap g
  = Graph.fromListSimple
      ( [ (v1, newEdges v1)
        | v1 <- Set.toList strictlyNewVertices
        ]
     ++ [ (v1, newEdges v1 ++ Graph.successors g v1)
        | v1 <- Graph.vertices g
        ]
      )
  where
    existingVertices :: Set Vertex
    existingVertices = Set.fromList $ Graph.vertices g

    strictlyNewVertices :: Set Vertex
    strictlyNewVertices = newVerticesSet Set.\\ existingVertices

    newEdges :: Vertex -> [Vertex]
    newEdges v1
      = fromMaybe []
      $ Map.lookup v1 newEdgesMap

-- |
-- >>> groupCycle testGroup
-- fromList [("[1]",["a1"]),("a1",["b1"]),("b1",["c1"]),("c1",["[1]"])]
groupCycle
  :: Group
  -> Map Vertex [Vertex]
groupCycle (Group {..})
  = go groupLeader groupNonLeaders
  where
    go :: Vertex -> [Vertex] -> Map Vertex [Vertex]
    go v1 []
      = Map.singleton v1 [groupLeader]
    go v1 (v2:vs)
      = Map.insert v1 [v2]
      $ go v2 vs

-- |
-- >>> putStr $ Dot.renderDot $ dotGraphFromGraph $ addGroupCycles testGroupList g
-- digraph {
--   "[1]"
--   "[2]"
--   "a1"
--   "a2"
--   "b1"
--   "b2"
--   "c1"
--   "c2"
--   "d"
--   "[1]" -> "a1"
--   "[2]" -> "a2"
--   "a1" -> "b1"
--   "a1" -> "a2"
--   "a2" -> "b2"
--   "b1" -> "c1"
--   "b1" -> "c2"
--   "b2" -> "c2"
--   "b2" -> "d"
--   "c1" -> "[1]"
--   "c2" -> "[2]"
-- }
addGroupCycles
  :: [Group]
  -> Graph.Graph Vertex Vertex
  -> Graph.Graph Vertex Vertex
addGroupCycles groupList
  = addVerticesAndEdges
      (Set.fromList leaders)
      (mconcatMap groupCycle groupList)
  where
    leaders :: [Vertex]
    leaders = fmap groupLeader groupList

-- |
-- >>> putStr $ Dot.renderDot $ dotGraphGroup testGroupList inputGraph
-- digraph {
--   "[1]"
--   "[2]"
--   "d"
--   "[1]" -> "[2]"
--   "[2]" -> "d"
-- }
dotGraphGroup :: [Group] -> Dot.Graph -> Dot.Graph
dotGraphGroup groupList dotGraph
  = -- TODO: preserve vertex attributes? we can't preserve the attributes of the edges.
    dotGraphFromGraph
  $ Subset.simplifyGraph
  $ Subset.graphSubset (leaders ++ Set.toList ungroupedVertices)
  $ addGroupCycles groupList
  $ g
  where
    g :: Graph.Graph Vertex Vertex
    g = graphFromDotGraph dotGraph

    leaders :: [Vertex]
    leaders = fmap groupLeader groupList

    existingVertices :: Set Vertex
    existingVertices = Set.fromList $ Graph.vertices g

    groupedVertices :: Set Vertex
    groupedVertices = mconcatMap (Set.fromList . groupMembers) groupList

    ungroupedVertices :: Set Vertex
    ungroupedVertices = existingVertices Set.\\ groupedVertices


data Options = Options
  { groupFile :: FilePath
  }
  deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  Options {groupFile} <- getRecord "dot-group"
  groupString <- readFile groupFile
  inputString <- getContents
  case parseGroups groupString of
    Left err -> do
      error $ show err
    Right groupList -> do
      case Dot.parseDot "stdin" inputString of
        Left err -> do
          error $ show err
        Right inputGraph -> do
          let outputGraph = dotGraphGroup groupList inputGraph
          let outputString = Dot.renderDot outputGraph
          putStr outputString
