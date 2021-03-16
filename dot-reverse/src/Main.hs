{-# LANGUAGE LambdaCase #-}
module Main where

import Test.DocTest
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
test = doctest ["dot-reverse/src/Main.hs"]


reverseStmt :: Dot.Statement -> Dot.Statement
reverseStmt = \case
  Dot.EdgeStatement entities x
    -> Dot.EdgeStatement (reverse entities) x
  x -> x


-- |
-- >>> mapM_ print $ reverseStmts inputStmts
-- EdgeStatement [ENodeId DirectedEdge (NodeId (NameId "b") Nothing),ENodeId NoEdge (NodeId (NameId "a") Nothing)] []
-- EdgeStatement [ENodeId DirectedEdge (NodeId (NameId "c") Nothing),ENodeId NoEdge (NodeId (NameId "b") Nothing)] []
reverseStmts :: [Dot.Statement] -> [Dot.Statement]
reverseStmts = fmap reverseStmt

-- |
-- >> putStr $ renderDot $ simplifyGraph ["a1", "b2"] inputGraph
-- strict digraph {
--   b -> a;
--   c -> b;
-- }
reverseGraph :: Dot.Graph -> Dot.Graph
reverseGraph (Dot.Graph x y z stmts)
  = Dot.Graph x y z (reverseStmts stmts)

main :: IO ()
main = do
  inputString <- getContents
  case Dot.parseDot "stdin" inputString of
    Left err -> do
      error $ show err
    Right inputGraph -> do
      let outputGraph = reverseGraph inputGraph
      let outputString = Dot.renderDot outputGraph
      putStr outputString
