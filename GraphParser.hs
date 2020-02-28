{-# OPTIONS -Wincomplete-patterns #-}

-- defines parser methods needed to convert
-- the string IR provided by Python into the
-- Graph data structure
module GraphParser where

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           IR
import           Parser              (Parser)
import qualified Parser              as P
import qualified ParserCombinators   as P
import           System.IO

-- Parses a variable name.
isVarText :: Char -> Bool
isVarText c = isAlpha c || isDigit c || (c == '.')

-- Function to check whether a string is a valid file name.
isFilenameTest :: Char -> Bool
isFilenameTest c = isVarText c || c `elem` "-_()/<>"

-- Parses some content and then whitespace (without the newline character).
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- Parses some content and then whitespace (including the newline character).
wsnewLineP :: Parser a -> Parser a
wsnewLineP p = p <* many (P.satisfy isSpace)

-- Parses a string, and then returns a value.
constP :: String -> a -> Parser a
constP s v = v <$ P.string s

-- Parses a boolean.
boolP :: Parser a -> Parser Bool
boolP p = (True <$ p) <|> pure False

-- Transforms a parser into a Maybe parser.
maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (Just <$> p) <|> pure Nothing

-- Parsers a variable name.
varP :: Parser Variable
varP = wsP $ P.char '%' *> some (P.satisfy isVarText)

-- Parsers content surrounded by parentheses.
parenP :: Parser a -> Parser a
parenP p = P.between (P.char '(') p (P.char ')')

-- Parses a typename, the base type.
baseTypeP :: Parser BaseType
baseTypeP =
  P.choice
    [ wsP $ constP "int" Integer
    , wsP $ constP "float" Float
    , wsP $ constP "bool" Boolean
    , wsP $ constP "Tensor" Tensor
    , wsP $ constP "Device" Device
    , wsP $ constP "str" String
    , wsP $ Other <$> many (P.satisfy (\c -> not (P.isNonLineBreakSpace c) && not (c `elem` ",():")))
    ]

-- Parses a typename with information about nullability and array-like structure.
typeP :: Parser Type
typeP =
  Type <$> baseTypeP <*> wsP (boolP (P.string "[]")) <*>
  wsP (boolP (P.char '?'))

-- Parses a variable-type pair.
vtP :: Parser VT
vtP = VT <$> varP <* wsP (P.char ':') <*> typeP

-- Parses a function name.
funcP :: Parser Func
funcP =
  P.choice
    [ wsP $
      Constant <$>
      (P.string "prim::Constant" *>
       ((Just . Left <$> P.between (P.string "[value=") P.int (P.char ']')) <|>
        Just . Right <$>
        P.between
          (P.string "[value=\"")
          (many (P.satisfy (\c -> c /= '(' && c /= '"')))
          (P.string "\"]") <|>
        pure Nothing))
    , wsP $ NamedFunc <$> many (P.satisfy (/= '('))
    ]

-- Parses a line annotation comment.
lineAnnotP :: Parser LineAnnot
lineAnnotP =
  LineAnnot <$> some (P.satisfy isFilenameTest) <* P.char ':' <*> P.int <*
  P.char ':' <*>
  P.int

-- Parses an assignment on a single line.
assignmentP :: Parser Assignment
assignmentP =
  Assignment <$> P.sepBy vtP (P.string ", ") <* wsP (P.char '=') <*> funcP <*>
  wsP (parenP (P.sepBy varP (P.string ", "))) <*>
  maybeP (wsP (P.char '#') *> lineAnnotP) <*
  many (P.satisfy isSpace)

-- Parses a block used in if or while blocks.
blockP :: Parser Block
blockP =
  Block <$>
  (P.string "block" *> P.int *> P.char '(' *> P.sepBy vtP (P.string ", ")) <*
  wsnewLineP (P.string "):") <*>
  many statementP <*>
  (wsP (P.string "->") *> wsP (parenP (P.sepBy varP (P.string ", "))))

-- Function to check whether the statement matches the function name parsed.
assignmentMatch :: Statement -> Bool
assignmentMatch (If (Assignment _ (NamedFunc s) _ _) _ _) = s == "prim::If"
assignmentMatch (Loop (Assignment _ (NamedFunc s) _ _) _) = s == "prim::Loop"
assignmentMatch (Assign (Assignment _ (NamedFunc s) _ _)) =
  s /= "prim::If" && s /= "prim::Loop"
assignmentMatch (Assign (Assignment _ f _ _)) = True
assignmentMatch s = False

-- Parses an if block.
ifP :: Parser Statement
ifP =
  P.filter
    assignmentMatch
    (If <$> wsnewLineP assignmentP <*> wsnewLineP blockP <*> wsnewLineP blockP)

-- Parses a while loop.
loopP :: Parser Statement
loopP =
  P.filter
    assignmentMatch
    (Loop <$> wsnewLineP assignmentP <*> wsnewLineP blockP)

-- Parses a if, loop, or an assignment.
statementP :: Parser Statement
statementP = ifP <|> loopP <|> P.filter assignmentMatch (Assign <$> assignmentP)

-- Parses the beginning of the computational graph.
graphHeaderP :: Parser [VT]
graphHeaderP =
  P.between
    (P.string "graph(")
    (P.sepBy vtP (wsP (P.string ",\n")))
    (wsnewLineP (P.string "):"))

-- Parses the entire graph.
graphP :: Parser Graph
graphP =
  Graph <$> graphHeaderP <*> many statementP <*>
  (P.string "return " *> parenP (P.sepBy varP (P.string ", ")))

-- Reads a text file containing a graph and parses the text into a Graph IR.
graphRead :: FilePath -> IO Graph
graphRead name = do
  str <- readFile name
  let t = P.parse graphP str in
    case t of 
      (Left _) -> return (Graph [] [] [])
      (Right g) -> return g