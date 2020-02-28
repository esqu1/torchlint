module Utils where

import           Control.Monad
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Map              as Map
import           Data.Maybe
import           GraphParser
import           IR
import           LinterTypes
import qualified Parser                as P
import qualified ParserCombinators     as P
import           State                 (State)
import qualified State                 as S
import           System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)
import           Text.Printf
import           Text.Read

-------------------- general utils ----------------------

-- A helper method for the getConstants method.
getConstantsHelper :: [Statement] -> Map Variable (Either Int String)
getConstantsHelper [] = Map.empty
getConstantsHelper (x:xs) =
  case x of
    (Assign (Assignment (VT v t:_) f _ _))
     ->
      case f of
        (Constant (Just y)) -> Map.insert v y (getConstantsHelper xs)
        (Constant _)        -> getConstantsHelper xs
        _                   -> Map.empty
    _ -> Map.empty

-- getConstants gets a list of constants declared at the beginning of the SSA file.
getConstants :: Graph -> Map Variable (Either Int String)
getConstants (Graph args l returns) = getConstantsHelper l

-- A helper method for the getLineAnnots method.
getLineAnnotsHelper :: [Statement] -> Map Variable LineAnnot
getLineAnnotsHelper [] = Map.empty
getLineAnnotsHelper (x:xs) =
  case x of
    (Assign a@(Assignment (VT v t:_) f _ (Just annot))) ->
      Map.insert v annot (getLineAnnotsHelper xs)
    (If a@(Assignment vartypes f _ (Just annot)) (Block _ l1 _) (Block _ l2 _)) ->
      Map.union (getLineAnnotsHelper l1) $
      Map.union
        (getLineAnnotsHelper l2)
        (Data.List.foldr
           (\(VT v t) acc -> Map.insert v annot acc)
           (getLineAnnotsHelper xs)
           vartypes)
    (Loop a@(Assignment vartypes f _ (Just annot)) (Block _ l ret)) ->
      let recd = getLineAnnotsHelper xs
       in Map.union
            (getLineAnnotsHelper l)
            (Data.List.foldr
               (\(VT v t) acc -> Map.insert v annot acc)
               recd
               vartypes)
    _ -> getLineAnnotsHelper xs

-- getLineAnnots creates a map of all variables associated with their line annotations.
-- Every variable is guaranteed to only appear once due to the nature of SSA.
getLineAnnots :: Graph -> Map Variable LineAnnot
getLineAnnots (Graph args l _) = getLineAnnotsHelper l

-- Converts a string like "tensor.1" to "tensor".
getRealVarName :: Variable -> Variable
getRealVarName s =
  let s' = Data.List.Split.splitOn "." s
   in case s' of
        [] -> error "invalid variable name"
        (x:_) ->
          if isJust (readMaybe x :: Maybe Int)
            then "<anonymous tensor>"
            else x

-- Composes multiple state transformers into one.
composeStatesUnit :: [State s ()] -> State s ()
composeStatesUnit [] = return ()
composeStatesUnit (x:xs) = do
  x
  composeStatesUnit xs

-- Serializes a line annotation.
serializeAnnot :: Maybe LineAnnot -> String
serializeAnnot Nothing = ""
serializeAnnot (Just (LineAnnot str lineNum colNum)) =
  printf "%s:%d:%d" str lineNum colNum

-- Prints several lint messages to the terminal.
printMessages :: [LintMessage] -> IO ()
printMessages =
  mapM_
    (\x -> do
       putStr $ color Magenta "warning: "
       putStrLn x)
