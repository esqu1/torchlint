{-# OPTIONS -Wincomplete-patterns #-}

-- CIS 552, University of Pennsylvania

module ParserCombinators where

import Parser (Parser)
import qualified Parser as P
import Control.Applicative
import Data.Char
import System.IO

type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: Parser a -> String -> Either ParseError a
parse parser str = case P.doParse parser str of
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  pure $ parse parser str

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P.filter p P.get

isNonLineBreakSpace :: Char -> Bool
isNonLineBreakSpace c = isSpace c && (c /= '\n')


-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isNonLineBreakSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)


-- | Parses one or more occurrences of @p@ separated by bindary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the Parsers lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest where
   comb x (op,y) = x `op` y
   rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p pop x = chainl1 p pop <|> pure x

  
-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close


-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
