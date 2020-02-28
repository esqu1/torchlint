-- defines the types of the Graph data type, which we will use to represent
-- the Torch IR in Haskell
module IR where

import           Text.Printf

type Variable = String

type Nullable = Bool

type IsList = Bool

data BaseType
  = Tensor
  | Integer
  | Boolean
  | Float
  | Device
  | String
  | Other String
  deriving (Eq, Show)

data Type =
  Type BaseType Nullable IsList
  deriving (Eq, Show)

data VT =
  VT Variable Type
  deriving (Eq, Show)

data Func
  = Constant (Maybe (Either Int String)) -- for constant assignments
  | NamedFunc String
  deriving (Eq, Show) -- for all other functions

data LineAnnot =
  LineAnnot String Int Int
  deriving (Eq, Show)

data Block =
  Block [VT] [Statement] [Variable]
  deriving (Eq, Show)

data Assignment =
  Assignment [VT] Func [Variable] (Maybe LineAnnot)
  deriving (Eq, Show)

data Statement
  = Assign Assignment
  | If Assignment Block Block
  | Loop Assignment Block
  deriving (Eq, Show)

data Graph =
  Graph
    [VT] -- args
    [Statement] -- actual code
    [Variable] -- return variables
  deriving (Eq, Show)
