module LinterTypes where

import           Data.Map
import           IR

---------------------- type declarations -----------------------
type LintMessage = String

data DeviceStore =
  DeviceStore
    (Map Variable String) -- variables representing devices
    (Map Variable String) -- variables and which device they reside
    [LintMessage] -- lint messages

data SizeStore =
  SizeStore
    (Map Variable [Int]) -- sizes of variables (if applicable)
    [LintMessage] -- lint messages
