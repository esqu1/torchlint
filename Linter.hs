-- provides the core linting methods, checking if there are device,
-- dimensional, variable type, or size inconsistencies that would prevent
-- the model from being run
module Linter where

import           Data.Map              as Map
import           DeviceLinter
import           GraphParser
import           IR
import           LinterTypes
import qualified Parser                as P
import qualified ParserCombinators     as P
import           SizeLinter
import           State                 (State)
import qualified State                 as S
import           Utils

-- | runs the suite of detectXInconsistencies methods, returning a list of all the lint messages
detectInconsistencies :: Graph -> [LintMessage]
detectInconsistencies g@(Graph _ l _) =
  let deviceState =
        detectDeviceInconsistencies l (getConstants g) (getLineAnnots g)
   in let sizeState =
            detectSizeInconsistencies l (getConstants g) (getLineAnnots g)
       in let (_, DeviceStore m1 m2 deviceMessages) =
                S.runState deviceState (DeviceStore Map.empty Map.empty [])
           in let (_, SizeStore m1 sizeMessages) =
                    S.runState sizeState (SizeStore Map.empty [])
               in deviceMessages ++ sizeMessages

-------------------- debugging aids ----------------------
testGraphDevice :: Graph -> [LintMessage]
testGraphDevice graph@(Graph args l _) =
  let state =
        detectDeviceInconsistencies l (getConstants graph) (getLineAnnots graph)
   in let (_, DeviceStore _ _ messages) =
            S.runState state (DeviceStore Map.empty Map.empty [])
       in messages

testGraphSize :: Graph -> (Map Variable [Int], [LintMessage])
testGraphSize graph@(Graph args l _) =
  let state =
        detectSizeInconsistencies l (getConstants graph) (getLineAnnots graph)
   in let (_, SizeStore m1 messages) = S.runState state (SizeStore Map.empty [])
       in (m1, messages)

testGraph :: Graph -> [LintMessage]
testGraph g =
  let (_, sizeMessages) = testGraphSize g
   in testGraphDevice g ++ sizeMessages
