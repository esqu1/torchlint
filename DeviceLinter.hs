module DeviceLinter where

import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Map              as Map
import           Data.Maybe
import           GraphParser
import           IR
import           LinterTypes
import qualified Parser                as P
import qualified ParserCombinators     as P
import           State                 (State)
import qualified State                 as S
import           Text.Printf
import           Text.Read
import           Utils

-------------------- device linter utils ----------------------
-- copyDevice takes two variables and copies the device associated
-- with the first variable to the second.
copyDevice :: Variable -> Variable -> State DeviceStore ()
copyDevice src dest = do
  DeviceStore m1 m2 messages <- S.get
  let deviceName = Map.lookup src m2
   in case deviceName of
        (Just s) -> S.put (DeviceStore m1 (Map.insert dest s m2) messages)
        Nothing  -> return ()

-- moveDevice changes the device of a particular variable.
moveDevice :: Variable -> String -> State DeviceStore ()
moveDevice var newDevice = do
  DeviceStore m1 m2 messages <- S.get
  S.put (DeviceStore m1 (Map.insert var newDevice m2) messages)

-- addDevicesOfReturnVars adds all of the returned variables from
-- an if or while loop to the current device store.
addDevicesOfReturnVars :: [VT] -> [Variable] -> State DeviceStore ()
addDevicesOfReturnVars vartypes l = do
  DeviceStore m1 m2 messages <- S.get
  let keyvals =
        Data.List.map (\(VT v' t', b) -> (v', fromJust b)) $
        Data.List.filter (\(a, b) -> isJust b) $
        zip vartypes (Data.List.map (`Map.lookup` m2) l)
   in S.put (DeviceStore m1 (Map.union (fromList keyvals) m2) messages)

-- lintTensor adds lint messages for assignments of tensors.
lintTensor ::
     Variable -> Assignment -> Map Variable LineAnnot -> State DeviceStore ()
lintTensor el (Assignment (VT v t:_) f l annot) annots =
  let (NamedFunc funcName) = f
   in case funcName
        -- either this is a Tensor construction (from aten::to), a new tensor
        -- from another one (via cpu() or cuda()) or combination of multiple
        -- tensors
            of
        "aten::cpu" -- copy the type over from the reference variable
         -> moveDevice v "cpu"
        "aten::cuda" -- copy the type over from the reference variable
         -> moveDevice v "cuda"
        "aten::to" -- same thing
         -> do
          DeviceStore m1 m2 messages <- S.get
          case l of
            _:newDevice:_ ->
              case Map.lookup newDevice m1 of
                Just x -> moveDevice v x
                _      -> error "Device does not exist in aten::to"
            _ -> error "Syntax error at aten::to"
        _ -- look at all the args devices, check to see if they're all the same
         -> do
          DeviceStore m1 m2 messages <- S.get
          let lookups -- lookup all the args to see which devices they reside on
               = Data.Maybe.mapMaybe (`Map.lookup` m2) l
           in case nub lookups of
                [] -> S.put (DeviceStore m1 (Map.insert v "cpu" m2) messages)
                [device] ->
                  S.put (DeviceStore m1 (Map.insert v device m2) messages)
                _ ->
                  let lineNums =
                        intercalate "\n\t" $
                        Data.List.map
                          (\x ->
                             serializeAnnot (Map.lookup x annots) ++
                             printf " (%s)" (getRealVarName x))
                          (Data.List.filter (\s -> isJust (Map.lookup s m2)) l)
                   in S.put
                        (DeviceStore
                           m1
                           m2
                           (messages ++
                            [ printf
                                ("Device mismatch at %s with " ++
                                 "the following variables:\n\t%s")
                                (serializeAnnot annot)
                                lineNums
                            ]))

-- finds inconsistencies in the devices of tensors involved in operations
detectDeviceInconsistencies ::
     [Statement]
  -> Map Variable (Either Int String)
  -> Map Variable LineAnnot
  -> State DeviceStore ()
detectDeviceInconsistencies [] m _ = return ()
detectDeviceInconsistencies (x:xs) m annots = do
  evalDevice x m annots
  detectDeviceInconsistencies xs m annots

-- evalDevice evaluates the device each tensor is at and finds any resulting
-- operation inconsistencies
evalDevice ::
     Statement
  -> Map Variable (Either Int String)
  -> Map Variable LineAnnot
  -> State DeviceStore ()
evalDevice (Assign asgn@(Assignment (VT v t:_) f l annot)) m annots =
  case l of
    [] ->
      case f
        -- if this is a constant expression assignment, ignore
            of
        Constant _ -> return ()
        _          -> error "Assignments need arguments"
    (el:els) ->
      case t
        -- for device variables, store what constant they're associated with
            of
        (Type Device _ _) ->
          let deviceName = Map.lookup el m
           in case deviceName of
                Just (Right name) -> do
                  (DeviceStore m1 m2 messages) <- S.get
                  S.put (DeviceStore (Map.insert v name m1) m2 messages)
                _ -> error "Device name not found in constants"
        (Type Tensor _ _) -> lintTensor el asgn annots
        _ -> return ()
evalDevice (If (Assignment vartypes f l annot) (Block _ st1 return1) (Block _ st2 return2)) m annots = do
  detectDeviceInconsistencies st1 m annots
  detectDeviceInconsistencies st2 m annots
  DeviceStore m1 m2 messages <- S.get
  if Data.List.map (`Map.lookup` m2) return1 /=
     Data.List.map (`Map.lookup` m2) return2
    then S.put
           (DeviceStore
              m1
              m2
              (messages ++
               [ printf
                   "Inconsistent device assignment in if block at %s"
                   (serializeAnnot annot)
               ]))
    else addDevicesOfReturnVars vartypes return1
evalDevice (Loop (Assignment vartypes f l _) (Block blockVars st return)) m annots = do
  composeStatesUnit $
    zipWith
      copyDevice
      (Data.List.drop 2 l)
      (Data.List.drop 1 (Data.List.map (\(VT v t) -> v) blockVars)) -- copy type into variable
  detectDeviceInconsistencies st m annots
  addDevicesOfReturnVars vartypes (Data.List.drop 1 return)
