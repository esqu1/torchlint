module SizeLinter where

import           Data.Either
import           Data.List
import           Data.Map              as Map
import           Data.Maybe
import           IR
import           LinterTypes
import qualified Parser                as P
import qualified ParserCombinators     as P
import           State                 (State)
import qualified State                 as S
import           Text.Printf
import           Utils

-------------------- size linter utils ----------------------
copySize :: Variable -> Variable -> State SizeStore ()
copySize src dest = do
  SizeStore m msg <- S.get
  let sz = Map.lookup src m
   in case sz of
        (Just s) -> S.put (SizeStore (Map.insert dest s m) msg)
        Nothing  -> return ()

-- transferSize transfers size of a particular variable to another variable.
transferSize :: Variable -> Variable -> State SizeStore ()
transferSize var otherVar = do
  SizeStore m1 messages <- S.get
  S.put (SizeStore
            (Map.insert
              var
              (fromMaybe
                (error $
                  printf "could not find dimension of non-constant var %s"
                    (getRealVarName otherVar))
                (Map.lookup otherVar m1))
              m1)
            messages)

-- handles situations where we only want to update lint messages
msgWithAnnot :: Maybe LineAnnot -> (String -> String) -> State SizeStore ()
msgWithAnnot annot msg = do
  SizeStore m1 messages <- S.get
  let an = case annot of
             Just (LineAnnot a l c) -> printf "%s:%d:%d" a l c
             Nothing                -> ""
    in S.put
      (SizeStore
        m1
        (messages ++ [msg an]))

-- handles situations where the operands are a Tensor and a constant
combineTensorWithConstant :: 
  Variable 
  -> Variable 
  -> Map Variable (Either Int String) 
  -> [Variable] 
  -> [Int] 
  -> Map Variable LineAnnot 
  -> State SizeStore ()
combineTensorWithConstant name x m l j annots = do
  SizeStore m1 messages <- S.get
  case Map.lookup x m of
    Just v1 -> S.put (SizeStore (Map.insert name j m1) messages)
    Nothing ->
      let lineNums =
            intercalate "\n\t" $
            Data.List.map
              (\v1 ->
                serializeAnnot (Map.lookup v1 annots) ++
                (let v2 = getRealVarName v1
                  in if v2 == ""
                        then ""
                        else printf " (%s)" v2))
              (Data.List.filter (\s -> isJust (Map.lookup s m1)) l)
      in S.put
            (SizeStore
              m1
              (messages ++ [printf
                  "size not defined before usage for variable declared at %s  \n"
                  lineNums]))

---------------------- MAIN LINTER METHODS --------------------
-- | finds inconsistensies in the sizes of tensors involved in operations
detectSizeInconsistencies ::
     [Statement]
  -> Map Variable (Either Int String)
  -> Map Variable LineAnnot
  -> State SizeStore ()
detectSizeInconsistencies [] m _ = return ()
detectSizeInconsistencies (x : xs) m annots = do
  evalSize x m annots
  detectSizeInconsistencies xs m annots

-- | evaluates the sizes of all tensors and finds if there are inconsistencies
evalSize ::
     Statement
  -> Map Variable (Either Int String) -- constants
  -> Map Variable LineAnnot -- line annotation for variable
  -> State SizeStore ()
evalSize 
  (Assign (Assignment (VT name (Type Tensor _ _) : _) f l annot)) m annots =
  case f of
    NamedFunc funcName ->
      if funcName `elem` ["aten::zeros", "aten::ones", "aten::to"] then
        do  SizeStore m1 messages <- S.get
            case l of
              l1 : ls -> transferSize name l1
              [] ->
                msgWithAnnot
                  annot
                  (\an -> "no size argument for tensor " ++
                    show (getRealVarName name) ++ " defined at " ++ an)
        else do
          SizeStore m1 messages <- S.get
          case l of
            (x : y : ns) ->
              case (Map.lookup x m1, Map.lookup y m1) of
                (Nothing, Just j) ->
                  combineTensorWithConstant name x m l j annots
                (Just j, Nothing) ->
                  combineTensorWithConstant name y m l j annots
                (Just v1, Just v2) ->
                  if v1 == v2 then
                    S.put (SizeStore (Map.insert name v1 m1) messages)
                    else
                      msgWithAnnot
                        annot
                        (\an -> "variables " ++
                          show (getRealVarName x) ++
                          " and " ++
                          show (getRealVarName y) ++
                          " have incompatible sizes at " ++ an ++ "\n")
                (Nothing, Nothing) ->
                  msgWithAnnot
                    annot
                    (printf
                      ("sizes not defined before usage for" ++
                      "variables %s and %s before usage at %s\n")
                      (getRealVarName x)
                      (getRealVarName y))
            [x] -> transferSize name x
            _ -> return ()
    _ -> return ()
evalSize (Assign (Assignment (VT name t : _) f l annot)) m annots =
  case f of
    NamedFunc "prim::ListConstruct" -> do
      (SizeStore m1 messages) <- S.get
      S.put
        (SizeStore
           (Map.insert
              name
              (Data.List.map
                 (\x ->
                    fromLeft
                      0
                      (fromMaybe
                        (error "error extracting constants for list construction")
                        (Map.lookup x m)))
                 l)
              m1)
           messages)
    _ -> return ()
evalSize 
  (If (Assignment vts f l annot) (Block _ st1 rv1s) (Block _ st2 rv2s)) 
  m 
  annots = do
  detectSizeInconsistencies st1 m annots
  detectSizeInconsistencies st2 m annots
  (SizeStore m1 messages) <- S.get
  case zip vts $ zip rv1s rv2s of
    (r12 : r12s) ->
      composeStatesUnit $
      Data.List.map
        (\(VT name _, (r1, r2)) -> do
           (SizeStore m1 messages) <- S.get
           if Map.lookup r1 m1 /= Map.lookup r2 m1 -- inconsistent sizes
             then (let an =
                         case annot of
                           Just (LineAnnot a l _) -> printf "%s:%d" a l
                           Nothing                -> ""
                    in S.put
                         (SizeStore
                            (Map.insert
                              name
                                (fromMaybe
                                (error
                                  ("returned value from If in case 1 of " ++
                                  "mismatched sizes does not exist in map!"))
                                (Map.lookup r1 m1)) -- must propagate a value
                              m1)
                            (("inconsistent sizes for " ++
                              show (getRealVarName r1) ++
                              " between if cases at " ++ an) :
                             messages)))
             else transferSize name r1)
        (r12 : r12s)
    _ ->  msgWithAnnot 
          annot 
          ("inconsistent returned values between if cases at " ++)
evalSize 
  (Loop (Assignment vts f l annot) (Block blockVars st (r1 : rs))) 
  m 
  annots = do
  composeStatesUnit $
    zipWith
      copySize
      (Data.List.drop 2 l)
      (Data.List.drop 1 (Data.List.map (\(VT v t) -> v) blockVars))
  detectSizeInconsistencies st m annots
  (SizeStore m1 messages) <- S.get
  case Data.Maybe.mapMaybe
        (\ (n, val) ->
          case Map.lookup val m1 of
              Just v -> Just (n, v)
              Nothing -> Nothing)
        (zip vts rs) of
    [] -> return ()
    vals ->
      composeStatesUnit $
      Data.List.map
        (\(VT name _, x) -> do
           (SizeStore m1 messages) <- S.get
           S.put (SizeStore (Map.insert name x m1) messages))
        vals
