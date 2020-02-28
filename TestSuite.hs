module Tests where
  import qualified System.Directory as Dir
  import           Test.HUnit
  import qualified GraphParser as GP
  import           Data.Map (Map)
  import qualified Data.Map as Map
  import qualified Linter as L
  import           Parser              (Parser)
  import qualified Parser              as P
  import qualified ParserCombinators   as P
  import           IR
  import           Data.List as List
  import           LinterTypes

  main :: IO ()
  main = do
    putStrLn "Running GraphParser tests"
    _ <- graphParseTest
    putStrLn "Running Linter tests"
    _ <- linterTest
    return ()

  readAllTestFiles :: IO [(FilePath, String)]
  readAllTestFiles = do
    files <- Dir.getDirectoryContents graphsFolder
    mapM (\x -> readFile x >>= (\output -> return (x, output)))
      ((map ((graphsFolder ++ "/") ++) . List.delete ".." . List.delete ".")
      files)

------------------------- Test Cases for GraphParser --------------------------
  graphsFolder :: String
  graphsFolder = "Tests/Python_Graphs"

  graphsMap :: Map String Graph
  graphsMap = Map.fromList [
    (graphsFolder ++ "/bad_device_1.txt", Graph [VT "self" (Type (Other "ClassType<BadDevice1Test>") False False)] [Assign (Assignment [(VT "27" (Type Integer False False))] (Constant (Just (Left 1))) [] Nothing),
                                                                                                   Assign (Assignment [(VT "22" (Type Boolean False False))] (Constant (Just (Left 0))) [] Nothing),
                                                                                                   Assign (Assignment [(VT "7" (Type (Other "None") False False))] (Constant Nothing) [] Nothing),
                                                                                                   Assign (Assignment [(VT "1" (Type String False False))] (Constant (Just (Right "cuda:0"))) [] (Just (LineAnnot "bad_device_1.py" 5 30))),
                                                                                                   Assign (Assignment [(VT "3" (Type Integer False False))] (Constant (Just (Left 3))) [] (Just (LineAnnot "bad_device_1.py" 6 30))),
                                                                                                   Assign (Assignment [(VT "4" (Type Integer False False))] (Constant (Just (Left 4))) [] (Just (LineAnnot "bad_device_1.py" 6 32))),
                                                                                                   Assign (Assignment [(VT "device.1" (Type Device False False))] (NamedFunc "aten::device") ["1"] (Just (LineAnnot "bad_device_1.py" 5 17))),
                                                                                                   Assign (Assignment [(VT "6" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["3","4"] Nothing),
                                                                                                   Assign (Assignment [(VT "11" (Type Tensor False False))] (NamedFunc "aten::zeros") ["6","7","7","7","7"] (Just (LineAnnot "bad_device_1.py" 6 18))),
                                                                                                   Assign (Assignment [(VT "tensor1.1" (Type Tensor False False))] (NamedFunc "aten::cpu") ["11"] (Just (LineAnnot "bad_device_1.py" 6 18))),
                                                                                                   Assign (Assignment [(VT "14" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["3","4"] Nothing),
                                                                                                   Assign (Assignment [(VT "19" (Type Tensor False False))] (NamedFunc "aten::zeros") ["14","7","7","7","7"] (Just (LineAnnot "bad_device_1.py" 7 18))),
                                                                                                   Assign (Assignment [(VT "tensor2.1" (Type Tensor False False))] (NamedFunc "aten::to") ["19","device.1","7","22","22"] (Just (LineAnnot "bad_device_1.py" 7 18))),
                                                                                                   Assign (Assignment [(VT "28" (Type Tensor False False))] (NamedFunc "aten::add") ["tensor1.1","tensor2.1","27"] (Just (LineAnnot "bad_device_1.py" 8 15)))
                                                                                                  ] ["28"]),
    (graphsFolder ++ "/basic_test.txt", Graph [VT "self" (Type (Other "ClassType<BasicTest>") False False)] [Assign (Assignment [(VT "5" (Type (Other "None") False False))] (Constant Nothing) [] Nothing),
                                                                                            Assign (Assignment [(VT "1" (Type Integer False False))] (Constant (Just (Left 3))) [] (Just (LineAnnot "basic_test.py" 6 19))),
                                                                                            Assign (Assignment [(VT "2" (Type Integer False False))] (Constant (Just (Left 1))) [] (Just (LineAnnot "basic_test.py" 6 21))),
                                                                                            Assign (Assignment [(VT "4" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["1","2"] Nothing),
                                                                                            Assign (Assignment [(VT "9" (Type Tensor False False))] (NamedFunc "aten::ones") ["4", "5", "5", "5", "5"] (Just (LineAnnot "basic_test.py" 6 8))),
                                                                                            Assign (Assignment [(VT "x.1" (Type Tensor False False))] (NamedFunc "aten::cuda") ["9"] (Just (LineAnnot "basic_test.py" 6 8)))
                                                                                            ] ["x.1"]),
    (graphsFolder ++ "/for.txt", Graph [VT "self" (Type (Other "ClassType<ForTest>") False False)] [Assign (Assignment [(VT "13" (Type Boolean False False))] (Constant (Just (Left 1))) [] (Just (LineAnnot "for.py" 7 8))),
                                                                                   Assign (Assignment [(VT "5" (Type (Other "None") False False))] (Constant Nothing) [] Nothing),
                                                                                   Assign (Assignment [(VT "1" (Type Integer False False))] (Constant (Just (Left 3))) [] (Just (LineAnnot "for.py" 6 24))),
                                                                                   Assign (Assignment [(VT "2" (Type Integer False False))] (Constant (Just (Left 1))) [] (Just (LineAnnot "for.py" 6 26))),
                                                                                   Assign (Assignment [(VT "10" (Type Integer False False))] (Constant (Just (Left 10))) [] (Just (LineAnnot "for.py" 7 23))),
                                                                                   Assign (Assignment [(VT "4" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["1","2"] Nothing),
                                                                                   Assign (Assignment [(VT "x.1" (Type Tensor False False))] (NamedFunc "aten::zeros") ["4","5","5","5","5"] (Just (LineAnnot "for.py" 6 12))),
                                                                                   Loop (Assignment [(VT "x" (Type Tensor False False))] (NamedFunc "prim::Loop") ["10","13","x.1"] (Just (LineAnnot "for.py" 7 8))) 
                                                                                      (Block 
                                                                                        [VT "t" (Type Integer False False),VT "x.6" (Type Tensor False False)] 
                                                                                        [Assign (Assignment [(VT "x.3" (Type Tensor False False))] (NamedFunc "aten::add") ["x.6","2","2"] (Just (LineAnnot "for.py" 8 16)))] 
                                                                                        ["13","x.3"]
                                                                                      )
                                                                                  ] ["x"]),

    (graphsFolder ++ "/if.txt", Graph [VT "self" (Type (Other "ClassType<IfTest>") False False)] [Assign (Assignment [(VT "5" (Type (Other "None") False False))] (Constant Nothing) [] Nothing),
                                                                                 Assign (Assignment [(VT "1" (Type Integer False False))] (Constant (Just (Left 3))) [] (Just (LineAnnot "if.py" 5 19))),
                                                                                 Assign (Assignment [(VT "2" (Type Integer False False))] (Constant (Just (Left 1))) [] (Just (LineAnnot "if.py" 5 21))),
                                                                                 Assign (Assignment [(VT "26" (Type Integer False False))] (Constant (Just (Left 0))) [] (Just (LineAnnot "if.py" 8 12))),
                                                                                 Assign (Assignment [(VT "4" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["1","2"] Nothing),
                                                                                 Assign (Assignment [(VT "x.1" (Type Tensor False False))] (NamedFunc "aten::ones") ["4","5","5","5","5"] (Just (LineAnnot "if.py" 5 8))),
                                                                                 Assign (Assignment [(VT "11" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["1","2"] Nothing),
                                                                                 Assign (Assignment [(VT "16" (Type Tensor False False))] (NamedFunc "aten::ones") ["11","5","5","5","5"] (Just (LineAnnot "if.py" 6 10))),
                                                                                 Assign (Assignment [(VT "y.1" (Type Tensor False False))] (NamedFunc "aten::mul") ["16","1"] (Just (LineAnnot "<string>" 3 9))),
                                                                                 Assign (Assignment [(VT "19" (Type Integer True False))] (NamedFunc "prim::ListConstruct") ["1","2"] Nothing),
                                                                                 Assign (Assignment [(VT "z.1" (Type Tensor False False))] (NamedFunc "aten::zeros") ["19","5","5","5","5"] (Just (LineAnnot "if.py" 7 8))),
                                                                                 Assign (Assignment [(VT "27" (Type Tensor False False))] (NamedFunc "aten::gt") ["x.1","26"] (Just (LineAnnot "if.py" 8 8))),
                                                                                 Assign (Assignment [(VT "28" (Type Tensor False False))] (NamedFunc "aten::all") ["27"] (Just (LineAnnot "if.py" 8 8))),
                                                                                 Assign (Assignment [(VT "29" (Type Boolean False False))] (NamedFunc "aten::Bool") ["28"] (Just (LineAnnot "if.py" 8 8))),
                                                                                 If (Assignment [(VT "k" (Type Tensor False False))] (NamedFunc "prim::If") ["29"] (Just (LineAnnot "if.py" 8 4)))
                                                                                    (Block
                                                                                      []
                                                                                      [Assign (Assignment [(VT "k.1" (Type Tensor False False))] (NamedFunc "aten::add") ["x.1","y.1","2"] (Just (LineAnnot "if.py" 9 10)))]
                                                                                      ["k.1"]
                                                                                    )
                                                                                    (Block
                                                                                      []
                                                                                      [Assign (Assignment [(VT "k.2" (Type Tensor False False))] (NamedFunc "aten::add") ["x.1","z.1","2"] (Just (LineAnnot "if.py" 11 10)))]
                                                                                      ["k.2"]
                                                                                    )
                                                                                  ] ["k"])]

  graphParseTest :: IO Counts
  graphParseTest = do
    nc <- readAllTestFiles
    x <- return $ TestList $ (\(fname, str) ->
          case P.parse GP.graphP str of
            (Left _) -> tGraphParseTest (Graph [] [] []) fname
            (Right g) -> tGraphParseTest g fname) <$> nc
    runTestTT x

  tGraphParseTest :: Graph -> String -> Test
  tGraphParseTest graph fname = "parse graph " ++ fname ~:
    case graphsMap Map.!? fname of
      Nothing -> return () -- this particular test wasn't selected
      Just g -> if g == graph then return () else assertFailure msg where
        msg = "Results differed! produced:\n\n" 
              ++ show graph ++ "\n\nbut expected:\n\n" ++ show g

---------------------------- Test Cases for Linter ----------------------------
  linterMap :: Map String [LintMessage]
  linterMap = Map.fromList [
      (graphsFolder ++ "/bad_device_1.txt", ["Device mismatch at bad_device_1.py:8:15 with the following variables:\n\tbad_device_1.py:6:18 (tensor1)\n\tbad_device_1.py:7:18 (tensor2)"]),
      (graphsFolder ++ "/bad_device_2.txt", ["Device mismatch at bad_device_2.py:8:11 with the following variables:\n\tbad_device_2.py:6:8 (x)\n\tbad_device_2.py:7:8 (y)"]),
      (graphsFolder ++ "/bad_if_1.txt", ["inconsistent sizes for \"k\" between if cases at bad_if_1.py:9","variables \"x\" and \"y\" have incompatible sizes at bad_if_1.py:10:10\n"]),
      (graphsFolder ++ "/bad_if_2.txt", ["inconsistent sizes for \"k\" between if cases at bad_if_2.py:9","variables \"x\" and \"z\" have incompatible sizes at bad_if_2.py:12:10\n"]),
      (graphsFolder ++ "/bad_if_device_1.txt", ["Inconsistent device assignment in if block at bad_if_device_1.py:9:8"]),
      (graphsFolder ++ "/bad_if_device_2.txt", ["Device mismatch at bad_if_device_2.py:12:18 with the following variables:\n\tbad_if_device_2.py:8:8 (tensor3)\n\tbad_if_device_2.py:12:28 (<anonymous tensor>)", "variables \"tensor3\" and \"<anonymous tensor>\" have incompatible sizes at bad_if_device_2.py:12:18\n"]),
      (graphsFolder ++ "/bad_size.txt", ["variables \"tensor1\" and \"tensor2\" have incompatible sizes at bad_size.py:7:15\n"]),
      (graphsFolder ++ "/bad_while_device.txt", ["Device mismatch at bad_while_device.py:11:18 with the following variables:\n\tbad_while_device.py:9:8 (tensor2)\n\tbad_while_device.py:11:28 (<anonymous tensor>)","variables \"tensor2\" and \"<anonymous tensor>\" have incompatible sizes at bad_while_device.py:11:18\n"]),
      (graphsFolder ++ "/basic_test.txt", []),
      (graphsFolder ++ "/for.txt", []),
      (graphsFolder ++ "/good_device_1.txt", []),
      (graphsFolder ++ "/good_device_2.txt", []),
      (graphsFolder ++ "/if_with_for.txt", ["inconsistent sizes for \"rv\" between if cases at if_with_for.py:8","variables \"rv\" and \"<anonymous tensor>\" have incompatible sizes at if_with_for.py:9:21\n"]),
      (graphsFolder ++ "/if.txt", []),
      (graphsFolder ++ "/bad_device_mult.txt", ["Device mismatch at bad_device_mult.py:13:15 with the following variables:\n\tbad_device_mult.py:10:8 (tensor3)\n\tbad_device_mult.py:10:8 (tensor4)"])
      ]

  linterTest :: IO Counts
  linterTest = do
    nc <- readAllTestFiles
    x <- return $ TestList $ (\(fname, str) ->
          case P.parse GP.graphP str of
            (Left _) -> tLinterTest (Graph [] [] []) fname
            (Right g) -> tLinterTest g fname) <$> nc
    runTestTT x

  tLinterTest :: Graph -> String -> Test
  tLinterTest graph fname = "lint graph " ++ fname ~:
    case linterMap Map.!? fname of
      Nothing -> assertFailure "fname not found"
      Just s -> if s == L.detectInconsistencies graph then return () 
        else assertFailure msg where
          msg = "Results differ: real was " 
            ++ show s ++ " but produced " 
            ++ show (L.detectInconsistencies graph)
