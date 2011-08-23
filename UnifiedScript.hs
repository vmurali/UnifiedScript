import System.Console.GetOpt
import Text.Regex
import System
import System.IO
import System.Directory
import System.Cmd
import Control.Monad

data Options = Options
  { genBsv      :: Bool
  , genVerilog  :: Bool
  , genMulti    :: Bool
  , genExec     :: Bool
  , genRefined  :: Bool
  , refinedDir  :: String
  , refinedMods :: [String]
  , topFile     :: String
  , topModule   :: String
  , force       :: Bool
  }

defaultOptions = Options
  { genBsv      = False
  , genVerilog  = False
  , genMulti    = False
  , genExec     = False
  , genRefined  = False
  , refinedDir  = ""
  , refinedMods = []
  , topFile     = ""
  , topModule   = ""
  , force       = False
  }

splitColon str = splitRegex (mkRegex ":") str

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['b'] ["bsv"]
      (NoArg (\opts -> return opts {genBsv = True}))
      "Generate Bsv"
  , Option ['v'] ["verilog"]
      (NoArg (\opts -> return opts {genBsv = True, genVerilog = True}))
      "Generate Verilog"
  , Option ['m'] ["multi"]
      (NoArg (\opts -> return opts {genBsv = True, genVerilog = True, genMulti = True}))
      "Generate Multicycle"
  , Option ['e'] ["exec"]
      (NoArg (\opts -> return opts {genExec = True}))
      "Generate Executable"
  , Option ['r'] ["refined"]
      (ReqArg (\refDir opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, genRefined = True, refinedDir = refDir}) "")
      "Refined Files Directory"
  , Option ['g'] ["refinedParts"]
      (ReqArg (\refMods opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, genRefined = True, refinedMods = splitColon refMods}) "")
      "Refined Partitions"
  , Option ['t'] ["topmodule"]
      (ReqArg (\topmod opts -> return opts{topModule = topmod}) "")
      "Top-level Module"
  , Option ['f'] ["force"]
      (NoArg (\opts -> return opts {force = True}))
      "Has refined modules"
  , Option ['h'] ["help"]
      (NoArg (\_ -> do{prg <- getProgName; hPutStrLn stderr (usageInfo prg options); exitWith ExitSuccess}))
      "Show help"
  ]

parserOpts args =
  foldl (>>=) (return defaultOptions{topFile = head fileList}) optionList
  where
    (optionList, fileList, err) = getOpt RequireOrder options args

main = do
  args <- getArgs
  opts <- parserOpts args
  let removeSlash = subRegex (mkRegex "^.*\\/") (topFile opts) ""
  let name = subRegex (mkRegex ".spec$") removeSlash ""
  let specCmd inDir = "cd " ++ inDir ++ ";StructuralSpec " ++ (if force opts then "-f " else "") ++ "-o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi " ++ topFile opts
  let bsvCmd inDir outDir = "cd " ++ inDir ++ "/bsv; bsc -u -unsafe-always-ready -verilog -vdir " ++ outDir ++ " -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/" ++ outDir ++ ":${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 " ++ (if topModule opts /= "" then "-g " else "") ++ topModule opts ++ " " ++ name ++ ".bsv 2>&1 | ignoreBsc.pl"
  let runSpec inDir = do{putStrLn $ specCmd inDir; system $ specCmd inDir}
  let runBsv inDir outDir = do{putStrLn $ bsvCmd inDir outDir; system $ bsvCmd inDir outDir}
  let whenRet cond x = when cond (x >> return ())
  whenRet (genBsv opts) $ do
    system "mkdir -p bsv"
    runSpec "."
  whenRet (genVerilog opts) $ do
    system "mkdir -p bsv/bdir bsv/single"
    runBsv "." "single"
    system "ln -sf -t bsv/single ${STRUCTURALSPEC_HOME}/lib/single/*.v"
  whenRet (genMulti opts) $ do
    system "mkdir -p bsv/multi"
    system "cd bsv/single; Convert.sh"
    system "ln -sf -t bsv/multi ${STRUCTURALSPEC_HOME}/lib/multi/*.v"
  whenRet (genRefined opts) $ do
    system "mkdir -p buildRefined buildRefined/bsv/bdir buildRefined/bsv/multi"
    system $ "ln -sf -t buildRefined `pwd`/*.spec"
    system $ "ln -sf -t buildRefined `pwd`/" ++ refinedDir opts ++ "/*.spec"
    runSpec "buildRefined"
    runBsv "buildRefined" "multi"
    foldl (\x file -> x >> (system $ "cp buildRefined/bsv/multi/" ++ file ++ ".v bsv/multi/" ++ file ++ "_multi.v")) (return ExitSuccess) (refinedMods opts)
  whenRet (genExec opts) $ do
    let cmd inDir = "cd bsv/" ++ inDir ++ "; bsc -e " ++ (if topModule opts == "" then "mk" ++ name else topModule opts) ++ " *.v"
    putStrLn $ cmd "single"
    system $ cmd "single"
    whenRet (genMulti opts) $ do
      putStrLn $ cmd "multi"
      system $ cmd "multi"
