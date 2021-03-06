import System.Console.GetOpt
import Text.Regex
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Cmd
import Control.Monad
import Data.List
import Data.Maybe

data Options = Options
  { genBsv      :: Bool
  , genVerilog  :: Bool
  , genMulti    :: Bool
  , multiMods   :: [String]
  , genExec     :: Bool
  , genRefined  :: Bool
  , refinedDir  :: String
  , refinedMods :: [String]
  , topFile     :: String
  , topModule   :: String
  , hasC        :: Bool
  , additional  :: [String]
  , force       :: Bool
  } deriving Show

defaultOptions = Options
  { genBsv      = False
  , genVerilog  = False
  , genMulti    = False
  , multiMods   = []
  , genExec     = False
  , genRefined  = False
  , refinedDir  = ""
  , refinedMods = []
  , topFile     = ""
  , topModule   = ""
  , hasC        = False
  , additional  = []
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
      (ReqArg (\mulMods opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, multiMods = (multiMods opts) ++ splitColon mulMods}) "")
      "Refined Partitions"
  , Option ['e'] ["exec"]
      (NoArg (\opts -> return opts {genBsv = True, genVerilog = True, genExec = True}))
      "Generate Executable"
  , Option ['r'] ["refined"]
      (ReqArg (\refDir opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, genRefined = True, refinedDir = refDir}) "")
      "Refined Files Directory"
  , Option ['g'] ["refinedParts"]
      (ReqArg (\refMods opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, multiMods = nub (multiMods opts ++ splitColon refMods), genRefined = True, refinedMods = (refinedMods opts) ++ splitColon refMods}) "")
      "Refined Partitions"
  , Option ['t'] ["topmodule"]
      (ReqArg (\topmod opts -> return opts{topModule = topmod, multiMods = nub $ topmod:(multiMods opts)}) "")
      "Top-level Module"
  , Option ['c'] ["c"]
      (NoArg (\opts -> return opts {hasC = True}))
      "Has C files"
  , Option ['a'] ["additional"]
      (ReqArg (\adds opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, genRefined = True, additional = (additional opts) ++ splitColon adds}) "")
      "Additional modules in refinement"
  , Option ['f'] ["force"]
      (NoArg (\opts -> return opts {force = True}))
      "Has refined modules"
  , Option ['h'] ["help"]
      (NoArg (\_ -> do{prg <- getProgName; hPutStrLn stderr (usageInfo prg options); exitWith ExitSuccess}))
      "Show help"
  ]

fileName opts = subRegex (mkRegex ".spec$") (removeSlash opts) ""
 where
  removeSlash opts = subRegex (mkRegex "^.*\\/") (topFile opts) ""

parserOpts args = do
  let (optionList, fileList, err) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions{topFile = head fileList}) optionList
  let retOpts = if topModule opts == ""
                  then opts{topModule = "mk" ++ fileName opts, multiMods = nub $ ("mk" ++ fileName opts):(multiMods opts)}
                  else opts
  putStrLn $ show retOpts
  return retOpts

runCmd cmd = do
  putStrLn $ "\nEXECUTING: " ++ cmd ++ "\n"
  exitCode <- system cmd
  case exitCode of
    ExitSuccess -> return ()
    _           -> exitWith exitCode

main = do
  args <- getArgs
  opts <- parserOpts args
  let getBsv inDir = "bash -c \"cd " ++ inDir ++ ";StructuralSpec " ++ (if force opts then "-f " else "") ++ "-o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi " ++ topFile opts ++ "\""
  let getV inDir outDir = "bash -o pipefail -c \"cd " ++ inDir ++ "/bsv; bsc -show-range-conflict -u -unsafe-always-ready -verilog -vdir " ++ outDir ++ " -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/" ++ outDir ++ ":${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 " ++ fileName opts ++ ".bsv +RTS -K4G -RTS 2>&1 | ignoreBsc.pl\""
  let cFiles = if hasC opts then " ../../*.c ../bdir/*.ba" else ""
  let getExec inDir name = "bash -c \"cd bsv/" ++ inDir ++ "; bsc -verilog -e " ++ topModule opts ++ name ++ " *.v" ++ cFiles ++ "\""
  when (genBsv opts) $ do
    runCmd "mkdir -p bsv"
    runCmd $ getBsv "."
  when (genVerilog opts) $ do
    runCmd "mkdir -p bsv/bdir bsv/single"
    runCmd $ getV "." "single"
    runCmd "ln -sf -t bsv/single ${STRUCTURALSPEC_HOME}/lib/single/*.v"
  when (genMulti opts) $ do
    allFiles <- getDirectoryContents "bsv/single"
    let files = [x|x <- allFiles, isJust $ matchRegex (mkRegex "^[a-z].*\\.v$") x]--, x /= "Base.v", x /= "Rand.v", x /= "RegFile.v"]
    runCmd "mkdir -p bsv/multi"
    foldl (\x file -> x >> (runCmd $ "Multicycle -o bsv/multi -m " ++ (intercalate ":" $ multiMods opts) ++ " bsv/single/" ++ file)) (return ()) files
    runCmd "ln -sf -t bsv/multi ${STRUCTURALSPEC_HOME}/lib/multi/*.v"
  when (genRefined opts) $ do
    runCmd "mkdir -p buildRefined buildRefined/bsv/bdir buildRefined/bsv/multi"
    runCmd $ "ln -sf -t buildRefined `pwd`/*.spec"
    runCmd $ "ln -sf -t buildRefined `pwd`/" ++ refinedDir opts ++ "/*.spec"
    runCmd $ getBsv "buildRefined"
    runCmd $ getV "buildRefined" "multi"
    foldl (\x file -> x >> (runCmd $ "cp -f buildRefined/bsv/multi/" ++ file ++ ".v bsv/multi/" ++ file ++ "_FIFO_ALL_EXPOSED.v")) (return ()) (refinedMods opts)
    foldl (\x file -> x >> (runCmd $ "cp -f buildRefined/bsv/multi/" ++ file ++ ".v bsv/multi/" ++ file ++ ".v")) (return ()) (additional opts)
  when (genExec opts) $ do
    runCmd $ getExec "single" ""
    when (genMulti opts) (runCmd $ getExec "multi" "_FIFO_OUTER_NOT_EXPOSED")
