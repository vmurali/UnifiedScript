import System.Console.GetOpt
import Text.Regex
import System
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
      (OptArg (\mulMods opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, multiMods = (multiMods opts) ++ splitColon (fromMaybe [] mulMods)}) "")
      "Refined Partitions"
  , Option ['e'] ["exec"]
      (NoArg (\opts -> return opts {genBsv = True, genVerilog = True, genExec = True}))
      "Generate Executable"
  , Option ['r'] ["refined"]
      (ReqArg (\refDir opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, genRefined = True, refinedDir = refDir}) "")
      "Refined Files Directory"
  , Option ['g'] ["refinedParts"]
      (ReqArg (\refMods opts -> return opts{genBsv = True, genVerilog = True, genMulti = True, multiMods = nub (multiMods opts ++ splitColon refMods), genRefined = True, refinedMods = splitColon refMods}) "")
      "Refined Partitions"
  , Option ['t'] ["topmodule"]
      (ReqArg (\topmod opts -> return opts{topModule = topmod, multiMods = nub $ topmod:(multiMods opts)}) "")
      "Top-level Module"
  , Option ['f'] ["force"]
      (NoArg (\opts -> return opts {force = True}))
      "Has refined modules"
  , Option ['h'] ["help"]
      (NoArg (\_ -> do{prg <- getProgName; hPutStrLn stderr (usageInfo prg options); exitWith ExitSuccess}))
      "Show help"
  ]

removeSlash opts = subRegex (mkRegex "^.*\\/") (topFile opts) ""
name opts = subRegex (mkRegex ".spec$") (removeSlash opts) ""

parserOpts args = do
  let (optionList, fileList, err) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions{topFile = head fileList}) optionList
  let retOpts = if topModule opts == ""
                  then opts{topModule = "mk" ++ name opts, multiMods = nub $ ("mk" ++ name opts):(multiMods opts)}
                  else opts
  putStrLn $ show retOpts
  return retOpts

main = do
  args <- getArgs
  opts <- parserOpts args
  let specCmd inDir = "cd " ++ inDir ++ ";StructuralSpec " ++ (if force opts then "-f " else "") ++ "-o bsv -i ${STRUCTURALSPEC_HOME}/lib:${STRUCTURALSPEC_HOME}/lib/multi " ++ topFile opts
  let bsvCmd inDir outDir = "cd " ++ inDir ++ "/bsv; bsc -u -unsafe-always-ready -verilog -vdir " ++ outDir ++ " -bdir bdir -p +:${STRUCTURALSPEC_HOME}/lib/" ++ outDir ++ ":${STRUCTURALSPEC_HOME}/lib -aggressive-conditions -v95 -steps-warn-interval 100000000 -g " ++ topModule opts ++ " " ++ name opts ++ ".bsv +RTS -K4G -RTS 2>&1 | ignoreBsc.pl"
  let runSpec inDir = do{putStrLn $ specCmd inDir; system $ specCmd inDir}
  let runBsv inDir outDir = do{putStrLn $ bsvCmd inDir outDir; system $ bsvCmd inDir outDir}
  let whenRet cond x = when cond $ x >> return ()
  whenRet (genBsv opts) $ do
    system "mkdir -p bsv"
    runSpec "."
  whenRet (genVerilog opts) $ do
    system "mkdir -p bsv/bdir bsv/single"
    runBsv "." "single"
    system "ln -sf -t bsv/single ${STRUCTURALSPEC_HOME}/lib/single/*.v"
  whenRet (genMulti opts) $ do
    allFiles <- getDirectoryContents "bsv/single"
    let files = [x|x <- allFiles, isJust $ matchRegex (mkRegex "^.*\\.v$") x, x /= "Base.v", x /= "Rand.v", x /= "RegFile.v"]
    system "mkdir -p bsv/multi"
    foldl (\x file -> do
                        x
                        putStrLn $ "Applying Multicycle on " ++ file
                        system $ "Multicycle -o bsv/multi -m " ++ (intercalate ":" $ multiMods opts) ++ " bsv/single/" ++ file
          ) (return ExitSuccess) files
    system "ln -sf -t bsv/multi ${STRUCTURALSPEC_HOME}/lib/multi/*.v"
  whenRet (genRefined opts) $ do
    system "mkdir -p buildRefined buildRefined/bsv/bdir buildRefined/bsv/multi"
    system $ "ln -sf -t buildRefined `pwd`/*.spec"
    system $ "ln -sf -t buildRefined `pwd`/" ++ refinedDir opts ++ "/*.spec"
    runSpec "buildRefined"
    runBsv "buildRefined" "multi"
    foldl (\x file -> x >> (system $ "cp -f buildRefined/bsv/multi/" ++ file ++ ".v bsv/multi/" ++ file ++ "_FIFO_ALL_EXPOSED.v")) (return ExitSuccess) (refinedMods opts)
  whenRet (genExec opts) $ do
    let cmd inDir name = "cd bsv/" ++ inDir ++ "; bsc -e " ++ topModule opts ++ name ++ " *.v"
    putStrLn $ cmd "single" ""
    system $ cmd "single" ""
    whenRet (genMulti opts) $ do
      putStrLn $ cmd "multi" "_FIFO_OUTER_NOT_EXPOSED"
      system $ cmd "multi" "_FIFO_OUTER_NOT_EXPOSED"
