module Hake.Run where
import System.Directory
import System.FilePath

import Data.List
import Control.Monad

import Data.List
import Data.Maybe (mapMaybe, fromJust)
import Data.Char

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import System.Environment

import Data.Text.IO as Text (writeFile)
import System.IO
import System.Exit
data Task = Task
  { path :: FilePath
  , moduleName :: String
  } deriving (Show, Eq)

run = do
  args <- getArgs
  case args of
    src : _ : dst : _ -> do
      hPutStrLn stderr dst
      tasks <- getTasksIn src
      Text.writeFile dst (renderStrict . layoutPretty opts $ mkTaskModule tasks)
    _ -> do
      hPutStrLn stderr "specify a directory to find tasks in"
      exitFailure
  where
  opts = defaultLayoutOptions

mkTaskModule tasks = vcat $
  [ pretty "module" <+> pretty "Main" <+> pretty "where"
  , pretty "import" <+> pretty "Hake"
  ] ++ map importTask tasks ++
  [ pretty "main"  <+> pretty "=" <+> pretty "hake" <+> pretty "tasks"
  , pretty "tasks" <+> pretty "::" <+> pretty "[Task]"
  , pretty "tasks" <+> pretty "=" <+> pretty "concat" <+> hang 2 (list (map invokeTask tasks))
  ]

invokeTask task = pretty (moduleName task) <> dot <> pretty "tasks"
importTask task = pretty "import" <+> pretty "qualified" <+> pretty (moduleName task)

getTasksIn :: FilePath -> IO [Task]
getTasksIn path = do
  let (dir, file) = splitFileName path
  getTaskFiles dir >>= pure . mapMaybe (fileToTask dir) . filter (/= path)
  where

  fileToTask :: FilePath -> FilePath -> Maybe Task
  fileToTask dir file = let
    dirParts = splitDirectories dir
    modParts = fromJust . stripPrefix dirParts . splitDirectories . dropExtensions $ file
    in case all isValidModuleName modParts of
      True  -> Just (Task file $ intercalate "." modParts)
      False -> Nothing

-- See `Test.Hspec.Discover.Run`
-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

getTaskFiles :: FilePath -> IO [FilePath]
getTaskFiles dir = filter ("Task.hs" `isSuffixOf`) <$> getFilesRecursively dir

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively baseDir = do
  (directories, files) <- listDirectory baseDir >>= partitionM doesDirectoryExist . map (baseDir </>)
  subdirectoryFiles <- forM directories $ getFilesRecursively
  return $ files ++ concat subdirectoryFiles
