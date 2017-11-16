module Hake where

import System.Environment
import System.Exit

import Data.List (find)

data Task
  = MkTask { taskName :: String, description :: String, dependencies :: [Task], action :: IO ()}

namespace :: String -> [Task] -> [Task]
namespace name tasks = map (\t -> t { taskName = name ++ ":" ++ (taskName t) }) tasks

{-
  Describe a rake task

  This is purely for syntactic sugar
-}
describe :: String -> Task -> Task
describe desc task = task { description = desc }

{-
  Make a task with no description
-}
task :: String -> [Task] -> IO () -> Task
task name deps act = MkTask name "" deps act

runTask :: Task -> IO ()
runTask task = mapM_ runTask (dependencies task) >> action task

{-
  Entrypoint for hake tasks

  Looks up the task passed in as an argument and attempts to perform the task with that name

  Forwards all remaining arguments to that task (TBD)
  Also provides help functionality aka (rake -T) (TBD)
  Should probably also provide functionality to run multiple tasks in one go (TBD)
-}

hake tasks = do
  args <- getArgs -- lol do some actual options parsing
  case args of
    [] -> do
      putStrLn "Please provide a task name"
      exitFailure
    (nm : args) -> do
      let taskToRun = find ((==) nm . taskName) tasks
      case taskToRun of
        Just task -> runTask task
        Nothing   -> do
          putStrLn $ "Task " ++ nm ++ " could not be found"
          exitFailure
