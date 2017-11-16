module Task.SimpleTask where

import Hake

tasks = [simpleTask, firstThis, thenThis]

simpleTask = MkTask
  "simple"
  "A very redundantly named task!"
  [] $ do
    putStrLn "Hello, Hake!"

firstThis =  MkTask
  "first"
  "A task that should run first!"
  [] $ do
    putStrLn "I'm first!"

thenThis = MkTask
  "second"
  "A task that should run second!"
  [firstThis] $ do
    putStrLn "I'm second!"

otherTask =
  describe "omg" $
    task "other" [] $ do
      putStrLn "omg"
