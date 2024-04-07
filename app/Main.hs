module Main where

import Control.Concurrent (threadDelay)
import GHC.IO.Handle.Text (hPutStrLn)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (IOMode (WriteMode), withFile)
import System.Process (callCommand)

-- Gets the first argument and returns it if it exists otherwise returns an empty string
handleArgs :: IO String
handleArgs = do
  args <- getArgs
  case args of
    arg : _ -> return arg
    _ -> return ""

-- Creates project folder, main.go file and inits a module with the same name
createProject :: String -> IO String
createProject maybeName = do
  projectName <-
    if null maybeName
      then do
        putStrLn "Enter project name:"
        getLine
      else return maybeName
  putStrLn ""

  -- Shows error and quits if directory already exists otherwise goes on as normal
  projectExists <- doesDirectoryExist projectName
  if projectExists
    then do
      putStrLn ("A project with the name '" ++ projectName ++ "' already exists.")
      putStrLn "Try again with a different name."
      _ <- exitWith (ExitFailure 1)
      return ""
    else do
      createDirectory projectName

      withFile
        (projectName ++ "/main.go")
        WriteMode
        ( \file -> do
            hPutStrLn file "package main\n\nfunc main() {\n\n}"
        )

      callCommand ("cd " ++ projectName ++ "; go mod init " ++ projectName)

      threadDelay 500000
      return projectName

-- Prints info about project and what to do next
printInfo :: String -> IO ()
printInfo projectName = do
  putStrLn ""
  putStrLn ("Project " ++ projectName ++ " created and initialized.")
  putStrLn "Start developing by running:"
  putStrLn ("cd " ++ projectName)
  putStrLn ""
  putStrLn "Good luck!"

-- Main function
main :: IO ()
main = do
  maybeName <- handleArgs
  projectName <- createProject maybeName
  printInfo projectName