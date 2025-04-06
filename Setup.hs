import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = copyExecutable
      }

copyExecutable _ _ pkg lbi = do
  let exeName = "aoc2024-haskell"
      exePath = buildDir lbi </> exeName </> exeName
      destPath = exeName
  putStrLn $ "Copying " ++ exePath ++ " to " ++ destPath
  copyFile exePath destPath