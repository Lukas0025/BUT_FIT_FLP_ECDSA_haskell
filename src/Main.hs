import Arguments
import FileParser
import System.Environment (getArgs)

main :: IO ()
main = do
  args        <- getArgs
  let options = parseOptions args defaultOptions
  config  <- if optInputFile options == "" then getContents else readFile (optInputFile options)
  let parsedConfig = parseFile config
  putStrLn (show options)
  putStrLn (show parsedConfig)