import ParseInput
import Types
import Minimize
import System.Environment (getArgs)

main :: IO ()
main = do
  -- load args
  args <- getArgs
  
  -- parse arcgs
  let options = parseOptions args defaultOptions
  
  -- For debug print parsed args
  --putStrLn (show options)

  -- load config file
  config <- if optInputFile options == "" then getContents else readFile (optInputFile options)
  
  -- parse config file
  let parsedConfig = parseFile config

  -- print parsed config if arg definaded
  if (optBypass options) == True 
    then putStrLn (show parsedConfig)
    else pure ()

  -- generate new key if arg definaded
  if (optNewKey options) == True
    then do
      privateKey <- generatePrivateKey     parsedConfig
      -- For testing without random
      --let privateKey = 112757557418114203588093402336452206775565751179231977388358956335153294300646
      let gkey    = generateKey parsedConfig privateKey
      putStrLn (show gkey)
    else pure ()
  