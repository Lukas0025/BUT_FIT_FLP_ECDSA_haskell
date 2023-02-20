--
-- ECDSA haskell implemention
-- module Arguments
-- This file implementing arguments parsing
-- @author Lukáš Plevač <at> BUT FIT 
--
module Arguments where

data Options = Options{ 
    optInputFile  :: FilePath,
    optBypass     :: Bool,
    optNewKey     :: Bool,
    optSing       :: Bool,
    optVerify     :: Bool
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options { 
    optInputFile  = "",
    optBypass     = False,
    optNewKey     = False,
    optSing       = False,
    optVerify     = False
}

parseOptions :: [String] -> Options -> Options
parseOptions []              opts = opts

--parseOptions ("-i":arg:args) opts = parseOptions args (opts { optInputFile = arg  })
parseOptions ("-i":args) opts  = parseOptions args (opts { optBypass = True   })
parseOptions ("-k":args) opts  = parseOptions args (opts { optNewKey = True   })
parseOptions ("-s":args) opts  = parseOptions args (opts { optSing   = True   })
parseOptions ("-v":args) opts  = parseOptions args (opts { optVerify = True   })

parseOptions (arg:[])    opts  = parseOptions []   (opts { optInputFile = arg  })
parseOptions (arg:args)  opts  = parseOptions args opts