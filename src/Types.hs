module Types where

--
-- CLI Parameters (options)
--
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

--
-- Program config 
-- save in parsed file
--
data Config = Config {
    curve     :: Curve,
    key       :: Key,
    signature :: Signature
} deriving (Show)

data Point = Point { 
  x :: Integer,
  y :: Integer
} deriving (Show)

data Curve = Curve {
    p :: Integer,
    a :: Integer,
    b :: Integer,
    g :: Point,
    n :: Integer,
    h :: Integer
} deriving (Show)

data Key = Key {
    d :: Integer,
    q :: Integer
} deriving (Show)

data Signature = Signature {
    r :: Integer,
    s :: Integer
} deriving (Show)


defaultConfig = Config {
    curve = Curve {
        p = 0,
        a = 0,
        b = 0,
        
        g = Point {
            x = 0,
            y = 0
        },

        n = 0,
        h = 0
    },

    key = Key {
        d = 0,
        q = 0
    },

    signature = Signature {
        r = 0,
        s = 0
    }
}