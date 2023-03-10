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
    signature :: Signature,
    ok        :: Bool
} deriving (Show)

data Point = Point { 
  x :: Integer,
  y :: Integer
} deriving (Show)

instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

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


defaultConfig :: Config
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
    },

    ok = True
}

updateCurveP :: Curve -> Integer -> Curve
updateCurveP (Curve _ a b (Point x y) n h) new     = Curve new a   b   (Point x y) n   h

updateCurveA :: Curve -> Integer -> Curve
updateCurveA (Curve p _ b (Point x y) n h) new     = Curve p   new b   (Point x y) n   h

updateCurveB :: Curve -> Integer -> Curve
updateCurveB (Curve p a _ (Point x y) n h) new     = Curve p   a   new (Point x y) n   h

updateCurveN :: Curve -> Integer -> Curve
updateCurveN (Curve p a b (Point x y) _ h) new     = Curve p   a   b   (Point x y) new h

updateCurveH :: Curve -> Integer -> Curve
updateCurveH (Curve p a b (Point x y) n _) new     = Curve p   a   b   (Point x y) n   new

updateCurvePointX :: Curve -> Integer -> Curve
updateCurvePointX (Curve p a b (Point _ y) n h) new = Curve p a b (Point new y  ) n h

updateCurvePointY :: Curve -> Integer -> Curve
updateCurvePointY (Curve p a b (Point x _) n h) new = Curve p a b (Point x   new) n h

updateKeyD :: Key -> Integer -> Key
updateKeyD (Key _ q) new = Key new q

updateKeyQ :: Key -> Integer -> Key
updateKeyQ (Key d _) new = Key d   new

updateSignatureR :: Signature -> Integer -> Signature
updateSignatureR (Signature _ s) new = Signature new s

updateSignatureS :: Signature -> Integer -> Signature
updateSignatureS (Signature r _) new = Signature r   new