--
-- ECDSA haskell implemention
-- module FileParser
-- This file implementing File parsing
-- @author Lukáš Plevač <at> BUT FIT 
--
module ParseInput where
import Types
import Data.Bits

---
--- Support function
---

bytesMask :: Integer -> Integer
bytesMask i = if i > 0 then 0xFF .|. ((bytesMask (i - 1)) `shiftL` 8) else 0

parseSECFormat :: Integer -> Point
parseSECFormat i = (Point xp yp)
    where
        xp :: Integer
        xp =  (i `shiftR` 256) .&. (bytesMask 32)

        yp :: Integer
        yp =   i               .&. (bytesMask 32)

---
--- CLI Arguments parser part
---

parseOptions :: [String] -> Options -> Options
parseOptions []              opts = opts

parseOptions ("-i":args) opts  = parseOptions args (opts { optBypass = True   })
parseOptions ("-k":args) opts  = parseOptions args (opts { optNewKey = True   })
parseOptions ("-s":args) opts  = parseOptions args (opts { optSing   = True   })
parseOptions ("-v":args) opts  = parseOptions args (opts { optVerify = True   })

parseOptions (arg:[])    opts  = parseOptions []   (opts { optInputFile = arg  })
parseOptions (_:args)    opts  = parseOptions args opts --skip unknew argument

--
-- File parser
--
parseFile :: String -> Config
parseFile str = parseWords (words str) defaultConfig

parseWords :: [String] -> Config -> Config
parseWords ("Curve":"{":args)     cfg = parseCurve     args cfg
parseWords ("Key":"{":args)       cfg = parseKey       args cfg
parseWords ("Signature":"{":args) cfg = parseSignature args cfg
parseWords ("Hash:":val:args)     cfg = parseWords     args (cfg { hash = (read val::Integer) })
parseWords []                     cfg = cfg
parseWords _                      cfg = (cfg { ok = False })

parseCurve :: [String] -> Config -> Config
parseCurve ("p:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveP (curve cfg) (read val::Integer) })
parseCurve ("a:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveA (curve cfg) (read val::Integer) })
parseCurve ("b:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveB (curve cfg) (read val::Integer) })
parseCurve ("n:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveN (curve cfg) (read val::Integer) })
parseCurve ("h:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveH (curve cfg) (read val::Integer) })
parseCurve ("g:":"Point":"{":args) cfg  = parseCurvePoint args cfg
parseCurve ("}":args)              cfg  = parseWords args cfg
parseCurve _                       cfg = (cfg { ok = False })

parseCurvePoint :: [String] -> Config -> Config
parseCurvePoint ("x:":val:args) cfg  = parseCurvePoint args (cfg { curve = updateCurvePointX (curve cfg) (read val::Integer) })
parseCurvePoint ("y:":val:args) cfg  = parseCurvePoint args (cfg { curve = updateCurvePointY (curve cfg) (read val::Integer) })
parseCurvePoint ("}":args)      cfg  = parseCurve args cfg
parseCurvePoint _               cfg = (cfg { ok = False })

parseKey :: [String] -> Config -> Config
parseKey ("d:":val:args) cfg  = parseKey args (cfg { key = updateKeyD (key cfg) (read val::Integer) })
parseKey ("Q:":val:args) cfg  = parseKey args (cfg { key = updateKeyQ (key cfg) (parseSECFormat (read val::Integer)) })
parseKey ("}":args)      cfg  = parseWords args cfg
parseKey _               cfg = (cfg { ok = False })

parseSignature :: [String] -> Config -> Config
parseSignature ("r:":val:args) cfg  = parseSignature args (cfg { signature = updateSignatureR (signature cfg) (read val::Integer) })
parseSignature ("s:":val:args) cfg  = parseSignature args (cfg { signature = updateSignatureS (signature cfg) (read val::Integer) })
parseSignature ("}":args)      cfg  = parseWords args cfg
parseSignature _               cfg = (cfg { ok = False })