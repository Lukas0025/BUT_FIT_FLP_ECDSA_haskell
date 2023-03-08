--
-- ECDSA haskell implemention
-- module FileParser
-- This file implementing File parsing
-- @author Lukáš Plevač <at> BUT FIT 
--
module ParseInput where
import Types 

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
parseOptions (arg:args)  opts  = parseOptions args opts

---
--- File parser part
---

updateCurveP (Curve _ a b (Point x y) n h) new     = Curve new a   b   (Point x y) n   h
updateCurveA (Curve p _ b (Point x y) n h) new     = Curve p   new b   (Point x y) n   h
updateCurveB (Curve p a _ (Point x y) n h) new     = Curve p   a   new (Point x y) n   h
updateCurveN (Curve p a b (Point x y) _ h) new     = Curve p   a   b   (Point x y) new h
updateCurveH (Curve p a b (Point x y) n _) new     = Curve p   a   b   (Point x y) n   new

updateCurvePointX (Curve p a b (Point x y) n h) new = Curve p a b (Point new y  ) n h
updateCurvePointY (Curve p a b (Point x y) n h) new = Curve p a b (Point x   new) n h

updateKeyD (Key _ q) new = Key new q
updateKeyQ (Key d _) new = Key d   new

updateSignatureR (Signature _ s) new = Signature new s
updateSignatureS (Signature r _) new = Signature r   new

--
-- File parser
--
parseFile :: String -> Config
parseFile str = parseWords (words str) defaultConfig

parseWords :: [String] -> Config -> Config
parseWords ("Curve":"{":args)     cfg = parseCurve     args cfg
parseWords ("Key":"{":args)       cfg = parseKey       args cfg
parseWords ("Signature":"{":args) cfg = parseSignature args cfg
parseWords []                     cfg = cfg

parseCurve :: [String] -> Config -> Config
parseCurve ("p:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveP (curve cfg) (read val::Integer) })
parseCurve ("a:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveA (curve cfg) (read val::Integer) })
parseCurve ("b:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveB (curve cfg) (read val::Integer) })
parseCurve ("n:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveN (curve cfg) (read val::Integer) })
parseCurve ("h:":val:args)         cfg  = parseCurve args (cfg { curve = updateCurveH (curve cfg) (read val::Integer) })
parseCurve ("g:":"Point":"{":args) cfg  = parseCurvePoint args cfg
parseCurve ("}":args)              cfg  = parseWords args cfg

parseCurvePoint :: [String] -> Config -> Config
parseCurvePoint ("x:":val:args) cfg  = parseCurvePoint args (cfg { curve = updateCurvePointX (curve cfg) (read val::Integer) })
parseCurvePoint ("y:":val:args) cfg  = parseCurvePoint args (cfg { curve = updateCurvePointY (curve cfg) (read val::Integer) })
parseCurvePoint ("}":args)      cfg  = parseCurve args cfg

parseKey :: [String] -> Config -> Config
parseKey ("d:":val:args) cfg  = parseKey args (cfg { key = updateKeyD (key cfg) (read val::Integer) })
parseKey ("Q:":val:args) cfg  = parseKey args (cfg { key = updateKeyQ (key cfg) (read val::Integer) })
parseKey ("}":args)      cfg  = parseWords args cfg

parseSignature :: [String] -> Config -> Config
parseSignature ("r:":val:args) cfg  = parseSignature args (cfg { signature = updateSignatureR (signature cfg) (read val::Integer) })
parseSignature ("s:":val:args) cfg  = parseSignature args (cfg { signature = updateSignatureS (signature cfg) (read val::Integer) })
parseSignature ("}":args)      cfg  = parseWords args cfg