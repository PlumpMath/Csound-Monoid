{-# LANGUAGE FlexibleInstances #-}

module Sound.Csound.Orchestra (
  OrcArg(..),
  Opcode(..),
  Instrument(..),
  Orchestra(..)
) where

import Data.List

data OrcArg = ODbl Double | OInt Int | OStr String

instance Show OrcArg where
  show (ODbl d) = show d
  show (OInt i) = show i
  show (OStr s) = s

data Opcode = Foscil OrcArg OrcArg OrcArg OrcArg OrcArg OrcArg
            | Oscil OrcArg OrcArg OrcArg
            | Out String

instance Show Opcode where
  show (Foscil a b c d e f) = "foscil " ++ intercalate "," (map show [a,b,c,d,e,f])
  show (Oscil a b c) = "oscil " ++ intercalate "," [show a, show b, show c]
  show (Out a) = "out " ++ a

data Instrument = Instrument [([String],Opcode)]

renderLine :: ([String],Opcode) -> String
renderLine (outs,opcode) = intercalate "," outs ++ " " ++ show opcode

renderInstrument :: Int -> Instrument -> String
renderInstrument n (Instrument olines) =  
    let body = unlines $ map renderLine olines
    in "instr " ++ show n ++ "\n" ++ body ++ "endin"

data Orchestra = Instr Int Instrument
               | Orchestra [Orchestra]

instance Monoid Orchestra where
  mempty = Orchestra []
  mappend (Orchestra acc) x = Orchestra (acc ++ [x])
  mappend x (Orchestra acc) = Orchestra (x:acc)
  mappend a b = Orchestra [a,b]

renderOrchestra :: Orchestra -> String
renderOrchestra (Instr n instr) = renderInstrument n instr
renderOrchestra (Orchestra xs) = intercalate "\n" $ map renderOrchestra xs

instance Show Orchestra where
  show = renderOrchestra
                                      
