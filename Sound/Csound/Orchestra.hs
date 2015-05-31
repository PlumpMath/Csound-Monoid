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

data Opcode = Oscil OrcArg OrcArg OrcArg
            | Out String

instance Show Opcode where
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
               | Orchestra String
               | EmptyOrc 

instance Monoid Orchestra where
  mempty = EmptyOrc
  mappend EmptyOrc a = a
  mappend a EmptyOrc = a
  mappend a b = Orchestra $ renderOrchestra a ++ "\n" ++ renderOrchestra b

renderOrchestra :: Orchestra -> String
renderOrchestra (Instr n instr) = renderInstrument n instr
renderOrchestra (Orchestra str) = str

instance Show Orchestra where
  show = renderOrchestra
                                      
testInstr = Instrument [
  (["a1"], Oscil (ODbl 10000) (ODbl 440) (OInt 1)),
  ([],     Out   "a1")
  ]

testOrc = Instr 1 testInstr `mappend` Instr 2 testInstr

