{-# LANGUAGE FlexibleInstances #-}

module Sound.Csound.Orchestra (OrcArg(..), Opcode(..), instr) where

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

data Orchestra = Instrument Int [([String],Opcode)]

renderLine :: ([String],Opcode) -> String
renderLine (outs,opcode) = intercalate "," outs ++ " " ++ show opcode

instance Show Orchestra where

  show (Instrument n olines) =
    let body = unlines $ map renderLine olines
    in "instr " ++ show n ++ "\n" ++ body ++ "endin"
                                      

instr = Instrument 1 [
  (["a1"], Oscil (ODbl 10000) (ODbl 440) (OInt 1)),
  ([],     Out   "a1")
  ]

