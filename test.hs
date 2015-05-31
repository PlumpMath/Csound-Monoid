import Sound.Csound.Document
import Sound.Csound.Orchestra
import Sound.Csound.Score

import Data.Monoid

p4 = OStr "p4"
p5 = OStr "p5"
p6 = OStr "p6"

oscil :: Instrument
oscil = Instrument [
  (["a1"], Oscil p4 p5 p6),
  ([],     Out "a1")
  ]

orc :: Orchestra
orc = Instr  1 oscil

geni start freq = 
        let num   = 1
            dur   = 0.1
            amp   = 10000
            tbl   = 1
        in I num start dur [amp,freq,tbl]

gensco n = mconcat $ gensco' 0
  where gensco' i =
          if i > n
             then []
             else geni (i * 0.1) (220 + i * 10) : gensco' (i + 1)

sco :: Score
sco =  F 1 0 4096 (GEN10 [1])
    <> gensco 100
           
main = putStrLn $ renderCsoundDocument orc sco
