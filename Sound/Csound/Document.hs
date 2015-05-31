module Sound.Csound.Document where

import Sound.Csound.Orchestra
import Sound.Csound.Score

renderCsoundDocument :: Orchestra -> Score -> String
renderCsoundDocument orc sco = concat [
  "<CsoundSynthesizer>\n",
  "<CsInstruments>\n", show orc, "\n</CsInstruments>\n",
  "<CsScore>\n", show sco, "\n</CsScore>\n",
  "</CsoundSynthesizer>"
  ]
  
