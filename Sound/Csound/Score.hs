module Sound.Csound.Score (
  Gen (..),
  Score (..)
) where

import Data.List

-- TODO add all GEN routines
data Gen = GEN10 [Double]

instance Show Gen where
  show (GEN10 xs) = unwords $ "10" : (map show xs)

data Score = BlankLine
           | Comment String
           | A Double Double 
           | B Double
           | E (Maybe Double)
           | F Int Double Int Gen
           | I Int Double Double [Double]
           | M String
           | N String 
           | Q Int Double Int
           | R Int
           | S (Maybe Double)
           | T Double [(Double,Double)]
           | V Double
           | X 
           | MultiScore [Score]

instance Show Score where
  show BlankLine = ""
  show (Comment str) = "; " ++ str
  
  show (A a b)      = unwords ["a", "0", show a, show b]
  show (B a)        = unwords ["b", show a]
  show (E Nothing)  = unwords ["e"]
  show (E (Just a)) = unwords ["e", show a]
  show (F a b c d)  = unwords ["f", show a, show b, show c, show d]
  show (I a b c d)  = unwords $ ["i", show a, show b, show c] ++ map show d
  show (M a)        = unwords ["m", a]
  show (N a)        = unwords ["n", a]
  show (Q a b c)    = unwords ["q", show a, show b, show c]
  show (R a)        = unwords ["r", show a]
  show (S Nothing)  = unwords ["s"]
  show (S (Just a)) = unwords ["s", show a]
  show (T a b)      = let pair2word (x,y) = show x ++ " " ++ show y
                      in unwords $ ["t", "0", show a] ++ map pair2word b
  show (V a)        = unwords ["v", show a]                       
  show X            = "x"                     

  show (MultiScore acc) = intercalate "\n" $ map show acc
  
             
instance Monoid Score where
  mempty = MultiScore []

  mappend (MultiScore acc) x = MultiScore (acc ++ [x])
  mappend x (MultiScore acc) = MultiScore (x:acc)
  mappend x y = MultiScore [x,y]

  
