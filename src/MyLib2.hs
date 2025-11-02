{-# LANGUAGE TemplateHaskell #-}
module MyLib2 where
import MyLib 
--import Language.Haskell.TH
data BoolSum a = BoolS Bool | Id a deriving Show
$(generateCatchBool ''BoolSum)

--[| instance Show (a -> b) where show _ = "<function>" |]