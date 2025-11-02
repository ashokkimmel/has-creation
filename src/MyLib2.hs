{-# LANGUAGE TemplateHaskell #-}
module MyLib2 where
import MyLib 
--import Language.Haskell.TH
data BoolTuple a = BoolTuple Bool a deriving Show
$(generateToBool ''BoolTuple)

--[| instance Show (a -> b) where show _ = "<function>" |]