{-# LANGUAGE TemplateHaskell #-}
module MyLib where
--import Language.Haskell.TH
import Control.Has.Error (hasFrom)

$(hasFrom ''Bool)