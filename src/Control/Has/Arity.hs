module Control.Has.Arity where 
import Language.Haskell.TH
import Control.Monad (replicateM)

getArity :: Name -> Q [Name]
getArity dataName = do
    info <- reify dataName
    datadecl <- case info of
        (TyConI decinfo) -> return decinfo
        _ -> fail "Expected the name of a data type"
   
    num <- case datadecl of
      DataD    _ _ lst _ _ _ -> return $ length lst
      NewtypeD _ _ lst _ _ _ -> return $ length lst
      _ -> fail "Expected the name of a data type"
    replicateM num (newName "a")