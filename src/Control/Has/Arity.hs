module Control.Has.Arity where 
getArity :: Name -> Q Int 
getArity dataName = do
    info <- reify dataName
    datadecl <- case info of
        (TyConI decinfo) -> return decinfo
        _ -> fail "Expected the name of a data type"
   
    case datadecl of
      DataD    _ _ lst _ _ _ -> return length lst
      NewtypeD _ _ lst _ _  _ -> return length lst
      DataD _ _ _ _ _ _  -> fail "Expected a single constructor data type"
      _ -> fail "Expected the name of a data type"
