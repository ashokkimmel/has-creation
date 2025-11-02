generateThrowMatch :: Name -> Name -> Q (Exp,Exp)
generateThrowMatch innerDatatype outerDatatype = do
  info <- reify outerDatatype
  datadecl <- case info of
    (TyConI decinfo) -> return decinfo
    _ -> fail "Expected the name of a data type"
  cons <- case datadecl of
    DataD _ _ _ _ consInfo _ -> return consInfo
    NewtypeD _ _ _ _ conInfo _ -> return [conInfo]
    DataD _ _ _ _ _ _  -> fail "Expected a single constructor data type"
    _ -> fail "Expected the name of a data type"
  let singlecons = filter ((==1) . howManyConstructors) cons
  let propercons = filter ((==ConT innerDatatype) . getInner) singlecons
  innertype <- traverse getInner propercons
  let withProperTypes = filter (\(t, _) -> t == ConT innerDatatype) (zip innertype propercons)
  constructorName <-
   case withProperTypes of
    [] -> fail (nameBase innerDatatype <> " was not in the constructor.")
    ((_,con):_) ->  return $ getName con
  fromFun <- do 
    x <- newName "x"
    return $ LamE [VarP x] (NormalB (ConP constructorName [VarP x]))
  matchFun <- do
    x <- newName "x"
    return $ LamCaseE 
      [ Match (ConP constructorName [VarP x]) (NormalB (VarE x)) []
      , Match WildP (NormalB (AppE (VarE 'error) (LitE (StringL ("Not a " <> nameBase innerDatatype))))) []
      ]
  return (fromFun, matchFun)
howManyConstructors :: Con -> Int
howManyConstructors (NormalC _ sts) = length sts
howManyConstructors (RecC _ vsts) = length vsts
howManyConstructors (InfixC _ _ _) = 2
howManyConstructors (ForallC _ _ con) = howManyConstructors con
howManyConstructors (GadtC _ sts _) = length sts
howManyConstructors (RecGadtC _ vsts _) = length vsts

getInner :: Con -> Type
getInner (NormalC _ [(_,t)]) = t
getInner (RecC _ [( _,_,t)]) = t
getInner (ForallC _ _ con) = getInner con
getInner (GadtC _ [(_,t)] _) = t
getInner (RecGadtC _ [( _,_,t)] _) = t
getInner _ = error "Report a bug in getInner to ashok.kimmel@gmail.com"

getName :: Con -> Name 
getName (NormalC name _) = name
getName (RecC name _) = name
getName (ForallC _ _ con) = getName con
getName (GadtC (name:_) _ _) = name
getName (RecGadtC (name:_) _ _) = name
getName (Infix _ name _) = name
getName _ = error "TH invariants violated with getConstructor name"