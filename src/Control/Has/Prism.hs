{-# LANGUAGE TemplateHaskellQuotes #-}
module Control.Has.Prism where 
import Language.Haskell.TH
import Control.Arrow ((&&&))
generateThrowMatch :: Name -> Name -> Q (Exp,Exp)
generateThrowMatch innerDatatype outerDatatype = do
  info <- reify outerDatatype
  datadecl <- case info of
    (TyConI decinfo) -> return decinfo
    _ -> fail "Expected the name of a data type"
  cons <- case datadecl of
    DataD _ _ _ _ consInfo _ -> return consInfo
    NewtypeD _ _ _ _ conInfo _ -> return [conInfo]
    _ -> fail "Expected the name of a data type"
  let singlecons = filter ((==1) . howManyConstructors) cons
  let propercons = filter ((==ConT innerDatatype) . getInner) singlecons
  let withinnertypes = map (getInner &&& id) propercons
  let withProperTypes = snd <$> filter (\(t, _) -> t == ConT innerDatatype) withinnertypes
  constructorName <-
   case withProperTypes of
    [] -> fail (nameBase innerDatatype <> " was not in the constructor.")
    (con:_) ->  return $ getName con
  fromFun <- do 
    x <- newName "x"
    return $ LamE [VarP x] (ConE constructorName `AppE` VarE x)
  matchFun <- do
    x <- newName "x"
    return $ LamCaseE 
      [ Match (ConP constructorName [] [VarP x]) (NormalB (ConE 'Just `AppE` VarE x)) []
      , Match WildP (NormalB (ConE 'Nothing)) []
      ]
  return (fromFun, matchFun)
-- takes in class name, method name, inner datatype name
writeMatchFun :: Name -> Name -> Name -> Name -> Q [Dec]
writeMatchFun className methodName innerDataType outerDataType =  (generateThrowMatch innerDataType outerDataType) >>= mkInstance where
   


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
getName (InfixC _ name _) = name
getName _ = error "TH invariants violated with getConstructor name"