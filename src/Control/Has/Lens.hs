{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Control.Has.Lens  where

import Control.Monad (replicateM)
import Language.Haskell.TH
import Data.List (elemIndex)
useInstance :: ((Exp,Exp,Int) -> Q a) -> Name -> Name  -> Q a
useInstance useExp innerDatatype outerDatatype = do
  info <- reify outerDatatype
  datadecl <- case info of
    (TyConI decinfo) -> return decinfo
    _ -> fail "Expected the name of a data type"
  (con,arity) <- 
    case datadecl of
      DataD    _ _ lst _ [conInfo] _ -> return (conInfo, length lst)
      NewtypeD _ _ lst _  conInfo  _ -> return (conInfo, length lst)
      DataD _ _ _ _ _ _  -> fail "Expected a single constructor data type"
      _ -> fail "Expected the name of a data type"
  let 
    getPosAndLength :: [Type] -> Q (Int, Int)
    getPosAndLength types = 
      maybe (fail (nameBase innerDatatype
                    <> " was not in the constructor."))
              (\innerpos -> return (innerpos, length types))
              (elemIndex (ConT innerDatatype) types)
  let 
    getAllConInfo (NormalC cname bangtype) =
      (cname,) <$> getPosAndLength (map snd bangtype)
    getAllConInfo (RecC cname varbangtype) =
      (cname,) <$> getPosAndLength (map (\(_,_,t) -> t) varbangtype)
    getAllConInfo (InfixC (_,t1) cname (_,t2)) =
      (cname,) <$> getPosAndLength [t1, t2]
    getAllConInfo (ForallC _ _ tcon) = getAllConInfo tcon
    getAllConInfo (GadtC [cname] bangtypes _) =
      (cname,) <$> getPosAndLength (map snd bangtypes)
    getAllConInfo (RecGadtC [cname] varbangtypes _) =
      (cname,) <$> getPosAndLength (map (\(_,_,t) -> t) varbangtypes)
    getAllConInfo _ =
      fail "GADT with multiple constructors not supported"
  (constructorName,(innerLoc,conLength)) <- getAllConInfo con
  getterfun <- do
    x <- newName "x"
    let ptrns = setIndex innerLoc (replicate conLength wildP) (varP x)
    let getterBody = varE x
    
    let fullptrn = [conP constructorName ptrns :: Q Pat ]
    (lamE fullptrn getterBody)
  setterfun <- do
    newval <- newName "newval"
    ptrnnames <- (replicateM conLength (newName "old"))
    let ptrns = setIndex innerLoc (VarP <$> ptrnnames) WildP
    let constructargs = setIndex innerLoc (VarE <$> ptrnnames) (VarE newval)
    let setterBody = foldl' AppE (ConE constructorName) constructargs
    return $ (LamE [VarP newval, ConP constructorName [] ptrns] setterBody)
  useExp (getterfun, setterfun,arity)

-- | takes in the  Class name, the name of the getter method
-- | the name of the setter method,inner datatype, the outer datatype, returns the an instance of the class
apply :: Exp -> [Exp] -> Exp 
apply = foldl' AppE 
setIndex :: Int -> [a] -> a -> [a]
setIndex 0 (_:xs) newVal = newVal:xs
setIndex idx (x:xs) newVal = x : setIndex (idx -1) xs newVal
setIndex _ [] _ = error "The impossible happened, please tell a maintainer setIndex called with too low index."
withStateMethods :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
withStateMethods className getterName setterName innerDatatype outerDatatype = useInstance useExp innerDatatype outerDatatype  where 
  useExp (getterMaker, setterMaker, arity) = do
    names <- replicateM arity (newName "s")
    return [InstanceD Nothing []  (AppT (ConT className) (foldl' AppT (ConT outerDatatype) (VarT <$> names)))
       [ ValD (VarP getterName) (NormalB (getterMaker)) []
       , ValD (VarP setterName) (NormalB (setterMaker)) []
       ]]
-- | takes in the  Class name, the name of the getter method
-- | the inner datatype, the outer datatype, returns the an instance of the class
withGetterMethod :: Name -> Name -> Name -> Name -> Q [Dec]
withGetterMethod className getterName innerDatatype outerDatatype = useInstance useExp innerDatatype outerDatatype  where 
  useExp (getterMaker, _,arity) = do
    names <- replicateM arity (newName "s") 
    return [InstanceD Nothing []  (AppT (ConT className) (foldl' AppT (ConT outerDatatype) (VarT <$> names)))
       [ ValD (VarP getterName) (NormalB getterMaker) []
       ]]
