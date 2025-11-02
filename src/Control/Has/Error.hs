{-# LANGUAGE TemplateHaskellQuotes #-}

module Control.Has.Error (hasFrom, hasFromWith) where

import Language.Haskell.TH
import Control.Has.Config (Config(..), defaultConfig)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Has.Prism (writeFromFun, writeFromMatchFun,writeMatchFun)
import Language.Haskell.TH.Syntax (lift)
-- | Generate a conversion class for parsing/failing from a source type.
--
-- Example:
--
-- > $(hasFrom ''Bool)
--
-- expands to roughly:
--
-- > class FromBool e a where
-- >     fromBool :: Bool -> Either e a
-- >
-- > instance FromBool e Bool where
-- >     fromBool x = Right x
--
-- This generator creates a class named `From<Type>` with a method
-- `from<Type> :: Type -> Either e a` (so parsing can fail with arbitrary
-- error type `e`). It also provides a trivial instance for the target type
-- itself which always succeeds using `Right`.
data ErrorConfig = ErrorConfig
  { superConfig :: Config,
    generateMatch :: Bool
  }
hasFromWith :: ErrorConfig -> Name -> Q [Dec]
hasFromWith cfg tyName = do
  let typeStr = case nameOverride (superConfig cfg) of
                  Just s -> s
                  Nothing -> nameBase tyName
      className = mkName ("From" ++ typeStr)
      methodName = mkName ("from" ++ typeStr)
  e <- newName "e"
  a <- newName "a"

  -- method signature: Type -> Either e a
  let sig = sigD methodName (appT (appT arrowT (conT tyName)) (varT a))

  cls <- classD (cxt []) className [plainTV a] [] [sig]

  -- instance: instance FromX e X where fromX x = Right x
  let instMethod = funD methodName [clause [] (normalB (varE 'id)) []]
  inst <- instanceD (cxt []) (appT (conT className) (conT tyName)) [instMethod]
  throwFunSig <- do 
    m <- newName "m"
    x <- newName "x"
    sub <- newName "a"
    sigD (mkName $ "throw" ++ typeStr) (forallT [plainTV x,plainTV m,plainTV sub]
      (cxt [conT ''MonadError `appT` varT sub `appT` varT m,conT className `appT` varT sub])
      (arrowT `appT` (conT tyName) `appT` (varT m `appT` varT x)))
  throwFun <- do
    input <- newName $ "my" ++ typeStr
    funD (mkName $ "throw" ++ typeStr) 
      [clause [varP input] 
        (normalB 
          ((varE 'throwError) `appE` (varE methodName `appE` (varE input)))) []]
  let fromDecs = [cls, inst, throwFun,throwFunSig]
  
  matchDecs <- 
    if generateMatch cfg
      then 
        hasMatchWith (Just (className, methodName)) (superConfig cfg) tyName
      else do 
        let generateInstancesName = mkName ("generateFrom" ++ typeStr)
        generateInstancesSig <- sigD generateInstancesName (arrowT `appT` (conT ''Name) `appT` (appT (conT ''Q) (appT (conT ''[]) (conT ''Dec))))
        generateInstances <-
          valD
              (varP generateInstancesName)
              (normalB (varE 'writeFromFun
                           `appE` (lift className)
                           `appE` (lift methodName)
                           `appE` (lift tyName)))
              []
        return [generateInstancesSig, generateInstances]
  return (matchDecs ++ fromDecs)
defaultErrorConfig :: ErrorConfig
defaultErrorConfig = ErrorConfig
  { superConfig = defaultConfig
  , generateMatch = True
  }
-- convenience wrapper using defaultConfig
hasFrom :: Name -> Q [Dec]
hasFrom = hasFromWith defaultErrorConfig


-- | Generate a "catch" class for a target type. This creates a class
-- named `Catch<Type>` with a method `catch<Type>` of type
--
-- > m Type -> (e -> m Type) -> m Type
--
-- The generator does NOT create any instances automatically; you can
-- implement instances for your application's monads (for example using
-- `MonadError`).
hasMatchWith :: Maybe (Name, Name) -> Config -> Name -> Q [Dec]
hasMatchWith maybeSuperclass cfg tyName = do
  let typeStr = case nameOverride cfg of
                  Just s -> s
                  Nothing -> nameBase tyName
      matchClassName = mkName ("Match" ++ typeStr)
      matchMethodName = mkName ("match" ++ typeStr)
      mtlMethodName = mkName ("catch" ++ typeStr)
  e <- newName "e" 
  let sigType = arrowT `appT` varT e `appT` (conT ''Maybe `appT` (conT tyName))
  let sig = sigD matchMethodName sigType
  cls <- classD (cxt (case maybeSuperclass of 
                          Just (superclass,_) -> [conT superclass `appT` varT e]
                          Nothing -> [])) matchClassName [plainTV e] [] [sig]
  inst <- instanceD (cxt []) (appT (conT matchClassName) (conT tyName)) [
      funD matchMethodName [clause [] (normalB (conE 'Just)) []]
    ]
--  catchX :: (MonadError e m,MatchX e) => m a -> (x -> m a) -> m a
  mtlSig <- do
    x <- newName "x"
    a <- newName "a"
    m <- newName "m"
    sigD mtlMethodName 
      (forallT [plainTV e, plainTV m, plainTV a]
        (cxt [conT ''MonadError `appT` varT e `appT` varT m, conT matchClassName `appT` varT e])
        (((varT m `appT` varT a) !-> ((conT tyName !-> (varT m `appT` varT a))) !-> (varT m `appT` varT a)))
      )
  mtlFun <- do
    ma <- newName "ma"
    xtoma <- newName "xtoma"
    x <- newName "x"
    funD 
      mtlMethodName
      [clause [varP ma, varP xtoma]
        (normalB 
          (varE 'catchError `appE` varE ma `appE`
            (lamE [varP e] (caseE 
                              (varE matchMethodName `appE` varE e) 
                              [match (conP 'Nothing [])    (normalB (varE 'throwError `appE` varE e)) []
                              ,match (conP 'Just [varP x]) (normalB (varE xtoma `appE` varE x))       []]
                            )
            ) 
          )
        )
        []
      ] 
  
  generateInstances <- 
   case maybeSuperclass of
    Nothing -> do 
      let generateName = mkName ("generateMatch" ++ typeStr)
      generateInstancesSig <- sigD generateName (arrowT `appT` (conT ''Name) `appT` (appT (conT ''Q) (appT (conT ''[]) (conT ''Dec))))
      generateInstances <-
        valD
            (varP generateName)
            (normalB (varE 'writeMatchFun
                         `appE` (lift matchClassName)
                         `appE` (lift matchMethodName)
                         `appE` (lift tyName)))
            []
      return [generateInstancesSig, generateInstances]
    Just (superClass,superMethod) -> do
      let generateName = mkName ("generateCatch" ++ typeStr)
      generateInstancesSig <- sigD generateName (arrowT `appT` (conT ''Name) `appT` (appT (conT ''Q) (appT (conT ''[]) (conT ''Dec))))
      generateInstances <-
        valD
            (varP generateName)
            (normalB (varE 'writeFromMatchFun
                         `appE` (lift superClass)
                         `appE` (lift superMethod)
                         `appE` (lift matchClassName)
                         `appE` (lift matchMethodName)
                         `appE` (lift tyName)))
            []
      return [generateInstancesSig, generateInstances]


  return $ generateInstances ++ [cls, inst,mtlFun,mtlSig]
-- convenience wrapper using defaultConfig
hasMatch :: Name -> Q [Dec]
hasMatch = hasMatchWith Nothing defaultConfig
(!->) :: Quote m => m Type -> m Type -> m Type
a !-> b = arrowT `appT` a `appT` b
infixr 1 !->