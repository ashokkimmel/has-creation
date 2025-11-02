{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Control.Has.Env (hasTo, hasToWith) where
import Control.Monad.Reader (reader,MonadReader)
import Language.Haskell.TH
import Control.Has.Config (Config(..), defaultConfig)
import Control.Has.Lens (withGetterMethod)
import Language.Haskell.TH.Syntax (lift)
-- | Generate a conversion class and a trivial instance for a target type.
--
-- Example:
--
-- > $(hasTo ''Bool)
--
-- expands to roughly:
--
-- > class ToBool a where
-- >     toBool :: a -> Bool
-- >
-- > instance ToBool Bool where
-- >     toBool = id
--
-- The generated class is named `To<Type>` and the method `to<Type>`.
-- A simple instance for the target type is provided that maps with `id`.
--
-- Note: this is intentionally small and only handles simple nullary type names.
hasToWith :: Config -> Name -> Q [Dec]
hasToWith cfg tyName = do
    let typeStr = case nameOverride cfg of
                    Just s -> s
                    Nothing -> nameBase tyName
        className = mkName ("To" ++ typeStr)
        methodName = mkName ("to" ++ typeStr)
        readerName = mkName ("ask" ++ typeStr)
        generateName = mkName ("generateTo" ++ typeStr)
    a <- newName "a"    
    -- class declaration: class ToX a where toX :: a -> X
    let sig = sigD methodName (appT (appT arrowT (varT a)) (conT tyName))
    cls <- classD (cxt []) className [(PlainTV a BndrReq)] [] [sig]
    -- instance: instance ToX X where toX = id
    let instMethod = funD methodName [clause [] (normalB (varE 'id)) []]
    inst <- instanceD (cxt []) (appT (conT className) (conT tyName)) [instMethod]
    monadName <- newName "m"
    thingReaderName <- newName "r"
    readerSig <- sigD 
                    readerName
                    (forallT [(plainTV monadName),(plainTV thingReaderName)]
                             (cxt [(conT ''MonadReader) `appT` (varT thingReaderName) `appT` (varT monadName)
                                  ,(conT className) `appT` (varT thingReaderName)])
                             ((varT monadName) `appT` (conT tyName)))
     -- reader method: askX :: MonadReader r m => m X; askX = reader toX
    readerMethod <- funD readerName do
        [do
            x <- newName "x"
            clause [] (normalB (varE 'reader `appE` (varE methodName))) []]
    generateInstancesSig <- sigD generateName (arrowT `appT` (conT ''Name) `appT` (appT (conT ''Q) (appT (conT ''[]) (conT ''Dec))))
    generateInstances <-
        valD
            (varP generateName)
            (normalB (varE 'withGetterMethod
                         `appE` (lift className)
                         `appE` (lift methodName)
                         `appE` (lift tyName)))
            []

    return [cls, inst, readerSig,readerMethod, generateInstancesSig, generateInstances]

-- convenience wrapper using the default configuration
hasTo :: Name -> Q [Dec]
hasTo = hasToWith defaultConfig
