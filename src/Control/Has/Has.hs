{-# LANGUAGE TemplateHaskellQuotes #-}

module Control.Has.Has
    ( hasToSet
    , hasToSetWith
    ) where
import Control.Has.Config (Config(..), defaultConfig)
import Control.Monad.State.Class (MonadState)
import Control.Has.Lens (withStateMethods)
import Language.Haskell.TH.Syntax (lift)
-- | Like 'hasTo', but also generate a setter method for the target type.
--
-- Example:
--
-- > $(hasToSet ''Bool)
--
-- expands to roughly:
--
-- > class ToBool a where
-- >     toBool :: a -> Bool
-- >     setBool :: Bool -> a -> a
-- >
-- > instance ToBool Bool where
-- >     toBool = id
-- >     setBool = const
--
-- The generated `set<Type>` method has the type `Type -> a -> a` and the
-- trivial instance for the target type uses `const` (so the new value replaces
-- the old one).
import Language.Haskell.TH
import Control.Monad.State.Class (gets, modify, state)
hasToSetWith :: Config -> Name -> Q [Dec]
hasToSetWith cfg tyName = do
    let typeStr = case nameOverride cfg of
                    Just s -> s
                    Nothing -> nameBase tyName
        className = mkName ("Has" ++ typeStr)
        getName = mkName ("to" ++ typeStr)
        setName = mkName ("set" ++ typeStr)
        generateName = mkName ("generateHas" ++ typeStr)
    a <- newName "a"

    -- method signatures: toX :: a -> X; setX :: X -> a -> a
    let getSig = sigD getName (appT (appT arrowT (varT a)) (conT tyName))
    let setSig = sigD setName (appT (appT arrowT (conT tyName)) (appT (appT arrowT (varT a)) (varT a)))

    cls <- classD (cxt []) className [(PlainTV a BndrReq)] [] [getSig, setSig]

    -- instance methods: toX = id; setX = const
    let instGet = funD getName [clause [] (normalB (varE 'id)) []]
        instSet = funD setName [clause [] (normalB (varE 'const)) []]
    inst <- instanceD (cxt []) (appT (conT className) (conT tyName)) [instGet, instSet]

    -- stateful helper methods:
    -- getX :: MonadState s m => m X
    -- getX = gets toX
    let getHelperName = mkName ("get" ++ typeStr)
    mName <- newName "m"
    sName <- newName "s"
    let constraints = [appT (appT (conT ''MonadState) (varT sName)) (varT mName), appT (conT className) (varT sName)]
    getSigHelper <- sigD getHelperName (forallT []
        (cxt constraints)
        (appT (varT mName) (conT tyName)))
    getMethod <- funD getHelperName [clause [] (normalB (varE 'gets `appE` varE getName)) []]

    -- putX :: MonadState s m => X -> m ()
    -- putX v = modify (setX v)
    let putHelperName = mkName ("put" ++ typeStr)
    v <- newName "v"
    putSigHelper <- sigD putHelperName (forallT []
        (cxt constraints)
        (appT (appT arrowT (conT tyName)) (appT (varT mName) (conT ''()))))
    putMethod <- funD putHelperName [clause [varP v] (normalB (varE 'modify `appE` (varE setName `appE` varE v))) []]

    -- stateX :: MonadState s m => (X -> (r, X)) -> m r
    -- stateX f = state (\s -> case f (toX s) of (r, x') -> (r, setX x' s))
    let stateHelperName = mkName ("state" ++ typeStr)
    f <- newName "f"
    s <- newName "s"
    let rName = mkName "r"
    x' <- newName "x'"
    -- f :: X -> (r, X)
    let pairRX = appT (appT (tupleT 2) (varT rName)) (conT tyName)
        fType = appT (appT arrowT (conT tyName)) pairRX
    stateSigHelper <- sigD stateHelperName (forallT []
        (cxt constraints)
        (appT (appT arrowT fType) (appT (varT mName) (varT rName))))

    let caseExpr = caseE (appE (varE f) (appE (varE getName) (varE s)))
                    [match (tupP [varP rName, varP x']) (normalB (tupE [varE rName, appE (appE (varE setName) (varE x')) (varE s)])) []]
    let lam = lamE [varP s] caseExpr
    stateMethod <- funD stateHelperName [clause [varP f] (normalB (varE 'state `appE` lam)) []]
    generateInstancesSig <- sigD generateName (arrowT `appT` (conT ''Name) `appT` (appT (conT ''Q) (appT (conT ''[]) (conT ''Dec))))
    generateInstances <-
        valD
            (varP generateName)
            (normalB (varE 'withStateMethods
                         `appE` (lift className)
                         `appE` (lift getName)
                         `appE` (lift setName)
                         `appE` (lift tyName)))
            []
    return [cls, inst, getSigHelper, getMethod, putSigHelper, putMethod, stateSigHelper, stateMethod, generateInstances,generateInstancesSig]

-- convenience wrapper using defaultConfig
hasToSet :: Name -> Q [Dec]
hasToSet = hasToSetWith defaultConfig