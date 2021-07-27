import           Control.Monad   (when)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



tmIdBool :: Term
tmIdBool = TmAbs "x" TyBool (TmVar "x")

tmBoolToInt :: Term
tmBoolToInt = TmAbs "f" (TyFun TyBool TyInt) (TmVar "f")

tmIdInt :: Term
tmIdInt = TmAbs "x" TyInt (TmVar "x")

data TypeError
    = UnboundVariable String
    | AdditionNonInteger Term Type
    | NonBooleanCondition Term Type
    | ArmsOfDifferentType Term Type Term Type
    | ApplicationWrongArgumentType Term Type Term Type
    | ApplicationNotFunction Term Type
    deriving (Show)


data Type
    = TyFun Type Type
    | TyBool
    | TyInt
    deriving(Show, Eq)

data Term
    = TmTrue
    | TmFalse
    | TmInt Integer
    | TmVar String
    | TmAbs String Type Term
    | TmApp Term Term
    | TmAdd Term Term
    | TmIf Term Term Term
    deriving(Show, Eq)

type Context = Map String Type

typeOf :: Context -> Term -> Either TypeError Type
typeOf ctx TmTrue    = Right TyBool
typeOf ctx TmFalse   = Right TyBool
typeOf ctx (TmInt n) = Right TyInt
typeOf ctx (TmVar s)
    = case Map.lookup s ctx of
           Nothing -> Left $ UnboundVariable s
           Just ty -> Right ty

typeOf ctx (TmAbs x ty t) =
    let ctx' = Map.insert x ty ctx
        ty' = typeOf ctx' t in TyFun ty <$> ty'

-- case typeOf ctx' ty of
  -- Left e    -> Left e
  -- Right ty' -> Right (TyFun ty ty')

typeOf ctx (TmApp t1 t2) = do
    ty1 <- typeOf ctx t1
    ty2 <- typeOf ctx t2
    case ty1 of
        TyFun ty11 ty12 ->
            if ty2 == ty11
               then Right ty12
            else Left $ ApplicationWrongArgumentType t1 ty1 t2 ty2

typeOf ctx (TmAdd t1 t2) = do
    ty1 <- typeOf ctx t1
    when (ty1 /= TyInt) $
        Left $ AdditionNonInteger t1 ty1
    ty2 <- typeOf ctx t2
    when (ty2 /= TyInt) $
        Left $ AdditionNonInteger t2 ty2
    Right TyInt

typeOf ctx (TmIf t1 t2 t3) = do
    ty1 <- typeOf ctx t1
    when (ty1 /= TyBool) $
        Left $ NonBooleanCondition t1 ty1
    ty2 <- typeOf ctx t2
    ty3 <- typeOf ctx t3
    when (ty2 /= ty3) $
        Left $ ArmsOfDifferentType t2 ty2 t3 ty3
    Right ty2

-- testSpec :: Spec
-- testSpec = do
--     it "Bool -> Bool" $ typeOf Map.empty tmIdBool `shouldBe` (Right (TyFun TyBool TyBool))
