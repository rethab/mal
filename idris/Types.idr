module Types


mutual

  public export
  data MalType = MalList (List MalType)
              | MalNumber Int
              | MalSymbol String
              | MalFunction Function

  public export
  Function : Type
  Function = List MalType -> Either String MalType

export
data Env = MkEnv (List (String, MalType))

export
symLookup : Env -> String -> Maybe MalType
symLookup (MkEnv xs) y = lookup y xs

sum : Function
sum ((MalNumber x) :: (MalNumber y) :: []) = Right (MalNumber (x + y))
sum _ = Left "+ must have two number params"

product : Function
product ((MalNumber x) :: (MalNumber y) :: []) = Right (MalNumber (x * y))
product _ = Left "* must have two number params"

diff : Function
diff ((MalNumber x) :: (MalNumber y) :: []) = Right (MalNumber (x - y))
diff _ = Left "- must have two number params"

div : Function
div ((MalNumber x) :: (MalNumber y) :: []) = Right (MalNumber (x `div` y))
div _ = Left "/ must have two number params"

export
empty : Env
empty = MkEnv [
    ("+", MalFunction sum)
  , ("-", MalFunction diff)
  , ("*", MalFunction product)
  , ("/", MalFunction div)
]
