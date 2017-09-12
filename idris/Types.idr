module Types

mutual

  public export
  data MalType = MalList (List MalType)
               | MalVector (List MalType)
               | MalMap (List (MapKey, MalType))
               | MalNumber Int
               | MalSymbol String
               | MalString String
               | MalNil
               | MalBool Bool
               | MalKeyword String
               | MalFunction String Function

  public export
  Function : Type
  Function = List MalType -> Either String MalType

  public export
  data MapKey = StringKey String | KeywordKey String

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
    ("+", MalFunction "+" sum)
  , ("-", MalFunction "-" diff)
  , ("*", MalFunction "*" product)
  , ("/", MalFunction "/" div)
]
