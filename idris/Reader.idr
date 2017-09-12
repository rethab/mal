module Reader

import Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

-- Based on: https://github.com/ziman/lightyear/blob/0bd3890646d9da88f92873fd860f88368e1fba0d/examples/Json.idr

specialChars : String
specialChars = " []{}()'`~^@"

specialChar : Parser Char
specialChar = do
  c <- anyChar
  case c of
    '"'  => pure '"'
    '\\' => pure '\\'
    'n'  => pure '\n'
    _ => fail "expected special char"

commas : Parser ()
commas = skip (many (char ',')) <?> "comma"

ignores : Parser ()
ignores = spaces <|> commas

malNumber : Parser MalType
malNumber = MalNumber <$> (integer <* commas <?> "Number")

malNil : Parser MalType
malNil = (\_ => MalNil) <$> (string "nil" <?> "nil")

malBool : Parser MalType
malBool = MalBool <$>
      (((\_ => True) <$> string "true")
  <|> ((\_ => False) <$> string "false")
  <?> "Bool")

malString' : Parser (List Char)
malString' = (char '"' *!> pure Prelude.List.Nil) <|> do
  c <- satisfy (/= '"')
  if c == '\\'
    then map (::) specialChar <*> malString'
    else map (c ::) malString'

malString : Parser String
malString = char '"' *!> map pack malString' <?> "String"


malSymbol : Parser MalType
malSymbol = MalSymbol <$>
  (map pack (some $ noneOf specialChars) <?> "Symbol")

malKeyword : Parser String
malKeyword = 
  char ':' *!> map pack (some $ noneOf specialChars) <?> "Keyword"

mutual 

  -- the ! makes it commit to this branch
  malList : Parser MalType
  malList = MalList <$> (
    char '(' *!> (malType `sepBy` (char ' ')) <* ignores <* char ')'
    <?> "List")

  malVector : Parser MalType
  malVector = MalVector <$> (
    char '[' *!> (malType `sepBy` (char ' ')) <* ignores <* char ']'
    <?> "Vector")

  malMap' : Parser (MapKey, MalType)
  malMap' = do _ <- spaces
               key <- (KeywordKey <$> malKeyword) <|>
                      (StringKey <$> malString)
               _ <- spaces
               val <- malType
               pure (key, val)

  malMap : Parser MalType
  malMap = MalMap <$>
    (char '{' *!> (malMap' `sepBy` (char ' ')) <* ignores <* char '}'
    <?> "HashMap")

  malType : Parser MalType
  malType = ignores
        *>  malNumber
       <|>  (MalKeyword <$> malKeyword)
       <|>  malNil
       <|>  malBool
       <|>  (MalString <$> malString)
       <|>| malList -- recursive definitions must be lazy
       <|>| malVector
       <|>| malMap
       <|>  malSymbol 

export
read_str : String -> Either String MalType
read_str str = parse malType str
