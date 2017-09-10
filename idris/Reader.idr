module Reader

import Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

-- Based on: https://github.com/ziman/lightyear/blob/0bd3890646d9da88f92873fd860f88368e1fba0d/examples/Json.idr

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

malNumber : Parser Int
malNumber = integer <* commas <?> "Number" 

malNil : Parser String
malNil = string "nil" <?> "nil"

malBool : Parser Bool
malBool = ((\_ => True)  <$> string "true")
      <|> ((\_ => False) <$> string "false")
      <?> "Bool"

malString' : Parser (List Char)
malString' = (char '"' *!> pure Prelude.List.Nil) <|> do
  c <- satisfy (/= '"')
  if c == '\\'
    then map (::) specialChar <*> malString'
    else map (c ::) malString'

malString : Parser String
malString = char '"' *!> map pack malString' <?> "String"


malSymbol : Parser String
malSymbol =
  map pack (some $ noneOf " []{}()'`~^@")
  <?> "Symbol"

mutual 



  -- the ! makes it commit to this branch
  malList : Parser (List MalType)
  malList =
    char '(' *!> (malType `sepBy` (char ' ')) <* ignores <* char ')'
    <?> "List"


  malType : Parser MalType
  malType = ignores
        *>  (map MalNumber malNumber)
       <|>  ((\_ => MalNil) <$> malNil)
       <|>  (map MalBool malBool)
       <|>  (map MalString malString)
       <|>| (map MalList malList) -- recursive definitions must be lazy
       <|>  (map MalSymbol malSymbol) 

export
read_str : String -> Either String MalType
read_str str = parse malType str
