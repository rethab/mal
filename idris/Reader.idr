module Reader

import Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

mutual 

  commas : Parser ()
  commas = skip (many (char ',')) <?> "comma"

  ignores : Parser ()
  ignores = spaces <|> commas

  -- the ! makes it commit to this branch
  malList : Parser (List MalType)
  malList =
    char '(' *!> (malType `sepBy` (char ' ')) <* ignores <* char ')'
    <?> "List"

  malNumber : Parser Int
  malNumber = integer <* commas <?> "Number" 

  malSymbol : Parser String
  malSymbol =
    map pack (some $ noneOf " []{}()'`~^@")
    <?> "Symbol"

  malType : Parser MalType
  malType = ignores
        *>  (map MalNumber malNumber)
       <|>| (map MalList malList) -- recursive definitions must be lazy
       <|>  (map MalSymbol malSymbol) 

export
read_str : String -> Either String MalType
read_str str = parse malType str
