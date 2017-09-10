module Types

public export
data MalType = MalList (List MalType)
             | MalNumber Int
             | MalSymbol String

