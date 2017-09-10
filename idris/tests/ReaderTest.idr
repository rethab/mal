module ReaderTest

import Types
import Reader
import Printer

import Lightyear.Testing

%access export

data Test : Type where
  MkTest : (inp: String) -> (exp: String) -> Test

runTest : Test -> String
runTest (MkTest inp exp) =
  case read_str inp of
    Left(err) => "Failed parsing on '" ++ inp ++ "': " ++ err
    Right(out) => 
      case pr_str out == exp of
        True  => "Success on '" ++ inp ++ "'"
        False => "Failed on '" ++ inp ++ "': got '" ++ pr_str out ++ "', but expected '" ++ exp ++ "'"

runTests : List Test -> IO ()
runTests = traverse_ (putStrLn . runTest)

readerTests : IO ()
readerTests = runTests
  [ MkTest "1" "1"
  , MkTest "7" "7"
  , MkTest "   7" "7"
  , MkTest "-123" "-123"
  , MkTest "+" "+"
  , MkTest "abc " "abc"
  , MkTest "   abc   " "abc"
  , MkTest "abc5 " "abc5"
  , MkTest "abc-def " "abc-def"
  , MkTest "(+ 1 2) " "(+ 1 2)"
  , MkTest "() " "()"
  , MkTest "(nil) " "(nil)"
  , MkTest "((3 4)) " "((3 4))"
  , MkTest "(+ 1 (+ 2 3)) " "(+ 1 (+ 2 3))"
  , MkTest "  ( +   1   (+   2 3   )   )  " "(+ 1 (+ 2 3))"
  , MkTest "(* 1 2) " "(* 1 2)"
  , MkTest "(** 1 2) " "(** 1 2)"
  , MkTest "(* -3 6) " "(* -3 6)"
  , MkTest "(1 2, 3,,,,),, " "(1 2 3)"
  ]

