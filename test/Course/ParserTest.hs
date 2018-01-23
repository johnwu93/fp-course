{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ParserTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Course.Applicative ((*>))
import Course.Core
import Course.List (List(..))
import Course.Parser
       (ParseResult(..), Parser(..), (>>>), (|||), ageParser, bindParser,
        character, constantParser, digit, firstNameParser, is,
        isErrorResult, list, list1, mapParser, parse, personParser,
        phoneBodyParser, phoneParser, sequenceParser, surnameParser,
        thisMany, upper, valueParser)
import Course.Person
       (Person, Person(..), age, firstName, phone, smoker, surname)

test_Parser :: TestTree
test_Parser =
  testGroup
    "Parser"
    [ valueParserTest
    , characterTest
    , mapParserTest
    , bindParserTest
    , bindParser2Test
    , listTest
    , list1Test
    , alternativeTest
    , sequenceTest
    , thisManyTest
    , ageParserTest
    , firstNameParserTest
    , surnameParserTest
    , phoneBodyParserTest
    , phoneParserTest
    ]

failParser :: Parser Int
failParser = P (const UnexpectedEof)

checkError :: ParseResult a -> IO ()
checkError result = isErrorResult result @?= True

valueParserTest :: TestTree
valueParserTest =
  let input = "abc"
  in testCase "valueParser" $ parse (valueParser 3) input @?= Result input 3

characterTest :: TestTree
characterTest =
  testGroup
    "character"
    [ testCase "nonempty input" $ parse character "abc" @?= Result "bc" 'a'
    , testCase "empty input" $ checkError $ parse character ""
    ]

mapParserTest :: TestTree
mapParserTest =
  testGroup
    "mapParser"
    [ testCase "basic input" $ parse (mapParser succ character) "amz" @?= Result "mz" 'b'
    , testCase "empty input" $ parse (mapParser (+ 10) (valueParser 7)) "" @?= Result "" 17
    , testCase "error" $ checkError (parse (mapParser (+ 10) failParser) "hi")
    ]

bindParserTest :: TestTree
bindParserTest =
  testGroup
    "bindParser"
    [ testCase "abc" $ parse (bindParser f character) "abc" @?= Result "bc" 'v'
    , testCase "a" $ parse (bindParser f character) "a" @?= Result "" 'v'
    , testCase "xabc" $ parse (bindParser f character) "xabc" @?= Result "bc" 'a'
    , testCase "parser has an error and will create an error" $
      isErrorResult (parse (bindParser f character) "") @?= True
    , testCase "leads to input error" $ checkError $ parse (bindParser f character) "x"
    ]
  where
    f char =
      if char == 'x'
        then character
        else valueParser 'v'

bindParser2Test :: TestTree
bindParser2Test =
  testGroup
    ">>>"
    [ testCase "basic" $ parse (character >>> valueParser 'v') "abc" @?= Result "bc" 'v'
    , testCase "error" $ checkError $ parse (character >>> valueParser 'v') ""
    ]

listTest :: TestTree
listTest =
  testGroup
    "list"
    [ testCase "parse eof of characters" $ parse (list character) "" @?= Result "" ""
    , testCase "parse set of digits" $ parse (list digit) "123abc" @?= Result "abc" "123"
    , testCase "tries to digits but cannot and the state is the same" $ parse (list digit) "abc" @?= Result "abc" ""
    , testCase "parse characters until there are no more " $ parse (list character) "abc" @?= Result "" "abc"
    , testCase "parse characters and treat them as v" $
      parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
    , testCase "parse eof of characters but no v" $ parse (list (character *> valueParser 'v')) "" @?= Result "" ""
    ]

list1Test :: TestTree
list1Test =
  testGroup
    "list1"
    [ testCase "parse all characters1" $ parse (list1 character) "abc" @?= Result "" "abc"
    , testCase "parse all characters2" $ parse (list1 character) "abc" @?= Result "" "abc"
    , testCase "empty string" $ checkError $ parse (list1 (character *> valueParser 'v')) ""
    , testCase "could not find a match" $ checkError $ parse (list1 $ is 'm') "1234"
    ]

alternativeTest :: TestTree
alternativeTest =
  testGroup
    "|||"
    [ testCase "encounters an error but finds alternative" $ parse (character ||| valueParser 'v') "" @?= Result "" 'v'
    , testCase "error but finds alternative" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?= Result "" 'v'
    , testCase "error but finds alternative" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?= Result "" 'v'
    ]

sequenceTest :: TestTree
sequenceTest =
  testGroup
    "sequence"
    [ testCase "success" $ parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?= Result "def" "axC"
    , testCase "failure" $ checkError $ parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef"
    ]

thisManyTest :: TestTree
thisManyTest =
  testGroup
    "thisMany"
    [ testCase "simple" $ parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
    , testCase "error" $ checkError $ parse (thisMany 4 upper) "ABcDef"
    ]

ageParserTest :: TestTree
ageParserTest =
  testGroup
    "ageParser"
    [ testCase "normal age" $ parse ageParser "98" @?= Result "" 98
    , testCase "leads with 0" $ checkError $ parse ageParser "098"
    , testCase "alpha characters" $ checkError $ parse ageParser "abc"
    , testCase "negative numbers" $ checkError $ parse ageParser "-120"
    ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup
    "firstNameParser"
    [ testCase "simple" $ parse firstNameParser "Abc" @?= Result "" "Abc"
    , testCase "fail" $ checkError $ parse firstNameParser "abc"
    ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup
    "surnameParser"
    [ testCase "simple" $ parse surnameParser "Abcdef" @?= Result "" "Abcdef"
    , testCase "long surname" $
      parse surnameParser "Abcdefghijklmnopqrstuvwxyz" @?= Result "" "Abcdefghijklmnopqrstuvwxyz"
    , testCase "short surname" $ checkError $ parse surnameParser "Abc"
    , testCase "no leading capitalization" $ checkError $ parse surnameParser "abc"
    ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup
    "phoneBodyParser"
    [ testCase "only numbers and hyphens" $ parse phoneBodyParser "123-456" @?= Result "" "123-456"
    , testCase "contains alphacharacters" $ parse phoneBodyParser "123-4a56" @?= Result "a56" "123-4"
    , testCase "leading alphacharacter" $ parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
    ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup
    "phoneParser"
    [ testCase "basic" $ parse phoneParser "123-456#" @?= Result "" "123-456"
    , testCase "has extra characters" $ parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
    , testCase "has no ending hash tag" $ checkError $ parse phoneParser "123-456"
    , testCase "has beginning character" $ checkError $ parse phoneParser "a123-456#"
    ]

personParserTest :: TestTree
personParserTest =
  testGroup
    "personParser"
    [ testCase "empty" $ checkError $ parse personParser ""
    , testCase "invalid age" $ checkError $ parse personParser "12x Fred Clarkson y 123-456.789#"
    , testCase "invalid firstName" $ checkError $ parse personParser "123 fred Clarkson y 123-456.789#"
    , testCase "invalid surname" $ checkError $ parse personParser "123 Fred Cla y 123-456.789#"
    , testCase "valid and terminates" $
      parse personParser "123 Fred Clarkson y 123-456.789#" @?=
      Result "" Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
    , testCase "valid with extra characters" $
      parse personParser "123 Fred Clarkson y 123-456.789# rest" @?=
      Result " rest" Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
    ]
