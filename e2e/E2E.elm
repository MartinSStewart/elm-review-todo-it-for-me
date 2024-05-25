module E2E exposing (suite)

-- Please edit this only in the e2e directory. The point of this is that the code is generated.

import Csv.Decode
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Serialize exposing (Codec)
import Standard exposing (..)
import Test exposing (Test)


decoder : Decoder (A Int)
decoder =
    Debug.todo ""


encode : A Int -> Value
encode =
    Debug.todo ""


serialize : Codec e (A Int)
serialize =
    Debug.todo ""


fuzz : Fuzzer (A Int)
fuzz =
    Debug.todo ""


type Foo
    = X
    | Y
    | Z


all : List Foo
all =
    Debug.todo ""


toString : Foo -> String
toString =
    Debug.todo ""


fromString : String -> Maybe Foo
fromString =
    Debug.todo ""


type alias FlatData =
    { a : Int
    , b : String
    , c : Bool
    , d : Maybe Float
    , f : Foo
    }


csvDecoder : Csv.Decode.Decoder FlatData
csvDecoder =
    Debug.todo ""


random : Random.Generator FlatData
random =
    Debug.todo ""


suite : Test
suite =
    Test.describe "E2E test"
        [ Test.fuzz fuzz "JSON encoding roundtrips on fuzzed value" <|
            \val ->
                encode val
                    |> Json.Decode.decodeValue decoder
                    |> Expect.equal (Ok val)
        , List.map
            (\foo ->
                Test.test (toString foo ++ " roundtrips from string") <|
                    \() ->
                        toString foo
                            |> fromString
                            |> Expect.equal (Just foo)
            )
            all
            |> Test.describe "toString/fromString"
        , Test.fuzz fuzz "serialize roundtrips on fuzzed value" <|
            \val ->
                Serialize.encodeToBytes serialize val
                    |> Serialize.decodeFromBytes serialize
                    |> Expect.equal (Ok val)
        , Test.test "CSV decoding works" <|
            \() ->
                """a,b,c_tag,d,f_tag
1,foo,True,1.0,X
2,bar,False,,Y
3,baz,True,3.0,Z"""
                    |> Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow csvDecoder
                    |> Result.mapError Csv.Decode.errorToString
                    |> Expect.equal
                        (Ok
                            [ { a = 1, b = "foo", c = True, d = Just 1.0, f = X }
                            , { a = 2, b = "bar", c = False, d = Nothing, f = Y }
                            , { a = 3, b = "baz", c = True, d = Just 3.0, f = Z }
                            ]
                        )
        , Test.test "Random generates a random-ish value" <|
            \() ->
                Random.initialSeed 42
                    |> Random.step random
                    |> Tuple.first
                    |> Expect.equal
                        { a = -848567307, b = "TODO: Define string options", c = False, d = Nothing, f = X }
        ]
