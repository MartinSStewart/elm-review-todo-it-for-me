module NoDebugTest exposing (all)

import NoDebug.TodoItForMe
import Review.Test exposing (ReviewResult)
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.runWithProjectData Review.Test.Dependencies.projectWithElmCore (NoDebug.TodoItForMe.rule False [])


todoMessage : String
todoMessage =
    "Remove the use of `Debug.todo` before shipping to production"


todoDetails : List String
todoDetails =
    [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ]


all : Test
all =
    describe "NoDebug.TodoItForMe"
        [ test "should not report normal function calls" <|
            \() ->
                testRule """
a = foo n
b = bar.foo n
c = debug
c = toString
c = List.toString
d = debug.todo n
e = debug.toString n
            """
                    |> Review.Test.expectNoErrors
        , test "should not report Debug.log calls" <|
            \() ->
                testRule """
a = Debug.log n
"""
                    |> Review.Test.expectNoErrors
        , test "should report Debug.todo use" <|
            \() ->
                testRule "a = Debug.todo"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = todoMessage
                            , details = todoDetails
                            , under = "Debug.todo"
                            }
                        ]
        , test "should not report calls from a module containing Debug but that is not Debug" <|
            \() ->
                testRule """
a = Foo.Debug.todo 1
b = Debug.Foo.todo 1
a = Foo.Debug.toString 1
b = Debug.Foo.toString 1
            """
                    |> Review.Test.expectNoErrors
        , test "should not report the import of the Debug module" <|
            \() ->
                testRule "import Debug"
                    |> Review.Test.expectNoErrors
        , test "should report the use of `todo` when `todo` has been explicitly imported" <|
            \() ->
                testRule """
import Debug exposing (todo)
a = todo ""
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = todoMessage
                            , details = todoDetails
                            , under = "todo"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 9 } }
                        ]
        , test "should report the use of `todo` when `todo` has been implicitly imported" <|
            \() ->
                testRule """
import Debug exposing (..)
a = todo "" 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = todoMessage
                            , details = todoDetails
                            , under = "todo"
                            }
                        ]
        , test "should not report the use of `todo` when it has not been imported" <|
            \() ->
                testRule """
import Debug exposing (log)
a = todo "" 1
"""
                    |> Review.Test.expectNoErrors
        ]
