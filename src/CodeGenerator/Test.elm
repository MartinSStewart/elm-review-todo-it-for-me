module CodeGenerator.Test exposing (codeGenTest, codeGenTestFailsWith, codeGenIncrementalTest, fakeDependency, FakeDependency)

{-| Testing code generators can be tricky, but very rewarding as it makes developing CodeGenerators much easier.

@docs codeGenTest, codeGenTestFailsWith, codeGenIncrementalTest, fakeDependency, FakeDependency

-}

import Array
import CodeGenerator exposing (CodeGenerator)
import Elm.Constraint
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Parser
import Elm.Processing
import Elm.Project
import Elm.RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Type
import Elm.Version
import Expect exposing (Expectation)
import Json.Decode
import NoDebug.TodoItForMe
import Review.Project
import Review.Project.Dependency exposing (Dependency)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test)


{-| Represents a dependency that the user has in their project.
-}
type FakeDependency
    = Dep Dependency
    | FailedDep String


codeGenTestHelper : String -> List FakeDependency -> List CodeGenerator -> List String -> Bool -> ({ module_ : String, under : String } -> Review.Test.ReviewResult -> Expectation) -> Test
codeGenTestHelper description dependencies codeGens modules incremental fn =
    Test.test description <|
        \_ ->
            let
                inputModules =
                    List.map (String.replace "\u{000D}" "") modules

                validDependencies =
                    List.filterMap
                        (\fakeDep ->
                            case fakeDep of
                                Dep dep ->
                                    Just dep

                                FailedDep _ ->
                                    Nothing
                        )
                        dependencies

                failedDeps =
                    List.filterMap
                        (\fakeDep ->
                            case fakeDep of
                                Dep _ ->
                                    Nothing

                                FailedDep reason ->
                                    Just (" - " ++ reason)
                        )
                        dependencies
            in
            case findTodo inputModules of
                Ok result ->
                    if List.isEmpty failedDeps then
                        let
                            project =
                                List.foldl Review.Project.addDependency Review.Test.Dependencies.projectWithElmCore validDependencies
                        in
                        fn result (Review.Test.runOnModulesWithProjectData project (NoDebug.TodoItForMe.rule incremental codeGens) inputModules)

                    else
                        Expect.fail ("Found issues in the following dependencies: \n\n" ++ String.join "\n" failedDeps)

                Err msg ->
                    Expect.fail msg


{-| Like `codeGenTest`, but expects the code generator to not be able to generate code. The final string is the expected error message.
-}
codeGenTestFailsWith : String -> List FakeDependency -> List CodeGenerator -> List String -> String -> Test
codeGenTestFailsWith description dependencies codeGens modules expectedFailureDetails =
    codeGenTestHelper description
        dependencies
        codeGens
        modules
        False
        (\result ->
            Review.Test.expectErrorsForModules
                [ ( result.module_
                  , [ Review.Test.error
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production.", expectedFailureDetails ]
                        , under = result.under
                        }
                    ]
                  )
                ]
        )


{-| Tests that a single `Debug.todo` gets replaced with a particular piece of code.

    codeGenTest "Generates a generator for a int"
        [ fakeDependency "elm/random" ]
        [ elmRandomCodeGeneratorUnderTest ]
        [ """module A exposing (..)
    import Random

    generator : Random.Generator Int
    generator =
        Debug.todo ""
    """ ]
        """module A exposing (..)
    import Random

    generator : Random.Generator Int
    generator =
        Random.int Random.minInt Random.maxInt
    """

The arguments are:

1.  Description (i.e. what you would normally pass to `Test.test`)
2.  A list of dependencies. Note that this rule will only activate code generators based on dependencies in the user's project. This allows you to control which dependencies are present.
3.  A list of code generators. Typically this will be the code generator under test, but can also be used to test interactions between multiple.
4.  A list of code modules that form the project the rule is being run on. Exactly one of these must have a single top level `Debug.todo` that the test is trying to replace.
5.  The expected source code of the module containing the `Debug.todo` after running the code generator.

-}
codeGenTest : String -> List FakeDependency -> List CodeGenerator -> List String -> String -> Test
codeGenTest description dependencies codeGens modules expected =
    codeGenTestHelper description
        dependencies
        codeGens
        modules
        False
        (\result ->
            Review.Test.expectErrorsForModules
                [ ( result.module_
                  , [ Review.Test.error
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                        , under = result.under
                        }
                        |> Review.Test.whenFixed (String.replace "\u{000D}" "" expected)
                    ]
                  )
                ]
        )


{-| Like `codeGenTest`, but runs in incremental mode. Only looks for a fix for the first `Debug.todo` found.
-}
codeGenIncrementalTest : String -> List FakeDependency -> List CodeGenerator -> List String -> String -> Test
codeGenIncrementalTest description dependencies codeGens modules expected =
    codeGenTestHelper description
        dependencies
        codeGens
        modules
        True
        (\result ->
            Review.Test.expectErrorsForModules
                [ ( result.module_
                  , [ Review.Test.error
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details = [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                        , under = result.under
                        }
                        |> Review.Test.whenFixed (String.replace "\u{000D}" "" expected)
                    ]
                  )
                ]
        )


extractSubstring : Range -> String -> String
extractSubstring { start, end } file =
    case
        String.lines file
            |> Array.fromList
            |> Array.slice (start.row - 1) end.row
            |> Array.toList
    of
        [] ->
            ""

        fst :: rest ->
            case List.reverse (String.dropLeft (start.column - 1) fst :: rest) of
                [] ->
                    ""

                last :: head ->
                    (String.dropRight (String.length last - end.column) last :: head)
                        |> List.reverse
                        |> String.join "\n"


listMaybe2MaybeList : List (Maybe a) -> Maybe (List a)
listMaybe2MaybeList =
    List.foldr (Maybe.map2 (::)) (Just [])


elmConstraint : Elm.Constraint.Constraint
elmConstraint =
    let
        c () =
            case Elm.Constraint.fromString "0.19.0 <= v < 0.20.0" of
                Just cons ->
                    cons

                Nothing ->
                    c ()
    in
    c ()


typeFromStr : String -> Maybe Elm.Type.Type
typeFromStr str =
    Json.Decode.decodeString Elm.Type.decoder ("\"" ++ str ++ "\"") |> Result.toMaybe


{-| Creates a fake elm review dependency type. This should only be used with the other helpers in this module, as the information inside the returned type is mostly rubbish.

How do you get this value without too much pain? I usually go to package.elm-lang.or, find the package I'm interested in, open the dev tools, in the Network tab find the `docs.json`,
select Preview, right-click and "Store as global variable", then run the following snippet:

    copy(
        `fakeDependency \n    { name = ${JSON.stringify(
            window.location.pathname.split("/").slice(2, 4).join("/")
        )}\n    , dependencies = []\n    , modules =\n        [ ${temp1
            .map(
                (mod) =>
                    `{ name = ${JSON.stringify(mod.name)}\n        , values =\n            [ ${mod.values
                        .map((v) => `( ${JSON.stringify(v.name)}, ${JSON.stringify(v.type)})`)
                        .join("\n            , ")}\n            ]\n        }`
            )
            .join("\n        , ")}\n    ]\n}`
    );

-}
fakeDependency :
    { name : String
    , dependencies : List String
    , modules : List { name : String, values : List ( String, String ) }
    }
    -> FakeDependency
fakeDependency data =
    case
        ( Elm.Package.fromString data.name
        , listMaybe2MaybeList (List.map (.name >> Elm.Module.fromString) data.modules)
        , listMaybe2MaybeList (List.map (\d -> Maybe.map2 Tuple.pair (Elm.Package.fromString d) (Elm.Constraint.fromString "1.0.0 <= v < 2.0.0")) data.dependencies)
        )
    of
        ( Just name, Just moduleNames, Just constraints ) ->
            case
                List.map
                    (\mod ->
                        Maybe.map
                            (\values ->
                                { name = mod.name
                                , comment = ""
                                , unions = []
                                , aliases = []
                                , binops = []
                                , values = values
                                }
                            )
                            (List.map
                                (\( vn, vts ) ->
                                    Maybe.map
                                        (\t ->
                                            { name = vn
                                            , comment = ""
                                            , tipe = t
                                            }
                                        )
                                        (typeFromStr vts)
                                )
                                mod.values
                                |> listMaybe2MaybeList
                            )
                    )
                    data.modules
                    |> listMaybe2MaybeList
            of
                Just modules ->
                    Dep
                        (Review.Project.Dependency.create data.name
                            (Elm.Project.Package
                                { name = name
                                , summary = "A package"
                                , license = Elm.License.bsd3
                                , version = Elm.Version.one
                                , exposed = Elm.Project.ExposedList moduleNames
                                , deps = constraints
                                , testDeps = []
                                , elm = elmConstraint
                                }
                            )
                            modules
                        )

                Nothing ->
                    FailedDep ("Modules of " ++ data.name ++ " are not all valid")

        ( Nothing, _, _ ) ->
            FailedDep ("Name of " ++ data.name ++ " is not a valid elm package name")

        ( _, Nothing, _ ) ->
            FailedDep ("Dependencies of " ++ data.name ++ " are not all valid package names")

        ( _, _, Nothing ) ->
            FailedDep ("Name of modules in " ++ data.name ++ " is not a valid elm module names")


findTodo : List String -> Result String { module_ : String, under : String }
findTodo modules =
    case modules of
        [] ->
            Err "You need to pass in a module that actually contains a Debug.todo as a top-level value."

        current :: rest ->
            case Elm.Parser.parse current of
                Ok rawFile ->
                    case
                        rawFile
                            |> Elm.Processing.process Elm.Processing.init
                            |> .declarations
                            |> List.filterMap
                                (\node ->
                                    case Elm.Syntax.Node.value node of
                                        FunctionDeclaration funDecl ->
                                            case (Elm.Syntax.Node.value funDecl.declaration).expression of
                                                Node _ (Elm.Syntax.Expression.Application ((Node _ (Elm.Syntax.Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
                                                    Just
                                                        { module_ = Elm.RawFile.moduleName rawFile |> String.join "."
                                                        , under = extractSubstring (Elm.Syntax.Node.range node) current
                                                        }

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                            |> List.head
                    of
                        Just res ->
                            Ok res

                        Nothing ->
                            findTodo rest

                Err _ ->
                    Err ("Failed to parse the following file:\n\n```" ++ current ++ "\n```")
