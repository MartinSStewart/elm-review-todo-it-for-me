module Internal.Builtin.Random exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


random =
    { int = \min max -> CG.apply [ CG.fqFun [ "Random" ] "int", min, max ]
    , float = \min max -> CG.apply [ CG.fqFun [ "Random" ] "float", min, max ]
    , minInt = CG.fqVal [ "Random" ] "minInt"
    , maxInt = CG.fqVal [ "Random" ] "maxInt"
    , uniform = \fst rest -> CG.apply [ CG.fqFun [ "Random" ] "uniform", fst, rest ]
    , map = \fn arg -> CG.apply [ CG.fqFun [ "Random" ] "map", fn, arg ]
    , map2 = \fn arg1 arg2 -> CG.apply [ CG.fqFun [ "Random" ] "map2", fn, arg1, arg2 ]
    , constant = \arg -> CG.apply [ CG.fqFun [ "Random" ] "constant", arg ]
    , andThen = \arg -> CG.apply [ CG.fqFun [ "Random" ] "andThen", arg ]
    , list = \child -> CG.apply [ CG.fqFun [ "Random" ] "list", CG.int 3, child ]
    , lazy = \fn -> CG.apply [ CG.fqFun [ "Random" ] "lazy", fn ]
    }


randomExtra =
    { anyInt = CG.fqVal [ "Random", "Int" ] "anyInt"
    , andMap = \arg -> CG.apply [ CG.fqVal [ "Random", "Extra" ] "andMap", arg ]
    , choices = \h t -> CG.apply [ CG.fqVal [ "Random", "Extra" ] "choices", h, t ]
    }


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define
        { id = "elm/random/Random.Generator"
        , dependency = "elm/random"
        , typePattern = Typed [ "Random" ] "Generator" [ Target ]
        , makeName = \name -> "random" ++ name
        }
        [ CodeGenerator.int (random.int random.minInt random.maxInt)
        , CodeGenerator.int randomExtra.anyInt |> CodeGenerator.ifUserHasDependency "elm-community/random-extra"
        , CodeGenerator.float (random.float (CG.float 0) (CG.float 1))
        , CodeGenerator.string (random.uniform (CG.string "TODO: Define string options") (CG.list []))
        , CodeGenerator.list random.list
        , CodeGenerator.pipeline random.constant (\arg -> CG.apply [ CG.fqFun [ "Random" ] "map2", CG.parens (CG.binOp CG.piper), arg ])
        , CodeGenerator.pipeline random.constant randomExtra.andMap |> CodeGenerator.ifUserHasDependency "elm-community/random-extra"
        , CodeGenerator.mapN 5 (\name ctor args -> CG.apply ([ CG.fqFun [ "Random" ] name, ctor ] ++ args))
        , CodeGenerator.map random.map
        , CodeGenerator.succeed random.constant
        , CodeGenerator.customType
            (\_ variants ->
                case variants of
                    [] ->
                        CG.val "never"

                    [ ( _, single ) ] ->
                        single

                    ( _, h ) :: t ->
                        CG.pipe (random.uniform h (CG.list (List.map Tuple.second t))) [ random.andThen (CG.val "identity") ]
            )
        , CodeGenerator.customType
            (\_ variants ->
                case variants of
                    [] ->
                        CG.val "never"

                    ( _, h ) :: t ->
                        randomExtra.choices h (CG.list (List.map Tuple.second t))
            )
            |> CodeGenerator.ifUserHasDependency "elm-community/random-extra"

        -- Random has a weird way to do custom types, which makes the compositional stuff unnatural looking for enums
        , CodeGenerator.custom
            (\t ->
                case t of
                    ResolvedType.CustomType _ [] [ ( ref, [] ) ] ->
                        Just (random.constant (CG.fqVal ref.modulePath ref.name))

                    ResolvedType.CustomType _ [] (( head, [] ) :: ctors) ->
                        if List.all (\( _, args ) -> List.isEmpty args) ctors then
                            Just (random.uniform (CG.fqVal head.modulePath head.name) (List.map (\( ctor, _ ) -> CG.fqVal ctor.modulePath ctor.name) ctors |> CG.list))

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        , CodeGenerator.lambdaBreaker
            (\expr ->
                random.lazy (CG.lambda [ CG.unitPattern ] expr)
            )
        , CodeGenerator.maybe
            (\justVariant ->
                CG.pipe (random.uniform (random.map (CG.fun "Just") justVariant) (CG.list [ random.constant (CG.val "Nothing") ])) [ random.andThen (CG.val "identity") ]
            )
        ]
