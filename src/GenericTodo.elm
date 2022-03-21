module GenericTodo exposing (CodeGenerator(..), Condition(..), GenericTodo, ResolvedGeneric, Resolver, ResolverImpl(..), buildFullGeneric, declarationVisitorGetGenericTypes, getTodos, todoErrors)

import AssocList exposing (Dict)
import AssocSet as Set exposing (Set)
import CodeGen.Helpers as Helpers
import Dict
import Elm.CodeGen as CG exposing (signature)
import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Internal.ExistingImport exposing (ExistingImport)
import List.Extra
import Pretty
import ResolvedType exposing (Reference, ResolvedType)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Project exposing (dependencies)
import Review.Rule as Rule exposing (ModuleKey)
import TypePattern exposing (TypePattern)



-- Public types


type CodeGenerator
    = Generic
        { id : String
        , searchPattern : TypePattern
        , resolvers : List Resolver
        , condition : Condition
        , makeName : String -> String
        }
    | Amendment String (List Resolver)


type alias Resolver =
    { implementation : ResolverImpl
    , condition : Condition
    }


type ResolverImpl
    = PrimitiveResolver Reference (List ResolvedType -> List Expression -> Maybe Expression)
    | UniversalResolver (ResolvedType -> Maybe Expression)
    | Combiner (ResolvedType -> Expression -> List Expression -> Maybe Expression)
    | CustomTypeResolver (List ( Reference, List ResolvedType ) -> List ( String, Expression ) -> Expression)


type Condition
    = Always
    | Dependencies (List String)



-- Configuration stuff


evalCondition : List String -> Condition -> Bool
evalCondition dependencies cond =
    case cond of
        Always ->
            True

        Dependencies deps ->
            List.all (\dep -> List.member dep dependencies) deps


processResolvers : List String -> List Resolver -> List ResolverImpl
processResolvers dependencies resolvers =
    List.filterMap
        (\resolver ->
            if evalCondition dependencies resolver.condition then
                Just resolver.implementation

            else
                Nothing
        )
        resolvers


type alias ResolvedGeneric =
    { id : String
    , searchPattern : TypeAnnotation -> Maybe TypeAnnotation
    , resolvers : List ResolverImpl
    , makeName : String -> String
    , makeAnnotation : TypeAnnotation -> TypeAnnotation
    }


buildFullGeneric : List String -> List CodeGenerator -> List ResolvedGeneric
buildFullGeneric dependencies generics =
    List.foldr
        (\generic ( amendments, resolved ) ->
            case generic of
                Amendment id resolvers ->
                    ( Dict.update id (Maybe.map (\e -> Just (resolvers ++ e)) >> Maybe.withDefault (Just resolvers)) amendments, resolved )

                Generic gen ->
                    if evalCondition dependencies gen.condition then
                        ( Dict.remove gen.id amendments
                        , { id = gen.id
                          , searchPattern = TypePattern.matches gen.searchPattern
                          , resolvers = processResolvers dependencies (Dict.get gen.id amendments |> Maybe.withDefault []) ++ processResolvers dependencies gen.resolvers
                          , makeName = gen.makeName
                          , makeAnnotation = TypePattern.generate gen.searchPattern
                          }
                            :: resolved
                        )

                    else
                        ( Dict.remove gen.id amendments, resolved )
        )
        ( Dict.empty, [] )
        generics
        |> Tuple.second



-- Implementation


type alias GenericTodo =
    { genericId : String
    , functionName : String
    , childType : ResolvedType
    , range : Range
    , parameters : List (Node Pattern)
    , signature : Signature
    }


type alias GenericProvider =
    { genericId : String, moduleName : ModuleName, functionName : String, childType : ResolvedType }


type alias ProjectContext a =
    { a
        | types : List ( ModuleName, ResolvedType )
        , genericProviders : List GenericProvider
        , genericTodos : List ( ModuleName, GenericTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , moduleKeys : Dict ModuleName ModuleKey
        , generics : List ResolvedGeneric
    }


declarationVisitorGetGenericTypes :
    { a | generics : List ResolvedGeneric, lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> Function
    -> Maybe { genericId : String, functionName : String, childType : TypeAnnotation }
declarationVisitorGetGenericTypes context function =
    case ( function.signature, function.declaration ) of
        ( Just (Node _ signature), Node _ declaration ) ->
            findMatchingPattern context.generics (normalizeTypes context.lookupTable (Node.value signature.typeAnnotation))
                |> Maybe.andThen
                    (\( genericId, childType ) ->
                        if Helpers.hasDebugTodo declaration then
                            Nothing

                        else
                            Just
                                { functionName = Node.value signature.name
                                , childType = childType
                                , genericId = genericId
                                }
                    )

        _ ->
            Nothing


findMatchingPattern : List ResolvedGeneric -> TypeAnnotation -> Maybe ( String, TypeAnnotation )
findMatchingPattern generics annotation =
    case generics of
        h :: t ->
            case h.searchPattern annotation of
                Just childType ->
                    Just ( h.id, childType )

                Nothing ->
                    findMatchingPattern t annotation

        [] ->
            Nothing


normalizeReference : ModuleNameLookupTable.ModuleNameLookupTable -> Node ( List String, String ) -> Node ( List String, String )
normalizeReference table ((Node r ( _, valName )) as node) =
    case ModuleNameLookupTable.moduleNameFor table node of
        Just moduleName ->
            Node r ( moduleName, valName )

        Nothing ->
            node


normalizeTypes : ModuleNameLookupTable.ModuleNameLookupTable -> TypeAnnotation -> TypeAnnotation
normalizeTypes lookupTable signature =
    case signature of
        GenericType var ->
            GenericType var

        Unit ->
            Unit

        Typed t args ->
            Typed (normalizeReference lookupTable t) (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)

        Tupled args ->
            Tupled (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)

        Record def ->
            Record (List.map (\(Node r ( k, Node rv v )) -> Node r ( k, Node rv (normalizeTypes lookupTable v) )) def)

        GenericRecord var (Node defr def) ->
            GenericRecord var (Node defr (List.map (\(Node r ( k, Node rv v )) -> Node r ( k, Node rv (normalizeTypes lookupTable v) )) def))

        FunctionTypeAnnotation (Node lr lv) (Node rr rv) ->
            FunctionTypeAnnotation (Node lr (normalizeTypes lookupTable lv)) (Node rr (normalizeTypes lookupTable rv))


getTodos :
    { a | generics : List ResolvedGeneric, lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> List ResolvedType
    -> Range
    -> Function
    -> Maybe GenericTodo
getTodos context availableTypes declarationRange function =
    let
        declaration =
            Node.value function.declaration
    in
    case function.signature of
        Just (Node _ signature) ->
            findMatchingPattern context.generics (normalizeTypes context.lookupTable (Node.value signature.typeAnnotation))
                |> Maybe.andThen
                    (\( genericId, childType ) ->
                        if Helpers.hasDebugTodo declaration then
                            Just
                                { functionName = Node.value signature.name
                                , childType = ResolvedType.fromTypeSignature context.lookupTable availableTypes context.currentModule childType
                                , range = declarationRange
                                , parameters = declaration.arguments
                                , signature = signature
                                , genericId = genericId
                                }

                        else
                            Nothing
                    )

        Nothing ->
            Nothing


todoErrors : ProjectContext a -> ModuleName -> GenericTodo -> Maybe (Rule.Error { useErrorForModule : () })
todoErrors projectContext currentModule todo =
    case ( Helpers.find (\generic -> generic.id == todo.genericId) projectContext.generics, AssocList.get currentModule projectContext.moduleKeys, AssocList.get currentModule projectContext.imports ) of
        ( Just generic, Just moduleKey, Just imports ) ->
            case createFixes projectContext generic imports currentModule todo of
                Ok fixes ->
                    Rule.errorForModuleWithFix moduleKey
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details =
                            [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                            ]
                        }
                        todo.range
                        fixes
                        |> Just

                Err reason ->
                    Rule.errorForModule moduleKey
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details =
                            [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                            , reason
                            ]
                        }
                        todo.range
                        |> Just

        _ ->
            Nothing


createFixes : ProjectContext a -> ResolvedGeneric -> { newImportStartRow : Int, existingImports : List ExistingImport } -> ModuleName -> GenericTodo -> Result String (List Review.Fix.Fix)
createFixes projectContext generic imports currentModule todo =
    generate True generic imports.existingImports currentModule (List.filter (\provider -> provider.genericId == generic.id) projectContext.genericProviders) todo.childType
        |> Result.map
            (\( expr, declarations_ ) ->
                let
                    ( expression, newImports_ ) =
                        Helpers.fixNamesAndImportsInExpression (postprocessExpression expr) currentModule imports.existingImports

                    ( declarations, newImports ) =
                        List.foldr
                            (\decl ( soFar, imports_ ) ->
                                let
                                    ( newDecl, foundImports ) =
                                        Helpers.fixNamesAndImportsInFunctionDeclaration decl currentModule imports.existingImports
                                in
                                ( fixFunctionDeclaration newDecl :: soFar, foundImports ++ imports_ )
                            )
                            ( [], newImports_ )
                            declarations_
                in
                Review.Fix.replaceRangeBy todo.range
                    ({ documentation = Nothing
                     , signature = Helpers.node todo.signature |> Just
                     , declaration =
                        Helpers.node
                            { name = Helpers.node todo.functionName
                            , arguments = todo.parameters
                            , expression = Helpers.node expression
                            }
                     }
                        |> fixFunctionDeclaration
                        |> Helpers.writeDeclaration
                    )
                    :: List.indexedMap
                        (\index decl ->
                            Review.Fix.insertAt { column = 0, row = 99999 - index } (Helpers.writeDeclaration decl ++ "\n")
                        )
                        declarations
                    ++ (case
                            Helpers.importsFix
                                currentModule
                                imports.existingImports
                                imports.newImportStartRow
                                (Set.fromList newImports)
                        of
                            Just importsFix_ ->
                                [ importsFix_ ]

                            Nothing ->
                                []
                       )
            )


generate : Bool -> ResolvedGeneric -> List ExistingImport -> ModuleName -> List GenericProvider -> ResolvedType -> Result String ( Expression, List Function )
generate isTopLevel generic existingImports currentModule genericProviders type_ =
    case Helpers.find (\provider -> matchType provider.childType type_) genericProviders of
        Just provider ->
            Ok ( CG.fqFun provider.moduleName provider.functionName, [] )

        Nothing ->
            case type_ of
                ResolvedType.GenericType name ->
                    Err ("Could not generate definition for `" ++ name ++ "`. Generic types are not yet supported")

                ResolvedType.Opaque ref args ->
                    applyResolvers
                        (\resolver ->
                            case resolver of
                                PrimitiveResolver reference fn ->
                                    if reference == ref then
                                        List.foldr
                                            (\arg ->
                                                Result.map2
                                                    (\( argExpr, argDefs ) ( resExprs, resDefs ) ->
                                                        ( argExpr :: resExprs, argDefs ++ resDefs )
                                                    )
                                                    (generate False generic existingImports currentModule genericProviders arg)
                                            )
                                            (Ok ( [], [] ))
                                            args
                                            |> Result.map (Tuple.mapFirst (fn args))
                                            |> transmogrify

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                        ref.name
                        generic
                        type_

                ResolvedType.Function args result ->
                    Err "Could not generate definition for a function. Function types are not yet supported"

                ResolvedType.TypeAlias ref [] (ResolvedType.AnonymousRecord children) ->
                    applyCombiner type_ (ResolvedType.refToExpr currentModule existingImports ref) (List.map Tuple.second children) generic existingImports currentModule genericProviders
                        |> makeExternalDeclaration isTopLevel generic ref

                ResolvedType.TypeAlias ref [] childType ->
                    generate False generic existingImports currentModule genericProviders childType

                ResolvedType.TypeAlias _ generics _ ->
                    Err "Generics are not supported"

                ResolvedType.AnonymousRecord children ->
                    applyCombiner type_
                        (List.map (\( name, _ ) -> ( name, CG.val name )) children
                            |> CG.record
                            |> CG.lambda (List.map (\( name, _ ) -> CG.varPattern name) children)
                        )
                        (List.map Tuple.second children)
                        generic
                        existingImports
                        currentModule
                        genericProviders

                ResolvedType.Tuple args ->
                    case List.length args of
                        0 ->
                            generate False generic existingImports currentModule genericProviders (ResolvedType.Opaque { modulePath = [ "Basics" ], name = "()" } [])

                        2 ->
                            applyCombiner type_ (CG.fqFun [ "Tuple" ] "pair") args generic existingImports currentModule genericProviders

                        3 ->
                            applyCombiner type_
                                (CG.lambda [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ] (CG.tuple [ CG.val "a", CG.val "b", CG.val "c" ]))
                                args
                                generic
                                existingImports
                                currentModule
                                genericProviders

                        _ ->
                            Err "You tried to use an illegal tuple type"

                ResolvedType.CustomType ref [] ctors ->
                    makeExternalDeclaration isTopLevel
                        generic
                        ref
                        (case ctors of
                            -- [ ( ctorRef, args ) ] ->
                            --     applyCombiner (ResolvedType.Opaque ctorRef args) (ResolvedType.refToExpr currentModule existingImports ctorRef) args generic existingImports currentModule genericProviders
                            _ ->
                                applyResolvers
                                    (\resolver ->
                                        case resolver of
                                            CustomTypeResolver fn ->
                                                ctors
                                                    |> List.map
                                                        (\( ctorRef, args ) ->
                                                            applyCombiner (ResolvedType.Opaque ctorRef args) (ResolvedType.refToExpr currentModule existingImports ctorRef) args generic existingImports currentModule genericProviders
                                                        )
                                                    |> combineResults
                                                    |> Result.map (Tuple.mapFirst (List.map2 (\( ctorRef, _ ) -> Tuple.pair ctorRef.name) ctors >> fn ctors))
                                                    |> Just

                                            _ ->
                                                Nothing
                                    )
                                    ref.name
                                    generic
                                    type_
                        )

                _ ->
                    Err "Generics are not yet supported"


matchType : ResolvedType -> ResolvedType -> Bool
matchType full possiblyRef =
    case ( full, possiblyRef ) of
        ( ResolvedType.CustomType ref1 _ _, ResolvedType.Opaque ref2 [] ) ->
            ref1 == ref2

        _ ->
            full == possiblyRef


makeExternalDeclaration : Bool -> ResolvedGeneric -> Reference -> Result String ( Expression, List Function ) -> Result String ( Expression, List Function )
makeExternalDeclaration isTopLevel generic ref defExpr =
    if isTopLevel then
        defExpr

    else
        let
            name =
                generic.makeName ref.name

            annotation =
                generic.makeAnnotation (CG.fqTyped ref.modulePath ref.name [])
        in
        Result.map
            (\( expr, defs ) ->
                ( CG.fun name
                , { documentation = Nothing
                  , signature =
                        Just
                            (Helpers.node
                                { name = Helpers.node name
                                , typeAnnotation = Helpers.node annotation
                                }
                            )
                  , declaration =
                        Helpers.node
                            { name = Helpers.node name
                            , arguments = []
                            , expression = Helpers.node expr
                            }
                  }
                    :: defs
                )
            )
            defExpr


combineResults : List (Result String ( Expression, List Function )) -> Result String ( List Expression, List Function )
combineResults =
    List.foldr
        (Result.map2
            (\( itemExpr, itemDefs ) ( accuExprs, accuDefs ) ->
                ( itemExpr :: accuExprs, itemDefs ++ accuDefs )
            )
        )
        (Ok ( [], [] ))


applyResolvers fn name generic t =
    Helpers.findMap
        (\resolver ->
            case resolver of
                UniversalResolver ur ->
                    Maybe.map (\expr -> Ok ( expr, [] )) (ur t)

                _ ->
                    fn resolver
        )
        generic.resolvers
        |> Result.fromMaybe ("Could not automatically generate a definition for `" ++ name ++ "`, as we don't know how to implement this type.")
        |> Result.andThen identity


applyCombiner t ctor children generic existingImports currentModule genericProviders =
    applyResolvers
        (\resolver ->
            case resolver of
                Combiner fn ->
                    let
                        childExprsAndDefs =
                            List.foldr
                                (\arg ->
                                    Result.map2
                                        (\( argExpr, argDefs ) ( resExprs, resDefs ) ->
                                            ( argExpr :: resExprs, argDefs ++ resDefs )
                                        )
                                        (generate False generic existingImports currentModule genericProviders arg)
                                )
                                (Ok ( [], [] ))
                                children
                    in
                    childExprsAndDefs
                        |> Result.map (Tuple.mapFirst (fn t ctor))
                        |> transmogrify

                _ ->
                    Nothing
        )
        (Helpers.writeExpression ctor)
        generic
        t


transmogrify : Result err ( Maybe a, b ) -> Maybe (Result err ( a, b ))
transmogrify input =
    case input of
        Ok ( Just a, b ) ->
            Just (Ok ( a, b ))

        Ok ( Nothing, _ ) ->
            Nothing

        Err err ->
            Just (Err err)


{-| This simplifies expressions to make them easier on the eyes, but still super simple to program:

1.  Transforms `((\foo -> foo + 1) bar)` to `(bar + 1)`
2.  Transforms (\\foo qux -> bar foo qux) to `bar`

This means that generators can use the simple composability of explicit lambdas but get nice looking but still correct code.

-}
postprocessExpression : Expression -> Expression
postprocessExpression expression =
    let
        etaApplicable args exprs =
            case ( List.reverse exprs, List.Extra.last args ) of
                ( (Node _ (Expression.FunctionOrValue [] name)) :: rest, Just (Node _ (Elm.Syntax.Pattern.VarPattern pattern)) ) ->
                    name
                        == pattern
                        && List.all
                            (\(Node _ v) ->
                                case v of
                                    Expression.FunctionOrValue m2 n2 ->
                                        n2 /= name || m2 /= []

                                    _ ->
                                        False
                            )
                            rest

                _ ->
                    False
    in
    Helpers.traverseExpression
        (\expr oo ->
            ( case expr of
                Expression.Application ((Node _ (Expression.LambdaExpression lambda)) :: args) ->
                    if List.length lambda.args == List.length args && List.all isPlainPattern lambda.args then
                        let
                            bindings =
                                List.map2
                                    (\(Node _ p1) (Node _ p2) ->
                                        case p1 of
                                            VarPattern n1 ->
                                                Just ( n1, p2 )

                                            _ ->
                                                Nothing
                                    )
                                    lambda.args
                                    args
                                    |> List.filterMap identity
                                    |> Dict.fromList
                        in
                        Helpers.traverseExpression
                            (\subexpr foo ->
                                case subexpr of
                                    Expression.FunctionOrValue [] name ->
                                        case Dict.get name bindings of
                                            Just r ->
                                                ( r, foo )

                                            Nothing ->
                                                ( subexpr, foo )

                                    _ ->
                                        ( subexpr, foo )
                            )
                            ()
                            (Node.value lambda.expression)
                            |> Tuple.first

                    else
                        expr

                Expression.LambdaExpression lambda ->
                    case lambda.expression of
                        Node ln (Expression.Application app) ->
                            if etaApplicable lambda.args app then
                                case ( List.Extra.init app, List.Extra.init lambda.args ) of
                                    ( Just [ Node _ oneThing ], Just [] ) ->
                                        oneThing

                                    ( Just newApp, Just newArgs ) ->
                                        Expression.LambdaExpression
                                            { args = newArgs
                                            , expression = Node ln (Expression.Application newApp)
                                            }

                                    _ ->
                                        expr

                            else
                                expr

                        _ ->
                            expr

                _ ->
                    expr
            , oo
            )
        )
        ()
        expression
        |> Tuple.first


isPlainPattern : Node Pattern -> Bool
isPlainPattern (Node _ arg) =
    case arg of
        VarPattern _ ->
            True

        _ ->
            False


{-| This handles cases where the generator generates lambdas, but the user expects to provide their own argument naming.
Also it promotes

     myFun : Arg -> Res
     myFun =
         \arg ->  arg ++ d

to

    myFun : Arg -> Res
    myFun arg =
        arg ++ d

-}
fixFunctionDeclaration : Function -> Function
fixFunctionDeclaration func =
    let
        declaration =
            Node.value func.declaration

        ( newExpr, newArgs ) =
            case postprocessExpression (Node.value declaration.expression) of
                Expression.LambdaExpression lambda ->
                    if List.isEmpty declaration.arguments then
                        ( lambda.expression, lambda.args )

                    else if List.length declaration.arguments == List.length lambda.args && List.all isPlainPattern declaration.arguments && List.all isPlainPattern lambda.args then
                        let
                            bindings =
                                List.map2
                                    (\(Node _ p1) (Node _ p2) ->
                                        case ( p1, p2 ) of
                                            ( VarPattern n1, VarPattern n2 ) ->
                                                Just ( n1, n2 )

                                            _ ->
                                                Nothing
                                    )
                                    lambda.args
                                    declaration.arguments
                                    |> List.filterMap identity
                                    |> Dict.fromList
                        in
                        ( Helpers.traverseExpression
                            (\expr oo ->
                                case expr of
                                    Expression.FunctionOrValue [] name ->
                                        case Dict.get name bindings of
                                            Just r ->
                                                ( Expression.FunctionOrValue [] r, oo )

                                            Nothing ->
                                                ( expr, oo )

                                    _ ->
                                        ( expr, oo )
                            )
                            ()
                            (Node.value lambda.expression)
                            |> Tuple.first
                            |> Helpers.node
                        , declaration.arguments
                        )

                    else
                        ( declaration.expression, declaration.arguments )

                _ ->
                    ( declaration.expression, declaration.arguments )
    in
    { func | declaration = Helpers.node { declaration | expression = newExpr, arguments = newArgs } }