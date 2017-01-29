module Fetch exposing (..)

import Json.Decode as Decode exposing (list, string, field, bool, maybe)
import Types exposing (..)
import String.Extra
import Http
import HttpBuilder exposing (RequestBuilder)


fetchOperation : String -> Cmd Msg
fetchOperation url =
    let
        url2 =
            if devMode then
                corsEverywhere url
            else
                url

        tagger : Result Http.Error Operation -> Msg
        tagger result =
            case result of
                Ok v ->
                    OperationFetched v

                Err s ->
                    FetchOperationFailure s
    in
        HttpBuilder.get url2
            |> HttpBuilder.withExpect (Http.expectJson operation)
            |> HttpBuilder.send tagger


fetchOperationLinks : Cmd Msg
fetchOperationLinks =
    let
        url =
            if devMode then
                corsEverywhere "https://api.atomist.com/catalog/operation/search"
            else
                "https://api.atomist.com/catalog/operation/search"

        body =
            """{ "queries": [{ "archive": { "scope": "public" }, "tags": ["elm"]}] }"""

        tagger : Result Http.Error (List OperationLink) -> Msg
        tagger result =
            case result of
                Ok v ->
                    OperationLinksFetched v

                Err s ->
                    FetchOperationLinksFailure s
    in
        HttpBuilder.post url
            |> HttpBuilder.withStringBody "application/json" body
            |> HttpBuilder.withExpect (Http.expectJson operationLinks)
            |> HttpBuilder.send tagger


corsEverywhere : String -> String
corsEverywhere url =
    "http://localhost:10000/" ++ url


filterInvalidOperations : List OperationLink -> List OperationLink
filterInvalidOperations operationLinks =
    List.filter
        (\operationLink ->
            case operationLink.type_ of
                UnknownOperationType _ ->
                    False

                _ ->
                    True
        )
        operationLinks


operationLinks : Decode.Decoder (List OperationLink)
operationLinks =
    (field "operations" (list operationLink))
        |> Decode.map filterInvalidOperations


decodeTypeString : String -> OperationType
decodeTypeString typeString =
    case typeString of
        "generator" ->
            Generator

        "editor" ->
            Editor

        _ ->
            UnknownOperationType typeString


operationLink : Decode.Decoder OperationLink
operationLink =
    Decode.map3 OperationLink
        (field "type" string |> Decode.map decodeTypeString)
        (field "name" string |> Decode.map stripJessitronPrefix)
        (field "link" (field "href" string))


parameter : Decode.Decoder Parameter
parameter =
    let
        mapMaybeDisplayName maybeName =
            case maybeName of
                Just name ->
                    name

                Nothing ->
                    ""
    in
        Decode.map4 Parameter
            (field "name" string)
            (field "description" string)
            (field "required" bool)
            (maybe (field "display_name" string) |> Decode.map mapMaybeDisplayName)
            |> Decode.map fixEmptyDisplayName


fixEmptyDisplayName : Parameter -> Parameter
fixEmptyDisplayName parameter =
    case parameter.displayName of
        "" ->
            { parameter | displayName = parameter.name }

        _ ->
            parameter


operation : Decode.Decoder Operation
operation =
    Decode.map4 Operation
        ((field "operation" <| field "type" string) |> Decode.map decodeTypeString)
        ((field "operation" <| field "name" string) |> Decode.map stripJessitronPrefix)
        (field "description" string)
        (field "parameters" <| list parameter)



-- {
-- "name": "project_name",
-- "description": "Name of your new project.",
-- "pattern": "^[-\\w.]+$",
-- "required": true,
-- "displayable": true,
-- "max_length": 21,
-- "min_length": -1,
-- "display_name": "Project Name",
-- "tags": []
-- },


stripJessitronPrefix : String -> String
stripJessitronPrefix name =
    String.Extra.replace "jessitron.elm-rugs." "" name
