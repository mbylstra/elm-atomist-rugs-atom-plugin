module Types exposing (..)

import Http
import Dict exposing (Dict)


devMode : Bool
devMode =
    True


type alias Model =
    { route : Page }


type Page
    = Loading
    | OperationLinksPage (List OperationLink)
    | OperationPage OperationPageModel


type alias OperationPageModel =
    { operation : Operation
    , formValues : FormValues
    }


type alias FormValues =
    Dict String String


type OperationType
    = Generator
    | Editor
    | UnknownOperationType String


type alias OperationLink =
    { type_ : OperationType
    , name : String
    , linkHref : String
    }


type alias Operation =
    { type_ : OperationType
    , name : String
    , description : String
    , parameters : List Parameter
    }



-- {
-- "operation": {
-- "type": "generator",
-- "name": "StaticPage",
-- "archive": {
-- "group": "jessitron",
-- "artifact": "elm-rugs",
-- "version": {
-- "value": "2.0.0"
-- },
-- "scope": "public",
-- "group_artifact": "jessitron elm-rugs",
-- "sortable_value": "000020000000000100000000000000000000000000000010"
-- }
-- },


type alias Parameter =
    { name : String
    , displayName : String
    , required : Bool
    , description : String
    }


type Msg
    = Noop
    | OperationLinksFetched (List OperationLink)
    | FetchOperationLinksFailure Http.Error
    | OperationLinkClicked String
    | OperationFetched Operation
    | FetchOperationFailure Http.Error
    | BackButtonClicked
    | FieldUpdated String String
    | FormSubmitted
