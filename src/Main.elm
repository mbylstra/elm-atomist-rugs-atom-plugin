module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (name, class, style, type_)
import Html.Events exposing (..)
import Types exposing (..)
import Fetch exposing (..)
import Dict


-- import Task exposing (Task)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


devMode : Bool
devMode =
    True


init : ( Model, Cmd Msg )
init =
    { route = Loading } ! [ fetchOperationLinks ]


initOperationPage : Operation -> OperationPageModel
initOperationPage operation =
    { operation = operation, formValues = Dict.empty }


operationPageModelToRugCommand : OperationPageModel -> String
operationPageModelToRugCommand { operation, formValues } =
    let
        operationVerb =
            case operation.type_ of
                Generator ->
                    "generate"

                Editor ->
                    "edit"

                UnknownOperationType _ ->
                    ""

        -- TODO: refactor types.. this shouldn't be possible!
        addQuotes s =
            "\"" ++ s ++ "\""

        params =
            operation.parameters
                |> List.filterMap
                    (\parameter ->
                        case Dict.get parameter.name formValues of
                            Just value ->
                                case parameter.required of
                                    True ->
                                        Just <| addQuotes value

                                    False ->
                                        Just <| parameter.name ++ "=" ++ addQuotes value

                            Nothing ->
                                Nothing
                    )
                |> String.join " "
    in
        "rug " ++ operationVerb ++ " jessitron:elm-rugs:" ++ operation.name ++ " " ++ params



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        OperationLinksFetched operations ->
            { model | route = OperationLinksPage operations } ! []

        FetchOperationLinksFailure error ->
            let
                _ =
                    Debug.crash (toString error)
            in
                model ! []

        OperationLinkClicked href ->
            { model | route = Loading } ! [ fetchOperation href ]

        OperationFetched operation ->
            { model
                | route = OperationPage <| initOperationPage operation
            }
                ! []

        FetchOperationFailure error ->
            let
                _ =
                    Debug.crash (toString error)
            in
                model ! []

        BackButtonClicked ->
            { model | route = Loading } ! [ fetchOperationLinks ]

        FieldUpdated name value ->
            case model.route of
                OperationPage { operation, formValues } ->
                    let
                        newFormValues =
                            Dict.insert name value formValues
                    in
                        { model
                            | route =
                                OperationPage
                                    { operation = operation
                                    , formValues = newFormValues
                                    }
                        }
                            ! []

                _ ->
                    model ! []

        FormSubmitted ->
            case model.route of
                OperationPage { operation, formValues } ->
                    -- _ = Debug.log operation formValues
                    model ! []

                _ ->
                    model ! []



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Loading ->
            Html.div [] [ text "loading" ]

        OperationLinksPage operationLinks ->
            operationLinksPageView operationLinks

        OperationPage operation ->
            operationPageView operation


operationLinksPageView : List OperationLink -> Html Msg
operationLinksPageView operationLinks =
    Html.div
        [ class "select-list" ]
        [ ol [ class "list-group" ]
            (List.map operationLinkView operationLinks)
        ]


operationLinkView : OperationLink -> Html Msg
operationLinkView link =
    li
        [ onClick <| OperationLinkClicked link.linkHref ]
        [ div [ class "status icon icon-chevron-right" ] []
        , div [] [ text link.name ]
        ]


operationPageView : OperationPageModel -> Html Msg
operationPageView operationPage =
    let
        { operation, formValues } =
            operationPage
    in
        div
            []
            [ div [ class "block" ]
                [ button
                    [ onClick BackButtonClicked
                    , class "btn icon icon-arrow-left inline-block-right"
                    ]
                    [ text "Commands" ]
                ]
            , hr [ style [ ( "border-top", "1px solid #191919" ) ] ] []
            , h1 [] [ text operation.name ]
            , p [] [ text operation.description ]
            , div [ class "settings-view" ]
                [ operationFormView operation ]
            , text <| operationPageModelToRugCommand operationPage
            ]


operationFormView : Operation -> Html Msg
operationFormView operation =
    form
        [ onSubmit FormSubmitted, style [ ( "flex-grow", "1" ) ] ]
        [ div [] (List.map parameterFieldView operation.parameters)
        , div [ class "control-group" ]
            [ button
                [ type_ "submit", class "btn btn-primary" ]
                [ text "Do It" ]
            ]
        ]


parameterFieldView : Parameter -> Html Msg
parameterFieldView parameter =
    let
        optional =
            if parameter.required then
                ""
            else
                " (optional)"
    in
        div
            [ class "control-group" ]
            [ div [ class "controls" ]
                [ label [ class "control-label" ]
                    [ div [ class "setting-title" ] [ text <| parameter.displayName ++ optional ] ]
                , div [ class "controls" ]
                    [ input
                        [ name parameter.name
                        , onInput (FieldUpdated parameter.name)
                        ]
                        []
                    ]
                ]
            ]


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none


exampleUrl : String
exampleUrl =
    "https://api.atomist.com/catalog/operation/jessitron/elm-rugs/2.0.0/generator/StaticPage/public"
