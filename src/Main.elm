module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page.Autocomplete
import Page.Button
import Page.TextArea
import Page.TextField
import Page.Validation
import Section


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Page
    = Button
    | TextField
    | TextArea
    | Autocomplete
    | Validation


type alias Model =
    { page : Page
    , autocompletePage : Page.Autocomplete.Model
    , validationPage : Page.Validation.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    -- TODO edit initial page
    ( { page = Validation
      , autocompletePage = Page.Autocomplete.init
      , validationPage = Page.Validation.init
      }
    , Cmd.none
    )


type Msg
    = SetPage Page
    | AutocompletePageMsg Page.Autocomplete.Msg
    | ValidationPageMsg Page.Validation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )

        AutocompletePageMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Page.Autocomplete.update subMsg model.autocompletePage
            in
            ( { model | autocompletePage = newModel }
            , Cmd.map AutocompletePageMsg cmd
            )

        ValidationPageMsg subMsg ->
            ( { model | validationPage = Page.Validation.update subMsg model.validationPage }
            , Cmd.none
            )


viewPageLink : Page -> String -> Page -> Html Page
viewPageLink thisPage text_ currentPage =
    button
        [ class "px-4 py-2 leading-none rounded"
        , class <|
            if currentPage == thisPage then
                "bg-cyan-900 text-white"

            else
                "bg-cyan-700/20"
        , onClick thisPage
        ]
        [ text text_ ]


pagesTabs : List (Page -> Html Page)
pagesTabs =
    [ viewPageLink Button "Button"
    , viewPageLink TextField "TextField"
    , viewPageLink TextArea "TextArea"
    , viewPageLink Validation "Validation"
    ]


view : Model -> Html Msg
view model =
    div []
        [ Html.map SetPage <|
            div [ class "mx-auto overflow-x-auto max-w-screen-lg px-4 py-4 my-4 flex gap-x-4" ]
                (List.map (\c -> c model.page) pagesTabs)
        , viewPage model
        , div [ class "h-10" ] []
        ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        Button ->
            Section.view Page.Button.view

        TextField ->
            Section.view Page.TextField.view

        TextArea ->
            Section.view Page.TextArea.view

        Autocomplete ->
            Section.view (Page.Autocomplete.view model.autocompletePage)
                |> Html.map AutocompletePageMsg

        Validation ->
            Section.view (Page.Validation.view model.validationPage)
                |> Html.map ValidationPageMsg
