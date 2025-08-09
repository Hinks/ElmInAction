port module PhotoGallery exposing
    ( Model
    , Msg(..)
    , Photo
    , Status(..)
    , init
    , initialModel
    , photoDecoder
    , subscriptions
    , update
    , urlPrefix
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Random



-- MODEL


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    , activity : String
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    , activity = ""
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
        }



-- UPDATE


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
    | GotActivity Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotPhotos (Ok photos) ->
            case List.head photos of
                Just photo ->
                    applyFilters { model | status = Loaded photos photo.url }

                Nothing ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity value ->
            case Decode.decodeValue Decode.string value of
                Ok status ->
                    ( { model | activity = status }, Cmd.none )

                Err _ ->
                    ( { model | activity = "Pasta status error" }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ Attr.class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> ThumbnailSize -> Model -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize model =
    [ button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ Attr.class "activity" ] [ text model.activity ]
    , div [ Attr.class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ Attr.id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ Attr.id "thumbnails", Attr.class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas [ Attr.id "main-canvas", Attr.class "large" ] []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ Attr.src (urlPrefix ++ thumb.url)
        , Attr.title (thumb.title ++ "[" ++ String.fromInt thumb.size ++ " kb]")
        , Attr.classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ Attr.type_ "radio"
            , Attr.name "size"
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    Decode.at [ "detail", "userSlidTo" ] Decode.int
        |> Decode.map toMsg
        |> on "slide"


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"



-- DECODER


photoDecoder : Decode.Decoder Photo
photoDecoder =
    Decode.succeed buildPhoto
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "size" Decode.int
        |> Pipeline.optional "title" Decode.string "(untitled)"


buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (Decode.Value -> msg) -> Sub msg



-- MAIN


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            case Decode.decodeValue Decode.float flags of
                Ok version ->
                    "Initializing Pasta v" ++ String.fromFloat version

                Err _ ->
                    "No pasta version number"
    in
    ( { initialModel | activity = activity }, initialCmd )


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
