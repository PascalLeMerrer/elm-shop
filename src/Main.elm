module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, fieldset, form, h1, header, input, label, legend, main_, span, text)
import Html.Attributes exposing (class, for, id, value)
import Html.Events exposing (onInput)



{-
   Step 1: update the total price every time the unit price or the quantity is updated
   Step 2: add a promotion input field: the quantity above with the price is reduced of 10%
   Step 3: (optional) add a discount input field, to allow changing the percentage of reduction
   Step 4: display an error message when the price is 0 or less
   Step 5: hide the error message when the close button of the notification is clicked
   Step 6: add a delivery address input field; the address may be completed by sending an http get request to
                "https://api-adresse.data.gouv.fr/search/?q=<URL encoded search parameter>"
   Step 7: display an error message when the request fails.
           You can use the throttling feature of the web browser developer tools to simulate a network failure.
   Step 8: display an error when the server response does not contain the expected JSON.
           Change the name of one of the expected fields to simulate this  kind of problem.
           Make sure the message is precise about the issue.
-}


type Msg
    = UserChangedQuantity String


type alias Model =
    { quantity : Int
    , unitPrice : Float
    , totalPrice : Float
    }


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



----- INIT -----


init : () -> ( Model, Cmd Msg )
init _ =
    ( { quantity = 0
      , unitPrice = 1.45
      , totalPrice = 123.0
      }
    , Cmd.none
    )



----- UPDATE -----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedQuantity string ->
            let
                quantity =
                    string
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( { model | quantity = quantity }, Cmd.none )



----- VIEW -----


view : Model -> Document Msg
view model =
    { title = "The Elm Shop"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ viewHeader
    , viewMain model
    ]


viewHeader : Html msg
viewHeader =
    header []
        [ h1 []
            [ text "The Elm Shop" ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    main_ []
        [ form [ class "form" ]
            [ viewCartContent model
            ]
        ]


viewCartContent : Model -> Html Msg
viewCartContent model =
    fieldset []
        [ legend [] [ text "Contenu du panier" ]
        , div [ class "form-item-2columns" ]
            [ label [ for "quantity" ] [ text "Quantité" ]
            , input
                [ id "quantity"
                , value (String.fromInt model.quantity)
                , onInput UserChangedQuantity
                ]
                []
            ]
        , div [ class "form-item-2columns" ]
            [ label [ for "unitPrice" ] [ text "Prix unitaire (€)" ]
            , input
                [ id "unitPrice"
                , value <| formatPrice model.unitPrice
                ]
                []
            ]
        , div [ class "form-item-2columns" ]
            [ label [] [ text "Prix total (€)" ]
            , span [ class "form-item-text" ] [ text <| formatPrice model.totalPrice ]
            ]
        ]


formatPrice price =
    let
        price_in_cents =
            price
                * 100
                |> round
                |> String.fromInt

        euros =
            String.slice 0 -2 price_in_cents

        cents =
            String.left 2 price_in_cents
    in
    euros ++ "." ++ cents


viewAddress : Model -> Html Msg
viewAddress model =
    fieldset []
        [ legend [] [ text "Adresse de livraison" ]
        , input
            [ class "form-item-1column"
            , onInput UserChangedQuantity
            ]
            []
        , div [ class "form-item-1column" ]
            []
        ]


viewError : Maybe String -> Html Msg
viewError maybeErrorMessage =
    case maybeErrorMessage of
        Just message ->
            div [ class "notification-error" ]
                [ text message
                , button [ class "button-close" ] [ span [] [ text "x" ] ]
                ]

        Nothing ->
            text ""
