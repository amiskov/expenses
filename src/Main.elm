module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html, button, div, form, h1, img, input, li, option, pre, select, table, td, text, textarea, th, tr, ul)
import Html.Attributes as Attr exposing (attribute, autofocus, checked, class, id, name, placeholder, selected, src, step, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, decodeString, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import JunJson
import Murmur3
import Task


productDecoder : Decoder Product
productDecoder =
    succeed Product
        |> required "name" string
        |> required "price" int
        |> hardcoded Neutral


type alias Document =
    { receipt : Receipt }


type alias Receipt =
    { items : List Product
    }


type alias Product =
    { name : String
    , price : Int
    , grade : Grade
    }


type alias Purchase =
    { document : Document
    }


purchaseDecoder : Decoder Purchase
purchaseDecoder =
    succeed Purchase
        |> required "document" documentDecoder


documentDecoder : Decoder Document
documentDecoder =
    succeed Document
        |> required "receipt" receiptDecoder


receiptDecoder : Decoder Receipt
receiptDecoder =
    succeed Receipt
        |> required "items" (list productDecoder)


type alias User =
    { id : Int
    , email : String
    , followers : Int
    }


type Grade
    = Good
    | Neutral
    | Bad


viewGradingInput idx grade selectedGrade =
    let
        inputName =
            "grade" ++ String.fromInt idx

        isChecked =
            case ( grade, selectedGrade ) of
                ( Neutral, Neutral ) ->
                    let
                        _ =
                            Debug.log "neutral, neutral" ( grade, selectedGrade )
                    in
                    True

                ( Bad, Bad ) ->
                    let
                        _ =
                            Debug.log "bad, bad" ( grade, selectedGrade )
                    in
                    True

                ( Good, Good ) ->
                    True

                _ ->
                    False

        val =
            case grade of
                Neutral ->
                    "Normal"

                Bad ->
                    "Bad"

                Good ->
                    "Good"
    in
    input
        [ type_ "radio"
        , name inputName
        , checked isChecked
        , value val
        ]
        []


viewGrading : Grade -> Int -> Html Msg
viewGrading selectedGrade idx =
    div [ class "whitespace-no-wrap" ]
        [ viewGradingInput idx Good selectedGrade
        , viewGradingInput idx Neutral selectedGrade
        , viewGradingInput idx Bad selectedGrade
        ]



---- MODEL ----


type alias Model =
    { purchase : List Product
    , newProduct : Product
    , rawJson : String
    , purchases : List Purchase
    }


init : ( Model, Cmd Msg )
init =
    ( { purchase = []
      , newProduct = Product "" 0 Neutral
      , rawJson = JunJson.json
      , purchases = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | AddProduct
    | UpdateProductName String
    | UpdateProductPrice String
    | ParsePurchases
    | UpdateRawJson String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddProduct ->
            ( updateProductList model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "productName")
            )

        UpdateProductName n ->
            ( updateProductName model n
            , Cmd.none
            )

        UpdateProductPrice p ->
            ( updateProductPrice model p
            , Cmd.none
            )

        UpdateRawJson j ->
            ( { model | rawJson = j }
            , Cmd.none
            )

        ParsePurchases ->
            let
                purchases =
                    case decodeString (list purchaseDecoder) model.rawJson of
                        Ok p ->
                            p

                        Err er ->
                            let
                                _ =
                                    Debug.log "Error" er
                            in
                            []

                newModel =
                    { model | purchases = purchases }
            in
            ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewPrice p =
    let
        rub =
            p // 100

        kop =
            remainderBy 100 p
    in
    String.fromInt rub
        ++ "."
        ++ (if kop == 0 then
                "00"

            else
                String.fromInt kop
           )
        ++ " ₽"


updateProductList : Model -> Model
updateProductList model =
    { model
        | purchase = List.append model.purchase [ model.newProduct ]
        , newProduct = Product "" 0 Neutral
    }


updateProductName ({ newProduct } as model) name =
    { model | newProduct = { newProduct | name = name } }


updateProductPrice ({ newProduct } as model) price =
    case String.toInt price of
        Just p ->
            { model | newProduct = { newProduct | price = p } }

        Nothing ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "bg-red" ]
        [ textarea
            [ onInput UpdateRawJson
            , class "border"
            , Attr.value model.rawJson
            ]
            []
        , button [ class "bg-gray-500 p-2", onClick ParsePurchases ] [ text "Parse" ]
        , viewPurchases model

        --, viewAddProductForm model
        ]


viewPurchases : Model -> Html Msg
viewPurchases model =
    let
        purchasedItems =
            List.map (\p -> p.document.receipt.items) model.purchases

        _ =
            Debug.log "Items" purchasedItems
    in
    div [] (List.map viewPurchase purchasedItems)


viewPurchase : List Product -> Html Msg
viewPurchase products =
    let
        total =
            List.length products

        range =
            List.range 1 total
    in
    table [ class "w-1/2 m-auto mb-8" ]
        (List.map2 viewPurchaseItem products range)


viewPurchaseItem : Product -> Int -> Html Msg
viewPurchaseItem p idx =
    let
        hash =
            -- idx and name are not always different
            -- TODO: use something more reliable to generate unique hash per product
            Murmur3.hashString idx p.name
    in
    tr [ class "border-b" ]
        [ td [ class "text-left" ] [ text p.name ]
        , td [ class "text-right whitespace-no-wrap" ] [ text (viewPrice p.price) ]
        , td []
            [ viewGrading p.grade hash
            ]
        ]


viewAddProductForm : Model -> Html Msg
viewAddProductForm model =
    form [ onSubmit AddProduct ]
        [ viewProductList model
        , input
            [ placeholder "Product Name"
            , id "productName"
            , attribute "required" ""
            , autofocus True
            , value model.newProduct.name
            , onInput UpdateProductName
            ]
            []
        , input
            [ type_ "number"
            , placeholder "Product Price"
            , step "0.01"
            , attribute "required" ""
            , value (viewPrice model.newProduct.price)
            , placeholder "Price"
            , onInput UpdateProductPrice
            ]
            []
        , select []
            [ option [] [ text "Neutral" ]
            , option [] [ text "Good" ]
            , option [] [ text "Bad" ]
            ]
        , button [] [ text "Add" ]
        ]


gradeClass g =
    case g of
        Neutral ->
            "grade_neutral"

        Bad ->
            "grade_bad"

        Good ->
            "grade_good"


gradeText g =
    case g of
        Neutral ->
            "Neutral"

        Bad ->
            "Bad"

        Good ->
            "Good"


viewProductList : Model -> Html Msg
viewProductList model =
    let
        showProduct p =
            tr []
                [ td [ class "product-name" ] [ text p.name ]
                , td [ class "product-price" ] [ text (viewPrice p.price) ]
                , td [ class "product-grade", class (gradeClass p.grade) ] [ text (gradeText p.grade) ]
                ]
    in
    table [ class "product-table" ]
        (List.map showProduct model.purchase)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
