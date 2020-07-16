module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Html exposing (Html, button, colgroup, div, form, h1, hr, img, input, label, li, option, pre, select, table, tbody, td, text, textarea, tfoot, th, thead, tr, ul)
import Html.Attributes as Attr exposing (attribute, autofocus, checked, class, for, id, name, placeholder, selected, src, step, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, array, decodeString, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Task


type alias Ticket =
    { document : Document
    }


type alias Document =
    { receipt : Receipt }


type alias Receipt =
    { items : Array Product
    }


type alias Purchase =
    { ticket : Ticket
    , createdAt : String
    }


type alias Product =
    { name : String
    , price : Int
    , grade : Grade
    }


type Grade
    = Good
    | Neutral
    | Bad


productDecoder : Decoder Product
productDecoder =
    succeed Product
        |> required "name" string
        |> required "price" int
        |> hardcoded Neutral


purchaseDecoder : Decoder Purchase
purchaseDecoder =
    succeed Purchase
        |> required "ticket" ticketDecoder
        |> required "createdAt" string


ticketDecoder : Decoder Ticket
ticketDecoder =
    succeed Ticket
        |> required "document" documentDecoder


documentDecoder : Decoder Document
documentDecoder =
    succeed Document
        |> required "receipt" receiptDecoder


receiptDecoder : Decoder Receipt
receiptDecoder =
    succeed Receipt
        |> required "items" (array productDecoder)


viewGradingInput p id purchaseIdx productIdx grade =
    let
        inputName =
            "grade_" ++ id

        bgClass =
            case grade of
                Neutral ->
                    "bg-gray-300"

                Bad ->
                    "bg-red-300"

                Good ->
                    "bg-green-300"

        isChecked =
            case ( grade, p.grade ) of
                ( Neutral, Neutral ) ->
                    True

                ( Bad, Bad ) ->
                    True

                ( Good, Good ) ->
                    True

                _ ->
                    False

        val =
            case grade of
                Neutral ->
                    "нейтрально"

                Bad ->
                    "вредно"

                Good ->
                    "полезно"
    in
    label
        [ class "inline-block p-2 rounded-md border text-white font-bold border-gray-500"
        , class bgClass
        ]
        [ text "" --val
        , input
            [ type_ "radio"
            , class ""
            , name inputName
            , checked isChecked
            , value val
            , onClick (GradeChecked grade purchaseIdx productIdx)
            ]
            []
        ]



---- MODEL ----


type alias Model =
    { newPurchase : Array Product
    , newProduct : Product
    , rawJson : String
    , purchases : Array (Array Product)
    }


init : ( Model, Cmd Msg )
init =
    ( { newPurchase = Array.fromList []
      , newProduct = Product "" 0 Neutral
      , rawJson = ""
      , purchases = Array.fromList []
      }
    , Task.succeed ParsePurchases
        |> Task.perform identity
    )



---- UPDATE ----


type Msg
    = NoOp
    | AddProduct
    | UpdateProductName String
    | UpdateProductPrice String
    | ParsePurchases
    | UpdateRawJson String
    | GradeChecked Grade Int Int


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

        GradeChecked newGrade purchaseIdx productIdx ->
            ( updateProductGrade model newGrade purchaseIdx productIdx
            , Cmd.none
            )

        ParsePurchases ->
            let
                purchases =
                    case decodeString (array purchaseDecoder) model.rawJson of
                        Ok p ->
                            let
                                autogradeItems : Array Product -> Array Product
                                autogradeItems items =
                                    Array.map
                                        (\prod -> autogradeProduct prod)
                                        items
                            in
                            Array.map (\purchase -> autogradeItems purchase.ticket.document.receipt.items) p

                        Err er ->
                            let
                                _ =
                                    Debug.log "Error" er
                            in
                            Array.fromList []

                newModel =
                    { model | purchases = purchases }
            in
            ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewPrice : Int -> String
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
        | newPurchase = Array.append model.newPurchase (Array.fromList [ model.newProduct ])
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
    div [ class "text-left" ]
        [ label
            [ class "block"
            , for "rawJson"
            ]
            [ text "Выписка из «Проверки чеков» в JSON" ]
        , div []
            [ textarea
                [ onInput UpdateRawJson
                , id "rawJson"
                , class "border rounded"
                , Attr.value model.rawJson
                ]
                []
            ]
        , button
            [ class "bg-gray-500 p-2 rounded text-white font-bold"
            , onClick ParsePurchases
            ]
            [ text "Обработать" ]
        , viewPurchases model

        --, viewAddProductForm model
        ]


viewExpensesByGrade model =
    let
        expensesBy : Grade -> Array (Array Product)
        expensesBy grade =
            Array.map (\p -> Array.filter (\prod -> prod.grade == grade) p) model.purchases
                |> Array.filter (\a -> not (Array.isEmpty a))

        sumProducts : Array Product -> Int
        sumProducts expenses =
            Array.foldl (\e s -> e.price + s) 0 expenses

        sumPurchaseBy : Grade -> Int
        sumPurchaseBy grade =
            Array.map sumProducts (expensesBy grade)
                |> Array.foldl (+) 0

        sumGood =
            sumPurchaseBy Good

        sumBad =
            sumPurchaseBy Bad

        sumNeutral =
            sumPurchaseBy Neutral

        total =
            sumGood + sumBad + sumNeutral

        viewRow key colorClass val =
            tr [ class colorClass ]
                [ th [ class "text-left pr-4" ] [ text key ]
                , td [ class "text-right" ] [ text val ]
                ]
    in
    div [ class "sticky top-0 right-0" ]
        [ div [ class "absolute rounded p-3 mt-8 right-0 bg-gray-100 text-right" ]
            [ table []
                [ tbody []
                    [ viewRow "Good" "text-green-700" (viewPrice sumGood)
                    , viewRow "Bad" "text-red-700" (viewPrice sumBad)
                    , viewRow "Neutral" "text-gray-700" (viewPrice sumNeutral)
                    ]
                , tfoot []
                    [ viewRow "Totals" "border-t-2" (viewPrice total)
                    ]
                ]
            ]
        ]


viewPurchases : Model -> Html Msg
viewPurchases model =
    let
        range =
            List.range 0 (Array.length model.purchases)

        purchasedItems =
            List.map2 (\purchase purchaseId -> ( purchaseId, purchase )) (Array.toList model.purchases) range
    in
    div [ class "my-8 border-t" ]
        ([ if Array.isEmpty model.purchases then
            text ""

           else
            viewExpensesByGrade model
         ]
            ++ List.map viewPurchase purchasedItems
        )


viewPurchase : ( Int, Array Product ) -> Html Msg
viewPurchase ( purchaseId, products ) =
    let
        range =
            List.range 0 (Array.length products)
    in
    div [ class "my-8" ]
        [ table [ class "w-2/3" ]
            ([ colgroup [ style "width" "80%" ] []
             , colgroup [ style "width" "10%" ] []
             , colgroup [ style "width" "10%" ] []
             , thead
                []
                [ th [] [ text "Товар" ]
                , th [] [ text "Цена" ]
                , th [] [ text "Оценка" ]
                ]
             ]
                ++ List.map2 (viewPurchaseItem purchaseId) (Array.toList products) range
            )
        ]


viewPurchaseItem : Int -> Product -> Int -> Html Msg
viewPurchaseItem purchaseIdx p productIdx =
    let
        textColor =
            case p.grade of
                Neutral ->
                    "text-gray-700"

                Bad ->
                    "text-red-700"

                Good ->
                    "text-green-700"
    in
    tr
        [ class "border-b"
        , class textColor
        ]
        [ td
            [ class "text-left"
            ]
            [ text p.name ]
        , td [ class "text-right whitespace-no-wrap pr-3" ] [ text (viewPrice p.price) ]
        , td []
            [ viewGrading p purchaseIdx productIdx
            ]
        ]


viewGrading : Product -> Int -> Int -> Html Msg
viewGrading p purchaseIdx productIdx =
    let
        productId =
            String.fromInt purchaseIdx ++ "_" ++ String.fromInt productIdx
    in
    div [ class "whitespace-no-wrap flex justify-between gap-1" ]
        [ viewGradingInput p productId purchaseIdx productIdx Good
        , viewGradingInput p productId purchaseIdx productIdx Neutral
        , viewGradingInput p productId purchaseIdx productIdx Bad
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


viewProductList : Model -> Html Msg
viewProductList model =
    let
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

        showProduct p =
            tr []
                [ td [ class "product-name" ] [ text p.name ]
                , td [ class "product-price" ] [ text (viewPrice p.price) ]
                , td
                    [ class "product-grade"
                    , class (gradeClass p.grade)
                    ]
                    [ text (gradeText p.grade) ]
                ]
    in
    table [ class "product-table" ]
        (List.map showProduct (Array.toList model.newPurchase))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


badWords =
    [ "пивн"
    , "пивной"
    , "пиво"
    , "вино"
    , "винный"
    , "pepsi"
    , "пепси"
    , "cola"
    , "кола"
    ]


goodWords =
    [ "авокадо"
    ]


autogradeProduct : Product -> Product
autogradeProduct p =
    { p | grade = autogradeByName p.name }


autogradeByName : String -> Grade
autogradeByName name =
    if findTriggerWordsInName name badWords then
        Bad

    else if List.member name goodWords then
        Good

    else
        Neutral


findTriggerWordsInName productName triggerWords =
    let
        nameWords =
            productName
                |> String.toLower
                |> String.words

        includedInTriggerWords w =
            List.member w triggerWords
    in
    List.map includedInTriggerWords nameWords
        |> List.any (\w -> w == True)


updateProductGrade : Model -> Grade -> Int -> Int -> Model
updateProductGrade model newGrade purchaseIdx productIdx =
    let
        purchase =
            Array.get purchaseIdx model.purchases
    in
    case purchase of
        Just p ->
            let
                product =
                    Array.get productIdx p
            in
            case product of
                Just prod ->
                    let
                        newProd =
                            { prod | grade = newGrade }

                        newItems =
                            Array.set productIdx newProd p

                        newPurchases =
                            Array.set purchaseIdx newItems model.purchases
                    in
                    { model | purchases = newPurchases }

                Nothing ->
                    model

        Nothing ->
            model
