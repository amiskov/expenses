module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Html exposing (Html, button, div, form, h1, hr, img, input, label, li, option, pre, select, table, td, text, textarea, th, tr, ul)
import Html.Attributes as Attr exposing (attribute, autofocus, checked, class, id, name, placeholder, selected, src, step, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, array, decodeString, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import JunJson
import Task


type alias Document =
    { receipt : Receipt }


type alias Receipt =
    { items : Array Product
    }


type alias Product =
    { name : String
    , price : Int
    , grade : Grade
    }


type alias Purchase =
    { document : Document
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
                    "Normal"

                Bad ->
                    "Bad"

                Good ->
                    "Good"
    in
    label [ class <| "inline-block p-2" ]
        [ input
            [ type_ "radio"
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
      , rawJson = JunJson.json
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
                            Array.map (\purchase -> autogradeItems purchase.document.receipt.items) p

                        Err er ->
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
    div [ class "bg-red" ]
        [ textarea
            [ onInput UpdateRawJson
            , class "border"
            , Attr.value model.rawJson
            ]
            []
        , button [ class "bg-gray-500 p-2", onClick ParsePurchases ] [ text "Parse" ]
        , hr [] []
        , viewExpensesByGrade model
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
    in
    div [ class "sticky top-0 right-0" ]
        [ ul [ class "absolute rounded-md p-3 m-3 right-0 bg-gray-300 text-right" ]
            [ li [] [ text <| "Good: " ++ viewPrice sumGood ]
            , li [] [ text <| "Bad: " ++ viewPrice sumBad ]
            , li [] [ text <| "Neutral: " ++ viewPrice sumNeutral ]
            , li [] [ text <| "Total: " ++ viewPrice total ]
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
    div [] (List.map viewPurchase purchasedItems)


viewPurchase : ( Int, Array Product ) -> Html Msg
viewPurchase ( purchaseId, products ) =
    let
        range =
            List.range 0 (Array.length products)
    in
    table [ class "w-1/2 m-auto mb-8" ]
        (List.map2 (viewPurchaseItem purchaseId) (Array.toList products) range)


viewPurchaseItem : Int -> Product -> Int -> Html Msg
viewPurchaseItem purchaseIdx p productIdx =
    let
        bg =
            case p.grade of
                Neutral ->
                    "bg-gray-300"

                Bad ->
                    "bg-red-300"

                Good ->
                    "bg-green-300"
    in
    tr [ class <| "border-b" ++ " " ++ bg ]
        [ td [ class "text-left" ] [ text p.name ]
        , td [ class "text-right whitespace-no-wrap" ] [ text (viewPrice p.price) ]
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
    div [ class "whitespace-no-wrap" ]
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
                , td [ class "product-grade", class (gradeClass p.grade) ] [ text (gradeText p.grade) ]
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
