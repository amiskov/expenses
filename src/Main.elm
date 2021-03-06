module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Calendar
import DateTime
import Html exposing (Html, button, colgroup, div, em, form, h1, h2, hr, img, input, label, li, option, p, pre, select, span, table, tbody, td, text, textarea, tfoot, th, thead, tr, ul)
import Html.Attributes as Attr exposing (attribute, autofocus, checked, class, classList, for, id, name, placeholder, selected, src, step, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Iso8601
import Json.Decode exposing (Decoder, array, decodeString, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Task
import Time


type alias Ticket =
    { document : Document
    }


type alias Document =
    { receipt : Receipt }


type alias Receipt =
    { items : Array Product
    , operationType : Int
    }


{-| Признак расчета (operationType):
1 Приход (для продавца, для меня — трата)
2 Возврат прихода (для продавца, для меня — возврат денег)
3 Расход
4 Возврат расхода
Here used simplified version: only counts what's spent and what's got back.
TODO: Use this type instead of Int in Purchase
-}
type Operation
    = Expense
    | Return


type alias PurchaseJson =
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


purchaseDecoder : Decoder PurchaseJson
purchaseDecoder =
    succeed PurchaseJson
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
        |> required "operationType" int


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


type alias Purchase =
    { date : String
    , products : Array Product
    , operationType : Int
    }



---- MODEL ----


type alias Model =
    { newPurchase : Array Product
    , newProduct : Product
    , rawJson : String
    , purchases : Maybe (Array Purchase)
    , isInstructionShowed : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { newPurchase = Array.fromList []
      , newProduct = Product "" 0 Neutral
      , rawJson = ""
      , purchases = Nothing
      , isInstructionShowed = False
      }
    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "rawJson")
    )



---- UPDATE ----


type Msg
    = NoOp
    | AddProduct
    | UpdateProductName String
    | UpdateProductPrice String
    | ParsePurchases
    | ToggleInstructionVisibility
    | UpdateRawJson String
    | GradeChecked Grade Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddProduct ->
            ( updateProductList model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "rawJson")
            )

        ToggleInstructionVisibility ->
            ( { model | isInstructionShowed = not model.isInstructionShowed }
            , Cmd.none
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
                        Ok parsedPurchases ->
                            let
                                autogradeItems : Array Product -> Array Product
                                autogradeItems products =
                                    Array.map
                                        (\prod -> autogradeProduct prod)
                                        products

                                parseJsonPurchase : PurchaseJson -> Purchase
                                parseJsonPurchase parsedPurchase =
                                    { date = parsedPurchase.createdAt
                                    , products = autogradeItems parsedPurchase.ticket.document.receipt.items
                                    , operationType = parsedPurchase.ticket.document.receipt.operationType
                                    }
                            in
                            parsedPurchases
                                |> Array.map parseJsonPurchase
                                |> Just

                        Err _ ->
                            Nothing

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


nbsp =
    "\u{00A0}"


view : Model -> Html Msg
view model =
    let
        cls bg =
            bg ++ " px-2 rounded pb-1"
    in
    div [ class "text-left container " ]
        [ div [ class "px-2 py-4 mx-auto" ]
            [ h2 [ class "text-lg md:text-2xl mb-3 font-bold" ]
                [ text "Считалка"
                , text " "
                , span [ class (cls "bg-green-300") ] [ text "полезных" ]
                , text ", "
                , span [ class (cls "bg-gray-300") ] [ text "нейтральных" ]
                , text (" и" ++ nbsp)
                , span [ class (cls "bg-red-300") ] [ text "вредных" ]
                , text " "
                , text "расходов"
                ]
            , label
                [ class "block"
                , for "rawJson"
                ]
                [ text "Выписка из «Проверки чека» в JSON:" ]
            , div []
                [ textarea
                    [ onInput UpdateRawJson
                    , id "rawJson"
                    , placeholder """[{"claims":[],"ticket":{"document":{"receipt":{"cashTotalSum" и т.д."""
                    , class "w-full h-20 p-2 border rounded"
                    , Attr.value model.rawJson
                    ]
                    []
                ]
            , button
                [ class "bg-gray-500 p-2 rounded text-white font-bold"
                , onClick ParsePurchases
                ]
                [ text "Обработать" ]
            ]
        , viewInstruction model
        , viewPurchases model

        --, viewAddProductForm model
        ]


viewInstruction model =
    div
        [ classList
            [ ( "border-t", True )
            , ( "bg-gray-200", model.isInstructionShowed )
            ]
        ]
        [ span
            [ onClick ToggleInstructionVisibility
            , class "cursor-pointer inline-block m-2 border-b border-black border-dashed"
            ]
            [ text
                (if model.isInstructionShowed then
                    "Скрыть"

                 else
                    "Показать"
                )
            , text " инструкцию"
            ]
        , if model.isInstructionShowed then
            let
                viewImg file =
                    li [ class "mx-2" ]
                        [ img [ src file ] []
                        ]
            in
            ul
                [ class "flex overflow-x-auto" ]
                [ viewImg "1_scan.png"
                , viewImg "2_collect.png"
                , viewImg "3_export.png"
                , viewImg "4_copy.png"
                ]

          else
            text ""
        ]


viewExpensesByGrade : Array Purchase -> Html Msg
viewExpensesByGrade purchases =
    let
        onlyProducts =
            Array.map (\p -> p.products) purchases

        expensesBy : Grade -> Array (Array Product)
        expensesBy grade =
            Array.map (\p -> Array.filter (\prod -> prod.grade == grade) p) onlyProducts
                |> Array.filter (\a -> not (Array.isEmpty a))

        summator e s =
            e.price + s

        sumProducts : Array Product -> Int
        sumProducts expenses =
            Array.foldl summator 0 expenses

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
    div [ class "sticky top-0 right-0 shadow-lg" ]
        [ div [ class "rounded p-2 right-0 bg-gray-100 text-right" ]
            [ table [ class "w-full md:w-auto" ]
                [ tbody []
                    [ viewRow "Полезно" "text-green-700" (viewPrice sumGood)
                    , viewRow "Вредно" "text-red-700" (viewPrice sumBad)
                    , viewRow "Нейтрально" "text-gray-700" (viewPrice sumNeutral)
                    ]
                , tfoot []
                    [ viewRow "Totals" "border-t-2" (viewPrice total)
                    ]
                ]
            ]
        ]


viewPurchases : Model -> Html Msg
viewPurchases model =
    case model.purchases of
        Just purchases ->
            let
                range =
                    List.range 0 (Array.length purchases)

                purchasedItems =
                    List.map2 (\purchase purchaseId -> ( purchaseId, purchase )) (Array.toList purchases) range
            in
            div [ class "my-8 border-t" ]
                ([ if Array.isEmpty purchases then
                    text ""

                   else
                    viewExpensesByGrade purchases
                 ]
                    ++ List.map viewPurchase purchasedItems
                )

        Nothing ->
            div [] [ text "" ]


viewPurchase : ( Int, Purchase ) -> Html Msg
viewPurchase ( purchaseId, { date, products, operationType } ) =
    let
        range =
            List.range 0 (Array.length products)

        time =
            case Iso8601.toTime date of
                Ok t ->
                    let
                        dateTime =
                            DateTime.fromPosix t

                        year =
                            dateTime |> DateTime.getYear |> String.fromInt

                        month =
                            dateTime |> DateTime.getMonth |> Calendar.monthToInt |> String.fromInt

                        day =
                            dateTime |> DateTime.getDay |> String.fromInt
                    in
                    day ++ "." ++ month ++ "." ++ year

                Err _ ->
                    ""

        isReturn =
            operationType == 2
    in
    div
        [ class "my-8 mx-2 px-2 bg-gray-100 border rounded"
        , classList [ ( "bg-green-200", isReturn ) ]
        ]
        [ div [ class "my-2" ]
            [ span [ class "px-2 py-1 italic bg-gray-400 rounded" ] [ text time ] ]
        , table [ class "w-full" ]
            ([ colgroup [ style "width" "80%" ] []
             , colgroup [ style "width" "10%" ] []
             , colgroup [ style "width" "10%" ] []
             , thead
                []
                [ th [] [ text "Товар" ]
                , th [] [ text "Цена" ]
                , th []
                    [ if not isReturn then
                        text "Оценка"

                      else
                        text ""
                    ]
                ]
             ]
                ++ List.map2 (viewPurchaseItem isReturn purchaseId) (Array.toList products) range
            )
        ]


viewPurchaseItem : Bool -> Int -> Product -> Int -> Html Msg
viewPurchaseItem isReturn purchaseIdx p productIdx =
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
        [ class "border-b hover:bg-gray-200"
        , class textColor
        ]
        [ td
            [ class "text-left py-2 align-top"
            , style "overflow-wrap" "break-word"
            ]
            [ text p.name ]
        , td [ class "text-right py-2 align-top whitespace-no-wrap pr-3" ] [ text (viewPrice p.price) ]
        , td [ class "align-top py-2" ]
            [ if not isReturn then
                viewGrading p purchaseIdx productIdx

              else
                text "Возврат"
            ]
        ]


viewGrading : Product -> Int -> Int -> Html Msg
viewGrading p purchaseIdx productIdx =
    let
        productId =
            String.fromInt purchaseIdx ++ "_" ++ String.fromInt productIdx
    in
    div [ class "text-center md:whitespace-no-wrap justify-between gap-1" ]
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
    , "сигареты"
    , "marlboro"
    , "winston"
    , "пицца"
    , "додо"
    , "пончик"
    , "чизкейк"
    , "зефир"
    , "чипсы"
    ]


goodWords =
    -- TODO: Use regexes for matching words
    [ "авокадо"
    , "кижуч"
    , "форель"
    , "горбуша"
    , "яблоко"
    , "яблоки"
    , "груша"
    , "груши"
    , "минтай"
    , "тилапия"
    ]


autogradeProduct : Product -> Product
autogradeProduct p =
    { p | grade = autogradeByName p.name }


autogradeByName : String -> Grade
autogradeByName name =
    if findTriggerWordsInName name badWords then
        Bad

    else if findTriggerWordsInName name goodWords then
        let
            _ =
                Debug.log "name" name
        in
        Good

    else
        Neutral


findTriggerWordsInName : String -> List String -> Bool
findTriggerWordsInName productName triggerWords =
    let
        -- TODO: Try to find each of trigger words in the productName with `String.contains`
        -- String.contains "пиво" "Пиво ШПАТЕН МЮНХ.св.ж/б 0.5л"
        -- String.contains "пивн" "Пиво ШПАТЕН МЮНХ.св.ж/б 0.5л"
        --- etc
        -- Потому что может быть так: "КУР.БЕР.Горбуша натур.250г" (слова не разделены пробелами, только точки)
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
            Array.get purchaseIdx (Maybe.withDefault Array.empty model.purchases)
    in
    case purchase of
        Just p ->
            let
                product =
                    Array.get productIdx p.products
            in
            case product of
                Just prod ->
                    let
                        newProd : Product
                        newProd =
                            { prod | grade = newGrade }

                        newProducts : Array Product
                        newProducts =
                            Array.set productIdx newProd p.products

                        newPurchase : Purchase
                        newPurchase =
                            { p | products = newProducts }

                        newPurchases =
                            Array.set purchaseIdx newPurchase (Maybe.withDefault Array.empty model.purchases)
                    in
                    { model | purchases = Just newPurchases }

                Nothing ->
                    model

        Nothing ->
            model
