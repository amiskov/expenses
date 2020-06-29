module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html, button, div, form, h1, img, input, li, option, select, table, td, text, tr, ul)
import Html.Attributes exposing (attribute, autofocus, class, id, placeholder, src, step, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Task


type alias Product =
    { name : String
    , price : Float
    , grade : Grade
    }


type Grade
    = Good
    | Neutral
    | Bad



---- MODEL ----


type alias Model =
    { purchase : List Product
    , newProduct : Product
    }


init : ( Model, Cmd Msg )
init =
    ( { purchase = []
      , newProduct = Product "" 0.0 Neutral
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | AddProduct
    | UpdateProductName String
    | UpdateProductPrice String


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

        NoOp ->
            ( model, Cmd.none )


updateProductList : Model -> Model
updateProductList model =
    { model
        | purchase = List.append model.purchase [ model.newProduct ]
        , newProduct = Product "" 0.0 Neutral
    }


updateProductName ({ newProduct } as model) name =
    { model | newProduct = { newProduct | name = name } }


updateProductPrice ({ newProduct } as model) price =
    case String.toFloat price of
        Just p ->
            { model | newProduct = { newProduct | price = p } }

        Nothing ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewProductList model
        , viewAddProductForm model
        ]


viewAddProductForm : Model -> Html Msg
viewAddProductForm model =
    form [ onSubmit AddProduct ]
        [ input
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
            , value (String.fromFloat model.newProduct.price)
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
                , td [ class "product-price" ] [ text (String.fromFloat p.price ++ "₽") ]
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
