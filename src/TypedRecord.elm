module TypedRecord exposing
    ( Attr
    , AttrValue(..)
    , TypedRecord
    , attrBoolDecoder
    , attrFloatDecoder
    , attrIntDecoder
    , attrRecordDecoder
    , attrStringDecoder
    , attrToString
    , filteredBy
    , getAttrByKey
    , listBoolDecoder
    , listFloatDecoder
    , listIntDecoder
    , listStringDecoder
    , sortedBy
    )

import Json.Decode as JD
import Tuple


type alias TR =
    TypedRecord


type alias TypedRecord =
    List Attr


type alias Attr =
    ( String, AttrValue )


type AttrValue
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Record (List Attr)
    | List (List AttrValue)


getAttrByKey : String -> TypedRecord -> Maybe Attr
getAttrByKey searchKey item =
    -- imitation of searching for attributes likewise in Record
    let
        checkAttrKey =
            \key attr ->
                Tuple.first attr == key
    in
    case String.split "." searchKey of
        [ key ] ->
            List.filter (checkAttrKey key) item |> List.head

        key :: rest ->
            case List.filter (checkAttrKey key) item |> List.head of
                Just attr ->
                    case attr of
                        ( _, Record subAttr ) ->
                            getAttrByKey (String.join "." rest) subAttr

                        ( _, _ ) ->
                            Nothing

                Nothing ->
                    Nothing

        [] ->
            Nothing


attrValueToString : AttrValue -> String
attrValueToString attrValue =
    case attrValue of
        String value ->
            value

        Int value ->
            String.fromInt value

        Float value ->
            String.fromFloat value

        Bool value ->
            if value then
                "true"

            else
                "false"

        Record attrs ->
            List.map (\( _, av ) -> attrValueToString av) attrs
                |> String.join " "

        List attrValues ->
            List.map (\av -> attrValueToString av) attrValues
                |> String.join " "


attrToString : Maybe Attr -> Maybe String
attrToString is_attr =
    is_attr
        |> Maybe.map (\( _, attrValue ) -> attrValueToString attrValue)


sortedBy : ( String, String ) -> List TR -> List TR
sortedBy ( byKey, withOrder ) items =
    let
        sortFunc : String -> (TR -> Maybe Attr) -> TR -> TR -> Order
        sortFunc =
            \order func a b ->
                case compare (func a |> attrToString |> Maybe.withDefault "") (func b |> attrToString |> Maybe.withDefault "") of
                    LT ->
                        if order == "asc" then
                            LT

                        else
                            GT

                    EQ ->
                        EQ

                    GT ->
                        if order == "dsc" then
                            LT

                        else
                            GT

        sortByOrder =
            sortFunc withOrder
    in
    List.sortWith
        (getAttrByKey byKey |> sortByOrder)
        items


filteredBy : List String -> String -> List TR -> List TR
filteredBy filteredKeys queryString items =
    -- filter only by certain attributes
    let
        containsQueryString =
            \attrValue ->
                String.contains (String.toLower queryString) (String.toLower attrValue)

        getFilteredAttrs =
            \item ->
                List.foldl (\key store -> getAttrByKey key item :: store) [] filteredKeys
    in
    items
        |> List.filter
            (\item ->
                getFilteredAttrs item
                    |> List.map attrToString
                    |> List.map (Maybe.withDefault "")
                    |> List.any containsQueryString
            )



{--JSON decoders--}


buildAttr : String -> AttrValue -> JD.Decoder Attr
buildAttr attrName attrValue =
    JD.succeed ( attrName, attrValue )



{--basic types decoders --}


attrIntDecoder : String -> JD.Decoder Attr
attrIntDecoder attrName =
    JD.map Int JD.int |> JD.andThen (buildAttr attrName)


attrFloatDecoder : String -> JD.Decoder Attr
attrFloatDecoder attrName =
    JD.map Float JD.float |> JD.andThen (buildAttr attrName)


attrStringDecoder : String -> JD.Decoder Attr
attrStringDecoder attrName =
    JD.map String JD.string |> JD.andThen (buildAttr attrName)


attrBoolDecoder : String -> JD.Decoder Attr
attrBoolDecoder attrName =
    JD.map Bool JD.bool |> JD.andThen (buildAttr attrName)



{--data structures decodeers --}


listStringDecoder : String -> JD.Decoder Attr
listStringDecoder attrName =
    JD.map List (JD.list (JD.map String JD.string)) |> JD.andThen (buildAttr attrName)


listIntDecoder : String -> JD.Decoder Attr
listIntDecoder attrName =
    JD.map List (JD.list (JD.map Int JD.int)) |> JD.andThen (buildAttr attrName)


listFloatDecoder : String -> JD.Decoder Attr
listFloatDecoder attrName =
    JD.map List (JD.list (JD.map Float JD.float)) |> JD.andThen (buildAttr attrName)


listBoolDecoder : String -> JD.Decoder Attr
listBoolDecoder attrName =
    JD.map List (JD.list (JD.map Bool JD.bool)) |> JD.andThen (buildAttr attrName)


attrRecordDecoder : String -> JD.Decoder TR -> JD.Decoder Attr
attrRecordDecoder attrName nestedRecordDecoder =
    JD.map Record (JD.lazy (\_ -> nestedRecordDecoder)) |> JD.andThen (buildAttr attrName)
