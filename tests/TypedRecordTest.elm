module TypedRecordTest exposing (tests)

import Debug
import Expect
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import Test exposing (Test, describe, test)
import TypedRecord as TR exposing (AttrValue(..), TypedRecord)


sampleUsersJson : String
sampleUsersJson =
    """
  [
    { "name": "Jane",
      "age": 24, 
      "rating": 4.3,
      "tags": ["customer", "vip"],
       "st_adress": {
          "city": "Montreal",
          "state": "WA",
          "zip": "47353"
        }
    },
    { "name": "Alexander",
      "age": 24, 
      "rating": 4.3,
      "tags": ["customer"],
       "st_adress": {
          "city": "Holland",
          "state": "MO",
          "zip": "14302"
        }
    },
    { "name": "Ahmad",
      "age": 24, 
      "rating": 4.3,
      "tags": ["admin", "system"],
       "st_adress": {
          "city": "Kansas City",
          "state": "MO",
          "zip": "40920"
        }
    },
    { "name": "Daisy",
      "age": 24, 
      "rating": 4.3,
      "tags": ["employee", "shop", "manager"],
       "st_adress": {
          "city": "Winchester",
          "state": "MA",
          "zip": "68376"
        }
    }
  ]
  """


type alias User =
    TR.TypedRecord


userDecoder : JD.Decoder User
userDecoder =
    JD.succeed (\a b c d e -> [ a, b, c, d, e ])
        |> required "name" (TR.attrStringDecoder "name")
        |> required "age" (TR.attrIntDecoder "age")
        |> required "rating" (TR.attrFloatDecoder "rating")
        |> required "tags" (TR.listStringDecoder "tags")
        |> required "st_adress" (TR.attrRecordDecoder "address" userAddressDecoder)


userAddressDecoder : JD.Decoder TypedRecord
userAddressDecoder =
    JD.succeed (\a b c -> [ a, b, c ])
        |> required "city" (TR.attrStringDecoder "city")
        |> required "state" (TR.attrStringDecoder "state")
        |> required "zip" (TR.attrStringDecoder "zip")


sampleUsers : Result JD.Error (List User)
sampleUsers =
    JD.decodeString (JD.list userDecoder) sampleUsersJson


tests : Test
tests =
    describe "TypedRecord"
        [ describe "decoding from JSON"
            [ test "valid JSON should be decoded correctly" <|
                \() ->
                    case sampleUsers of
                        Ok (user1 :: _ :: _ :: _) ->
                            user1
                                |> Expect.all
                                    [ \user ->
                                        user
                                            |> TR.getAttrByKey "name"
                                            |> Expect.equal
                                                (Just ( "name", String "Jane" ))
                                    , \user ->
                                        user
                                            |> TR.getAttrByKey "age"
                                            |> Expect.equal
                                                (Just ( "age", Int 24 ))
                                    , \user ->
                                        user
                                            |> TR.getAttrByKey "rating"
                                            |> Expect.equal
                                                (Just ( "rating", Float 4.3 ))
                                    , \user ->
                                        user
                                            |> TR.getAttrByKey "tags"
                                            |> Expect.equal
                                                (Just ( "tags", List [ String "customer", String "vip" ] ))
                                    , \user ->
                                        user
                                            |> TR.getAttrByKey "address"
                                            |> Expect.equal
                                                (Just ( "address", Record [ ( "city", String "Montreal" ), ( "state", String "WA" ), ( "zip", String "47353" ) ] ))
                                    ]

                        Ok _ ->
                            Expect.fail "JSON decoding error"

                        Err message ->
                            Expect.fail (Debug.toString message)
            ]
        , describe "filtering and sorting"
            [ test "it should return right values from nested input keys" <|
                \() ->
                    case sampleUsers of
                        Ok (user :: _) ->
                            user
                                |> TR.getAttrByKey "address.city"
                                |> Expect.equal
                                    (Just ( "city", String "Montreal" ))

                        Ok _ ->
                            Expect.fail "JSON decoding error"

                        Err message ->
                            Expect.fail (Debug.toString message)
            , test "sorting by flat key" <|
                \() ->
                    let
                        users =
                            [ [ ( "id", Int 5 ), ( "name", String "Daisy" ) ]
                            , [ ( "id", Int 7 ), ( "name", String "Ahmad" ) ]
                            , [ ( "id", Int 2 ), ( "name", String "Coralas" ) ]
                            ]
                    in
                    users
                        |> TR.sortedBy ( "id", "asc" )
                        |> Expect.all
                            [ \sortedUsers ->
                                sortedUsers
                                    |> List.length
                                    |> Expect.equal 3
                            , \sortedUsers ->
                                sortedUsers
                                    |> List.map (TR.getAttrByKey "id")
                                    |> Expect.equal
                                        [ Just ( "id", Int 2 )
                                        , Just ( "id", Int 5 )
                                        , Just ( "id", Int 7 )
                                        ]
                            ]
            , test "sorting by nested key" <|
                \() ->
                    let
                        users =
                            [ [ ( "id", Int 5 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Daisy" )
                                    , ( "last", String "Yyy" )
                                    ]
                                )
                              ]
                            , [ ( "id", Int 7 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Ahmad" )
                                    , ( "last", String "Xxx" )
                                    ]
                                )
                              ]
                            , [ ( "id", Int 2 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Coralas" )
                                    , ( "last", String "Zzz" )
                                    ]
                                )
                              ]
                            ]
                    in
                    users
                        |> Expect.all
                            [ \u ->
                                u
                                    |> TR.sortedBy ( "name.first", "asc" )
                                    |> List.map (TR.getAttrByKey "name.first")
                                    |> Expect.equal
                                        [ Just ( "first", String "Ahmad" )
                                        , Just ( "first", String "Coralas" )
                                        , Just ( "first", String "Daisy" )
                                        ]
                            , \u ->
                                u
                                    |> TR.sortedBy ( "name.last", "dsc" )
                                    |> List.map (TR.getAttrByKey "name.last")
                                    |> Expect.equal
                                        [ Just ( "last", String "Zzz" )
                                        , Just ( "last", String "Yyy" )
                                        , Just ( "last", String "Xxx" )
                                        ]
                            ]
            , test "filtering by flat key" <|
                \() ->
                    let
                        users =
                            [ [ ( "id", Int 5 ), ( "name", String "Daisy" ) ]
                            , [ ( "id", Int 7 ), ( "name", String "Ahmadal" ) ]
                            , [ ( "id", Int 2 ), ( "name", String "Coralas" ) ]
                            ]
                    in
                    users
                        |> Expect.all
                            [ \u ->
                                u
                                    |> TR.filteredBy [ "id" ] "2"
                                    |> Expect.equal
                                        [ [ ( "id", Int 2 )
                                          , ( "name", String "Coralas" )
                                          ]
                                        ]
                            , \u ->
                                u
                                    |> TR.filteredBy [ "id" ] "8"
                                    |> Expect.equal []
                            , \u ->
                                u
                                    |> TR.filteredBy [ "id", "name" ] "al"
                                    |> Expect.equal
                                        [ [ ( "id", Int 7 )
                                          , ( "name", String "Ahmadal" )
                                          ]
                                        , [ ( "id", Int 2 )
                                          , ( "name", String "Coralas" )
                                          ]
                                        ]
                            ]
            , test "filtering by nested key" <|
                \() ->
                    let
                        users =
                            [ [ ( "id", Int 5 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Daisy" )
                                    , ( "last", String "Yyy" )
                                    ]
                                )
                              ]
                            , [ ( "id", Int 7 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Ahmad" )
                                    , ( "last", String "Xxx" )
                                    ]
                                )
                              ]
                            , [ ( "id", Int 2 )
                              , ( "name"
                                , Record
                                    [ ( "first", String "Coralas" )
                                    , ( "last", String "Zzz" )
                                    ]
                                )
                              ]
                            ]
                    in
                    users
                        |> Expect.all
                            [ \u ->
                                u
                                    |> TR.filteredBy [ "name.first" ] "Ahmad"
                                    |> List.map (TR.getAttrByKey "name.first")
                                    |> Expect.equal
                                        [ Just ( "first", String "Ahmad" ) ]
                            , \u ->
                                u
                                    |> TR.filteredBy [ "name.WRONG" ] "Ahmad"
                                    |> Expect.equal []
                            , \u ->
                                u
                                    |> TR.filteredBy [ "name.last" ] "SAMPLE"
                                    |> Expect.equal []
                            ]
            ]
        ]
