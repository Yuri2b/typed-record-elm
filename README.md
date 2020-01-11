# TypedRecord.elm

Data structure with named keys and explicitly typed values.
Allow to represent JS Object like this:
```javascript
user : {
  name: "Jane",
  age: 24,
  rating: 4.3,
  tags: ["customer", "vip"],
  // ugly named attribute for example
  st_adress: {
    city: "Montreal",
    state: "WA"
  }
}
```
via JSON decoding to this:
```elm
user =
  [
    ("name", String "Jane")
  , ("age", Int 24)
  , ("rating", Float 4.3)
  , ("tags",
      [ String "customer"
      , String "vip"
      ]
    )
  , ("address",
      [ ("city", String "Monreal")
      , ("state", String "WA")
      ]
    )
  ]
```
Decoder example:
```elm
import TypedRecord as TR
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)

type alias User =
  TR.TypedRecord

userDecoder : JD.Decoder User
userDecoder =
    JD.succeed (\a b c d e -> [ a, b, c, d, e ])
        |> required "name"      (TR.attrStringDecoder "name")
        |> required "age"       (TR.attrIntDecoder "age")
        |> required "rating"    (TR.attrFloatDecoder "rating")
        |> required "tags"      (TR.listStringDecoder "tags")
        |> required "st_adress" (TR.attrRecordDecoder "address" userAddressDecoder)

userAddressDecoder : JD.Decoder TypedRecord
userAddressDecoder =
  JD.succeed (\a b -> [a, b])
    |> required "city"  (TR.attrStringDecoder "city")
    |> required "state" (TR.attrStringDecoder "state")
```

## Acccessing attributs
Allow getting attributes by its string keys, like `user["name"]` and `user["address"]["state"]` in JS
```elm
TR.getAttrByKey "name" user -- Just (String "Jane")
TR.getAttrByKey "age"  user -- Just (Int 24)
TR.getAttrByKey "tags" user -- Just ([String "customer", String "vip"])
TR.getAttrByKey "address.state"  user -- Just (String "WA")
TR.getAttrByKey "surname"  user -- Nothing
```

and convert these to `String` for `HTML.text` in view:
```elm
user
  |> TR.getAttrByKey "rating"
  |> TR.attrToString
  |> Maybe.withDefault "not set yet"
```

## Sorting and Filtering

Sorting by chosen key and order. Nested attribute's keys like `"address.city"` are also supported

```elm
users =
  [
    [("id", Int 5), ("name", String "Daisy")]
  , [("id", Int 7), ("name", String "Ahmad")]
  , [("id", Int 2), ("name", String "Coralas")]
  ]

users
  |> TR.sortedBy ("id", "asc")
  {-- 
      [
        [("id", Int 2), ("name", String "Coralas")]
      , [("id", Int 5), ("name", String "Daisy")]
      , [("id", Int 7), ("name", String "Ahmad")]
      ] 
  --}

users
  |> TR.sortedBy ("name", "dsc")
  {-- 
      [
      , [("id", Int 5), ("name", String "Daisy")]
        [("id", Int 2), ("name", String "Coralas")]
      , [("id", Int 7), ("name", String "Ahmad")]
      ] 
  --}
```

Filtering by substring matching in choosen attributes. Nested attribute's keys like `"address.city"` are also supported
```elm
users =
  [
    [("id", Int 5), ("name", String "Daisy"), ("email", String "@mail.org")]
  , [("id", Int 8), ("name", String "Kornetty"), ("email", String "@server.org")]
  , [("id", Int 2), ("name", String "Coralas"), ("email", String "@postamt.net")]
  ]

users
  |> TR.filteredBy ["email"] "org"
  {-- 
    [
    , [("id", Int 5), ("name", String "Daisy"), ("email", String "@mail.org")]
      [("id", Int 8), ("name", String "Kornetty"), ("email", String "@server.org")]
    ] 
--}

users
  |> TR.filteredBy ["email", "name"] "net"
  {-- 
    [
      [("id", Int 8), ("name", String "Kornetty"), ("email", String "@server.org")]
    , [("id", Int 2), ("name", String "Coralas"), ("email", String "@postamt.net")]
    ] 
--}
```
