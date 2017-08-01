module Main exposing (..)

import Debug.Json.View as View
import Json.Decode


type alias Person =
    { name : String
    , age : Int
    }


decodePerson : Json.Decode.Decoder Person
decodePerson =
    Json.Decode.map2 Person
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "age" Json.Decode.int)


main =
    View.program "user.json" decodePerson
