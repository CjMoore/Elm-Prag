module Entry exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodedPipeline exposing (decode, required, optional, hardcoded)

type alias Entry = 
        { id : Int
        , phrase : String
        , points : Int
        , marked : Bool
        }

markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id = 
        let
            markEntry e = 
                    if e.id == id then
                            { e | marked = (not e.marked) }
                    else
                            e
        in
            List.map markEntry entries

entryDecoder : Decoder Entry
entryDecoder = 
        decode Entry
        |> DecodedPipeline.required "id" Decode.int
        |> DecodedPipeline.required "phrase" Decode.string
        |> DecodedPipeline.optional "points" Decode.int 100
        |> DecodedPipeline.hardcoded False

entriesUrl : String
entriesUrl = 
        "http://localhost:3000/random-entries"


getEntries : (Result Http.Error (List Entry) -> msg)-> String -> Cmd msg
getEntries msg url = 
        ( Decode.list entryDecoder )
        |> Http.get url 
        |> Http.send msg


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry = 
        li [ classList [ ( "marked", entry.marked ) ], onClick (msg entry.id) ]
        [ span [class "phrase"] [text entry.phrase]
        , span [class "points"] [text (toString entry.points)] 
        ]

viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries = 
        let 
            listOfEntries =
                    List.map ( viewEntryItem  msg ) entries
        in
            ul [] listOfEntries
