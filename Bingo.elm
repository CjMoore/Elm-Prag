module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


-- Model
type alias Entry =
        { id : Int
        , phrase : String
        , points : Int
        , marked : Bool }

type alias Model = 
         { name : String
         , gameNumber: Int
         , entries : List Entry  
         , alertMessage : Maybe String }

type alias Score = 
        { id : Int
        , name: String
        , score: Int  }

initialModel : Model
initialModel = 
        { name = "Mike",
          gameNumber = 1,
          entries = [], 
          alertMessage = Nothing 
        }


-- UPDATE


type Msg 
        = NewGame 
        | Mark Int 
        | NewRandom Int 
        | NewEntries (Result Http.Error (List Entry))
        | CloseAlert
        | ShareScore
        | NewScore (Result Http.Error Score)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
        case msg of 
                NewGame ->
                        ({ model | gameNumber = model.gameNumber+1}, getEntries)
                CloseAlert ->
                        ( { model | alertMessage = Nothing }, Cmd.none )
                ShareScore -> 
                        (model, postScore model)
                NewScore ( Ok score ) ->
                        let
                            message = 
                                    "Your score of "
                                    ++ (toString score.score)
                                    ++ " was successfully shared!"

                        in
                            ( { model | alertMessage = Just message }, Cmd.none )
                NewScore ( Err error ) ->
                        let
                            message = 
                                    "Error posting your score"
                                    ++ (toString error)
                        in
                            ( { model | alertMessage = Just message }, Cmd.none )
                Mark id ->
                        let
                            markEntry e = 
                                    if e.id == id then
                                            { e | marked = (not e.marked) }
                                    else
                                            e
                         in 
                             ( { model | entries = List.map markEntry model.entries }, Cmd.none )
                NewRandom randomNumber ->
                        ( { model | gameNumber = randomNumber }, Cmd.none )
                NewEntries ( Ok randomEntries) ->
                            ( { model | entries = randomEntries
                                                  |> List.sortBy .points }, Cmd.none)
                NewEntries ( Err error) ->
                        let
                            errorMessage = 
                                    case error of 
                                            Http.NetworkError ->
                                                    "Is the server running?"
                                            Http.BadStatus response ->
                                                    (toString response.status)
                                            Http.BadPayload message _ ->
                                                    "Decoding Failed: " ++ message
                                            _ ->
                                                    (toString error)
                                            
                        in                        
                            ({ model | alertMessage  = Just errorMessage }, Cmd.none)
                            


-- Decoders/Encoders


entryDecoder : Decoder Entry
entryDecoder = 
        decode Entry
                |> DecodePipeline.required "id" Decode.int
                |> DecodePipeline.required "phrase" Decode.string
                |> DecodePipeline.optional "points" Decode.int 100
                |> DecodePipeline.hardcoded False


encodedScore : Model -> Encode.Value
encodedScore model = 
                   Encode.object  [ ("name", Encode.string model.name)
                                  , ( "score", Encode.int (sumMarkedPoints model.entries) ) ]


scoreDecoder : Decoder Score
scoreDecoder = 
        decode Score
                |> DecodePipeline.required "id" Decode.int
                |> DecodePipeline.required "name" Decode.string
                |> DecodePipeline.required "score" Decode.int

-- Commands


postScore : Model -> Cmd Msg
postScore model = 
        let
           url = 
               " http://localhost:3000/scores"
           body = 
                   encodedScore model
                   |> Http.jsonBody
           request = 
                   Http.post url body scoreDecoder
        in
           Http.send NewScore request

generateRandomNumber : Cmd Msg
generateRandomNumber = 
        Random.generate  NewRandom (Random.int 1 100)

entriesUrl : String
entriesUrl = 
        "http://localhost:3000/random-entries"

getEntries : Cmd Msg
getEntries = 
       ( Decode.list entryDecoder )
        |> Http.get entriesUrl
        |> Http.send NewEntries


-- View

viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage = 
        case alertMessage of 
                Just message -> 
                        div [ class "alert" ]
                            [ span [ class "close", onClick CloseAlert ] [ text "X" ],
                              text message ]
                Nothing ->
                        text ""

playerInfo : String -> Int -> String
playerInfo name gameNumber = 
        name ++ " - Game #" ++ (toString gameNumber)

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber = 
        let
         playerInfoText = 
                playerInfo name gameNumber
                        |> String.toUpper
                        |> text               
        in
                h2 [ id "info" , class "classy" ] 
                   [ playerInfoText ]

viewHeader : String -> Html Msg
viewHeader title = 
        header []
                [ h1 [] [ text title ] ]

viewFooter : Html Msg
viewFooter = 
        footer [ ]
                [ a [ href "http://elm-lang.org" ]
                    [ text "Powered By Elm" ] 
                ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries = 
        let
            listOfEntries =
                   entries
                   |> List.map  viewEntryItem
                   --List.map viewEntryItem entries 
        in
            ul [] listOfEntries 


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries = 
        entries
        |> List.filter .marked
        |> List.map .points
        |> List.foldl (+) 0
           

viewScore : Int -> Html Msg
viewScore sum = 
        div 
           [ class "score" ]
           [ span [ class "label" ] [ text "Score" ] 
           ,  span [ class "value" ] [ text (toString sum) ]
           ]
           
viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
        li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
           [ span [ class "phrase" ] [text entry.phrase]
           , span [ class "points" ] [text (toString entry.points) ]]


view : Model -> Html Msg 
view model = 
        div [class "content"]
        [viewHeader "BUZZWORD BINGO",
         viewPlayer model.name model.gameNumber,
         viewEntryList model.entries,
         viewScore (sumMarkedPoints model.entries), 
         viewAlertMessage model.alertMessage, 
         div [ class "button-group" ] 
             [ button [ onClick NewGame ] [text "New Game"]
             , button [ onClick ShareScore ] [text "Share Score"] ],
         div [class "debug"] [ text (toString model) ],
         viewFooter
         ]

allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
       List.all .marked entries 
        

--main : Html Msg
--main = 
--    update NewGame initialModel
--    |> view 

main : Program Never Model Msg
main = 
        Html.program
          { init  = ( initialModel, getEntries )
          , view = view
          , update = update
          , subscriptions = (\model -> Sub.none )
          }

        
