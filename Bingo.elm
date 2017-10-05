module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import ViewHelpers exposing (..)
import Entry


-- Model

type GameState = EnteringName | Playing


--type alias Entry =
--        { id : Int
--        , phrase : String
--        , points : Int
--        , marked : Bool }

type alias Model = 
         { name : String
         , gameNumber: Int
         , entries : List Entry.Entry  
         , alertMessage : Maybe String 
         , nameInput : String
         , gameState : GameState}


type alias Score = 
        { id : Int
        , name: String
        , score: Int  }

initialModel : Model
initialModel = 
        { name = "Mike",
          gameNumber = 1,
          entries = [], 
          alertMessage = Nothing,
          nameInput = "",
          gameState = EnteringName 
        }


-- UPDATE


type Msg 
        = NewGame 
        | Mark Int 
        | NewRandom Int 
        | NewEntries (Result Http.Error (List Entry.Entry))
        | CloseAlert
        | ShareScore
        | NewScore (Result Http.Error Score)
        | SetNameInput String
        | SaveNameInput
        | CancelName
        | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
        case msg of 
                ChangeGameState state ->
                        ( { model | gameState = state }, Cmd.none )
                CancelName ->
                        ( { model | nameInput = "", gameState = Playing }, Cmd.none )
                SaveNameInput ->
                        ({ model | name = model.nameInput,
                                   nameInput = "",
                                   gameState = Playing }, Cmd.none)
                SetNameInput value ->
                        ( { model | nameInput = value }, Cmd.none )
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
                    ( { model | alertMessage = Just (httpErrorToMessage error)}, Cmd.none )
                Mark id ->
                     ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )
                NewRandom randomNumber ->
                        ( { model | gameNumber = randomNumber }, Cmd.none )
                NewEntries ( Ok randomEntries) ->
                            ( { model | entries = randomEntries
                                                  |> List.sortBy .points }, Cmd.none)
                NewEntries ( Err error) ->
                    ({ model | alertMessage  = Just (httpErrorToMessage error)}, Cmd.none)
                            

httpErrorToMessage : Http.Error -> String
httpErrorToMessage error = 
    case error of 
            Http.NetworkError ->
                    "Is the server running?"
            Http.BadStatus response ->
                    (toString response.status)
            Http.BadPayload message _ ->
                    "Decoding Failed: " ++ message
            _ ->
                    (toString error)


-- Decoders/Encoders


--entryDecoder : Decoder Entry.Entry
--entryDecoder = 
--        decode Entry.Entry
--                |> DecodePipeline.required "id" Decode.int
--                |> DecodePipeline.required "phrase" Decode.string
--                |> DecodePipeline.optional "points" Decode.int 100
--                |> DecodePipeline.hardcoded False


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


apiUrlPrefix : String
apiUrlPrefix = 
        "http://localhost:3000"

postScore : Model -> Cmd Msg
postScore model = 
        let
           url = 
               apiUrlPrefix ++ "/scores"
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
        apiUrlPrefix ++ "/random-entries"

getEntries : Cmd Msg
getEntries = 
      Entry.getEntries NewEntries entriesUrl 

hasZeroScore : Model -> Bool
hasZeroScore model = 
        ( sumMarkedPoints model.entries == 0 )


-- View

--viewAlertMessage : Maybe String -> Html Msg
--viewAlertMessage alertMessage = 
--        case alertMessage of 
--                Just message -> 
--                        div [ class "alert" ]
--                            [ span [ class "close", onClick CloseAlert ] [ text "X" ],
--                              text message ]
--                Nothing ->
--                        text ""

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber = 
        h2 [ id "info" , class "classy" ] 
           [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
           , text (" - Game #" ++ (toString gameNumber))
           ]

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

--viewEntryList : List Entry.Entry -> Html Msg
--viewEntryList entries = 
--        let
--            listOfEntries =
--                   entries
--                   |> List.map  viewEntryItem
--                   --List.map viewEntryItem entries 
--        in
--            ul [] listOfEntries 


sumMarkedPoints : List Entry.Entry -> Int
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
           
--viewEntryItem : Entry.Entry -> Html Msg
--viewEntryItem entry =
--        li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
--           [ span [ class "phrase" ] [text entry.phrase]
--           , span [ class "points" ] [text (toString entry.points) ]]


view : Model -> Html Msg 
view model = 
        div [class "content"]
        [viewHeader "BUZZWORD BINGO",
         viewPlayer model.name model.gameNumber,
         viewNameInput model,
         Entry.viewEntryList Mark model.entries,
         viewScore (sumMarkedPoints model.entries), 
         alert CloseAlert model.alertMessage, 
         div [ class "button-group" ] 
             [ primaryButton NewGame "New Game"
             , button [onClick ShareScore, disabled (hasZeroScore model), class "primary"] [text "Share Score"]],
         div [class "debug"] [ text (toString model) ],
         viewFooter
         ]


viewNameInput : Model -> Html Msg
viewNameInput model = 
        case model.gameState of
                EnteringName -> 
                        div [class "name-input"]
                            [input [ type_ "text"
                                   , placeholder "Who's playing?"
                                   , autofocus True 
                                   , onInput SetNameInput
                                   , value model.nameInput] []
                            , primaryButton SaveNameInput "Save"
                            , primaryButton CancelName "Cancel" 
                            ]

                Playing ->
                         text ""

allEntriesMarked : List Entry.Entry -> Bool
allEntriesMarked entries =
       List.all .marked entries 
        
emptyNameInput : Model -> Bool
emptyNameInput model =
        String.isEmpty model.nameInput


main : Program Never Model Msg
main = 
        Html.program
          { init  = ( initialModel, getEntries )
          , view = view
          , update = update
          , subscriptions = (\model -> Sub.none )
          }

        
