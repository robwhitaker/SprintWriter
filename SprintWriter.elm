module SprintWriter where

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field exposing (..)
import Color exposing (..)
import Time exposing (Time)
import String
import Html exposing (Html)
import Html.Attributes exposing (style, disabled, class)
import Html.Events exposing (on, targetValue)
import StartApp
import Text
import Keyboard
import Effects exposing (Effects)
import Window
import Task exposing (Task)

---- MODEL ----

type alias Model =
    { text : String
    , wordGoal : Int
    , timeGoal : Time
    , timer : Time
    , lastTime : Time
    , mode : ViewMode
    , windowDimensions : (Int, Int)
    }

initialModel = { text = "", wordGoal = 0, timeGoal = 0, timer = 0, mode = CreatingSession, windowDimensions = (0,0), lastTime = 0 }

type alias SavedSession =
    { text : String
    , wordGoal : Int
    , timeGoal : Time
    , timer : Time
    , mode : String
    }

type ViewMode = Writing | Peeking | CreatingSession | DoneWithSession

---- UPDATE ----

type Action
    = EnterChar String
    | Peek
    | StartSession
    | EndSession
    | CreateSession
    | UpdateWordGoal Int
    | UpdateTimeGoal Time
    | TimerTick Time
    | ContinueLastSprint
    | UpdateText String
    | WindowResize (Int, Int)
    | NoOp

update : Action -> Model -> (Model, Effects Action)
update action model =
    (case action of
        EnterChar ch ->
            if model.mode /= Writing then model
            else { model | text <- model.text ++ ch }

        UpdateText newText ->
            { model | text <- newText }

        Peek ->
            if model.mode /= Writing && model.mode /= Peeking then model
            else { model | mode <- if model.mode == Writing then Peeking else Writing }

        StartSession -> { model | mode <- Writing, timeGoal <- model.timeGoal * 1000 }

        EndSession -> { model | mode <- DoneWithSession }

        CreateSession -> { model | mode <- CreatingSession }

        UpdateWordGoal newGoal -> { model | wordGoal <- newGoal }

        UpdateTimeGoal newGoal -> { model | timeGoal <- newGoal }

        TimerTick newTime ->
            if model.mode /= Writing && model.mode /= Peeking then model
            else { model | timer <- if model.lastTime == 0 then model.timer + 1000 else model.timer + (newTime - model.lastTime)
                         , lastTime <- newTime
                 }

        ContinueLastSprint ->
            lastSession
             `Maybe.andThen` (\last ->
                Just { model |
                    text <- last.text,
                    wordGoal <- last.wordGoal,
                    timeGoal <- last.timeGoal,
                    timer <- last.timer,
                    mode <- case last.mode of
                        "CreatingSession" -> CreatingSession
                        "Writing" -> Writing
                        "Peeking" -> Writing
                        "DoneWithSession" -> DoneWithSession
                })
             |> Maybe.withDefault initialModel

        WindowResize newDimensions -> { model | windowDimensions <- newDimensions }

        _ -> model
    ) |> flip (,) Effects.none

---- VIEW ----

view : Signal.Address Action -> Model -> Html
view address model =
    let numWords = List.length <| String.words model.text
        (width, height) = model.windowDimensions
        words = String.words model.text |> List.filter ((/=) "") |> List.length
        completion =
            if
                | model.wordGoal == 0 && model.timeGoal == 0 -> 1
                | model.timeGoal == 0 -> toFloat words / toFloat model.wordGoal
                | model.wordGoal == 0 -> model.timer / model.timeGoal
                | otherwise -> max (toFloat words / toFloat model.wordGoal) (model.timer / model.timeGoal)
        makeText size = Text.fromString >> Text.color lightGrey >> Text.height size >> Text.typeface ["Courier New", "Courier", "monospace"] >> centered
    in
        case model.mode of
            Writing ->
                flow down
                    [ container (round <| toFloat width * completion) 5 topLeft empty |> color green
                    , container width 50 middle (makeText 30 <| toString words)
                    , container width (height-120) middle (makeText 150 <| String.right 1 model.text)
                    , spacer width (height - (height-120) - 50 - 5 - 30)
                    , container width 30 (midRight) (flow right
                        [ makeText 24 "Hold `Esc` to peek..."
                        , if completion >= 1 then (makeText 24 " | End Sprint >>" |> clickable (Signal.message address EndSession)) else show ""
                        , makeText 22 " "
                        ])
                    ]
                |> container width height topLeft
                |> color black
                |> Html.fromElement

            CreatingSession ->
                flow down
                    [ Text.fromString "Create Sprint" |> Text.height 50 |> Text.monospace |> centered |> container width 50 middle
                    , flow down
                        [ field defaultStyle (\{string} -> Signal.message address <| UpdateWordGoal <| (String.toInt >> Result.toMaybe >> Maybe.withDefault 0) string) "Word Goal" { noContent | string <- if model.wordGoal > 0 then toString model.wordGoal else "", selection <- Selection 100 100 Forward }
                        , field defaultStyle (\{string} -> Signal.message address <| UpdateTimeGoal <| (String.toInt >> Result.toMaybe >> Maybe.withDefault 0 >> toFloat) string) "Timer (in seconds)" { noContent | string <- if model.timeGoal > 0 then toString model.timeGoal else "", selection <- Selection 100 100 Forward }
                        , centered (Text.height 18 <| Text.monospace <| Text.fromString <| String.join ":" <| List.map (toString >> \str -> if String.length str < 2 then "0" ++ str else str)
                            [ floor <| Time.inHours model.timeGoal * Time.second
                            , floor (Time.inMinutes model.timeGoal * Time.second) % 60
                            , round model.timeGoal % 60
                            ]) |> Graphics.Element.width 200
                        , spacer 10 10
                        , (button (Signal.message address StartSession) "Start New Sprint") |> Graphics.Element.width 200
                        , if lastSession /= Nothing then (Text.fromString "(Overwrites last sprint)" |> Text.color red |> Text.monospace |> centered |> container 200 35 midTop) else spacer 0 0
                        , if lastSession /= Nothing then (button (Signal.message address ContinueLastSprint) "Continue Last Sprint" |> Graphics.Element.width 200 ) else spacer 0 0
                        ] |> container width 300 middle
                    ]
                |> container width height middle
                |> Html.fromElement

            Peeking ->
                flow down
                [ container (round <| toFloat width * completion) 5 topLeft empty |> color green
                , Html.textarea [style [("width", toString (width-10) ++ "px"), ("height", toString (height - 15) ++ "px"), ("cursor", "not-allowed")], disabled True, class "peek"] [ Html.text model.text ] |> Html.toElement width (height-5)
                ] |> Html.fromElement

            DoneWithSession ->
                Html.textarea [style [("width", toString (width-10) ++ "px"), ("height", toString (height-10) ++ "px")], on "input" targetValue (\val -> Signal.message address (UpdateText val)) ] [ Html.text model.text ]

---- WIRING ----

main =
     app.html

app =
    StartApp.start
        { init = (initialModel, Effects.none)
        , view = view
        , update = update
        , inputs = [Signal.map EnterChar charPress, esc, Signal.map WindowResize windowDimensions, timer]
        }

esc : Signal Action
esc = Signal.sampleOn (Keyboard.isDown 27) (Signal.constant Peek)

timer : Signal Action
timer = Signal.map TimerTick <| Time.every Time.second

port charPress : Signal String

port writingMode : Signal Bool
port writingMode = Signal.map (.mode >> flip List.member [Writing, Peeking]) app.model |> Signal.dropRepeats

port session : Signal SavedSession
port session =
    app.model
    |> Signal.map
        (\model ->
            { text = model.text
            , wordGoal = model.wordGoal
            , timeGoal = model.timeGoal
            , timer = model.timer
            , mode = if model.mode == Peeking then toString Writing else toString model.mode
            }
        )
    |> Signal.dropRepeats
    |> Signal.filter (.mode >> (/=) (toString CreatingSession)) {text="",wordGoal=0,timeGoal=0,timer=0,mode=""}

port lastSession : Maybe SavedSession

-- Force initial window dimensions
windowDimensions =
  Signal.merge
    (Signal.sampleOn startAppMailbox.signal Window.dimensions)
    Window.dimensions

startAppMailbox =
  Signal.mailbox ()

port startApp : Signal (Task error ())
port startApp =
  Signal.constant (Signal.send startAppMailbox.address ())
