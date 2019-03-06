module Main exposing (main)

import Time
import Strftime
import Task
import Browser.Navigation
import Url
import Browser
import Element exposing (Element, centerX, centerY, spacing, column, row, width, height, px, el, alignLeft, alignRight)
import Json.Decode as JD
import Http
import List
import Debug
import Result
import ISO8601
import Element.Events exposing (onClick)
import Element.Input
import Element.Font as Font
import Element.Background as Background


-- *** Constants ***

-- Since the challenge reads that we only need to deal with two stops, this can
-- be hard-coded. These are the stop IDs (the property "id") fetched from
-- https://api-v3.mbta.com/stops?filter[route_type]=2.
validStops = ["South Station", "North Station"]

-- In case we are out of bound for stops.
defaultStop = "South Station"

schedulesUrlPrefix = "https://api-v3.mbta.com/schedules?include=prediction,trip"

-- What to display when there is no prediction data.
defaultStatus = "ON TIME"

defaultTrackNumber = "TBD"

-- In milliseconds
timeRefreshPeriod = 1000
boardRefreshPeriod = 1000 * 30

defaultWidth = width (px 100)
defaultHeight = height (px 30)


-- *** Application model ***

type alias Flags = {}

type alias Model =
  { stopToDisplayForIndex : Int
  , currentDateTime : Maybe Time.Posix
  , currentTimeZone : Time.Zone
  , boardEntries : List Entry
  }

type alias Entry =
  { carrier : String
  , time : Time.Posix
  , destination : String
  -- Train number is simply an identifier, so a string is more appropriate.
  , trainNumber : String
  -- Track number is simply an identifier, so a string is more appropriate.
  , trackNumber : String
  , status : String
  }

initialModel : Model
initialModel =
  { stopToDisplayForIndex = 0
  , currentDateTime = Nothing
  , currentTimeZone = Time.utc
  , boardEntries = []
  }

type Msg
  = NoOp
  | Initialize (List Msg)
  | SetTimeZone Time.Zone
  | UpdateTime Time.Posix
  | ToggleStop
  | GotSchedules (Result Http.Error ReturnPackage)
  | UpdateBoardEntries Time.Posix

type alias ReturnPackage = List Record

type Record
  = EmptyRecord
  | ScheduleRecord Schedule
  | TripRecord Trip
  | PredictionRecord Prediction

type alias Schedule =
  { id : String
  , departureTime : Time.Posix
  , tripId : String
  , predictionId : Maybe String
  }

type alias Trip =
  { id : String
  , headSign : String
  , name : String
  }

type alias Prediction =
  { id : String
  , status : String
  , departureTime : Maybe Time.Posix
  , stopId : String
  }


-- *** Application logic ***

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every timeRefreshPeriod UpdateTime
    , Time.every boardRefreshPeriod UpdateBoardEntries
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    NoOp -> (model, Cmd.none)
    Initialize msgs ->
      let
        f msg (mdl, cmd) = update msg mdl
      in
        List.foldl f (model, Cmd.none) msgs
    SetTimeZone timezone ->
      ({ model | currentTimeZone = timezone }, Task.perform UpdateTime Time.now)
    UpdateTime newTime ->
      let
        currentTime = model.currentDateTime
      in
        ( { model | currentDateTime = Just newTime }
        , case currentTime of
            Nothing -> Task.perform UpdateBoardEntries Time.now
            Just x -> Cmd.none
        )
    UpdateBoardEntries _ ->
      case model.currentDateTime of
        Just dateTime ->
          (model, getBoardEntries model.currentTimeZone dateTime (getStopToDisplayFor model.stopToDisplayForIndex))
        Nothing -> (model, Cmd.none)
    ToggleStop ->
      let
        incremented = model.stopToDisplayForIndex + 1
        newIndex =
          if incremented < List.length validStops
             then incremented
             else 0
        refreshRecords = Task.perform UpdateBoardEntries Time.now
      in
        ({ model | stopToDisplayForIndex = newIndex }, refreshRecords)
    GotSchedules (Result.Ok package) ->
      let
        stop = getStopToDisplayFor model.stopToDisplayForIndex
      in
        ({ model | boardEntries = schedulePackageToBoardEntries stop package }, Cmd.none)
    GotSchedules (Result.Err httpError) ->
      let
        errorMessage =
          case httpError of
            Http.BadUrl url -> "Bad url: " ++ url
            Http.Timeout -> "API timed out"
            Http.NetworkError -> "Network error"
            Http.BadStatus statusCode -> "Status code: " ++ String.fromInt statusCode
            Http.BadBody body -> "Bad body: " ++ body
      in
        Debug.log errorMessage (model, Cmd.none)

schedulePackageToBoardEntries : String -> ReturnPackage -> List Entry
schedulePackageToBoardEntries stopToDisplayForIndex package =
  let
    schedules = List.filterMap getSchedule package
    getSchedule x =
      case x of
        ScheduleRecord record -> Just record
        _ -> Nothing
    entries = List.map (scheduleToBoardEntry package) schedules
    fromStop = List.filter (\x -> x.destination /= stopToDisplayForIndex) entries
    sorted = List.sortBy (\x -> Time.posixToMillis (.time x)) fromStop
  in
    List.take 15 sorted

scheduleToBoardEntry : ReturnPackage -> Schedule -> Entry
scheduleToBoardEntry package schedule =
  let
    (destination, trainNumber) =
      case findRecordById schedule.tripId package of
        TripRecord x -> (x.headSign, x.name)
        _ -> ("-", "-")
    defaultPrediction = (defaultStatus, schedule.departureTime, defaultTrackNumber)
    (status, departureTime, trackNumber) =
      case schedule.predictionId of
        Nothing -> defaultPrediction
        Just id ->
          case findRecordById id package of
            PredictionRecord prediction ->
              case (prediction.departureTime, getTrackNumber prediction.stopId) of
                (Just time, track) -> (prediction.status, time, track)
                (Nothing, track) -> (prediction.status, schedule.departureTime, track)
            _ -> defaultPrediction
  in
    { carrier = "MBTA"
    , time = departureTime
    , destination = destination
    , trainNumber = trainNumber
    , trackNumber = trackNumber
    , status = status
    }

getTrackNumber : String -> String
getTrackNumber stopId =
  case String.split "-" stopId of
    [_, trackNumber] -> trackNumber
    _ -> defaultTrackNumber

findRecordById : String -> List Record -> Record
findRecordById id records =
  let
    found = List.filter (doesRecordMatchId id) records
  in
    case List.head found of
      Just x -> x
      Nothing -> Debug.log ("Record for id '" ++ id ++ "' is not found. Data returned from the API is corrupted.") EmptyRecord

doesRecordMatchId : String -> Record -> Bool
doesRecordMatchId id x =
  case x of
    EmptyRecord -> False
    ScheduleRecord y -> y.id == id
    TripRecord y -> y.id == id
    PredictionRecord y -> y.id == id

getBoardEntries : Time.Zone -> Time.Posix -> String -> Cmd Msg
getBoardEntries timezone time stopId =
  let
    stopFilter = "&filter%5Bstop%5D=" ++ String.replace " " "%20" stopId
    timeFilter = "&filter%5Bmin_time%5D=" ++ Strftime.format "%H:%M" timezone time
    url = schedulesUrlPrefix ++ stopFilter ++ timeFilter
  in
    Http.get
      { url = url
      , expect = Http.expectJson GotSchedules schedulesDecoder
      }

schedulesDecoder : JD.Decoder ReturnPackage
schedulesDecoder =
  let
    scheduleD =
      JD.map ScheduleRecord
        <| JD.map4 Schedule
          (JD.field "id" JD.string)
          (JD.at ["attributes", "departure_time"] (JD.map ISO8601.toPosix ISO8601.decode))
          (JD.at ["relationships", "trip", "data", "id"] JD.string)
          (JD.at ["relationships", "prediction", "data"] (JD.nullable (JD.field "id" JD.string)))
    tripD =
      JD.map TripRecord
        <| JD.map3 Trip
          (JD.field "id" JD.string)
          (JD.at ["attributes", "headsign"] JD.string)
          (JD.at ["attributes", "name"] JD.string)
    predictionD =
      JD.map PredictionRecord
        <| JD.map4 Prediction
          (JD.field "id" JD.string)
          (JD.at ["attributes", "status"] JD.string)
          (JD.at ["attributes", "departure_time"] (JD.nullable (JD.map ISO8601.toPosix ISO8601.decode)))
          (JD.at ["relationships", "stop", "data", "id"] JD.string)
  in
    JD.map2 (++)
      (JD.field "data" (JD.list scheduleD))
      (JD.field "included" (JD.list (JD.oneOf [tripD, predictionD])))

getStopToDisplayFor : Int -> String
getStopToDisplayFor i =
  Maybe.withDefault defaultStop (List.head (List.drop i validStops))


-- *** UI ***

headerStyle =
  [ Font.family [ Font.sansSerif ]
  , Font.color (Element.rgb255 255 255 255)
  ]
monospaceFont = Font.family [ Font.monospace ]

view : Model -> Browser.Document Msg
view model =
  { title = "MBTA"
  , body = [ Element.layout
              [ Background.color (Element.rgb 0 0 0)
              , monospaceFont
              , Font.color (Element.rgb255 249 250 111)
              ]
              ( board model )
           ]
  }

board : Model -> Element Msg
board model =
  let
    toggleStop =
      [ Element.Input.button
          [ centerX
          , width (px 400)
          , height (px 50)
          ]
          { onPress = Just ToggleStop
          , label = el ( [ centerX ] ++ headerStyle ) (Element.text "Click to toggle between North/South station")
          }
      , Element.el [ height (px 50) ] (Element.text "")
      ]
    dateTime =
      case model.currentDateTime of
        Just x -> x
        Nothing -> Time.millisToPosix 0
    header =
      [ row [ defaultHeight ]
          [ column [ width (px 200) ]
              [ Element.text <| Strftime.format "%A" model.currentTimeZone dateTime ]
          , column [ width (px 450) ]
              [ Element.el ( [ centerX ] ++ headerStyle ) (Element.text <| String.toUpper (getStopToDisplayFor model.stopToDisplayForIndex) ++ " INFORMATION") ]
          , column [ width (px 200) ]
              [ Element.el [ alignRight ] (Element.text "CURRENT TIME") ]
          ]
      , row [ defaultHeight ]
          [ column [ width (px 425) ]
              [ Element.text <| Strftime.format "%m-%d-%Y" model.currentTimeZone dateTime ]
          , column [ width (px 425) ]
              [ Element.el [ alignRight ] (Element.text <| Strftime.format "%I:%M %p" model.currentTimeZone dateTime) ]
          ]
      , row [ defaultHeight ]
          [ column ( [ width (px 100) ] ++ headerStyle ) [ Element.text "CARRIER" ]
          , column ( [ width (px 150) ] ++ headerStyle ) [ Element.text "TIME" ]
          , column ( [ width (px 300) ] ++ headerStyle ) [ Element.text "DESTINATION" ]
          , column ( [ defaultWidth ] ++ headerStyle ) [ Element.text "TRAIN#" ]
          , column ( [ defaultWidth ] ++ headerStyle ) [ Element.text "TRACK#" ]
          , column ( [ defaultWidth ] ++ headerStyle ) [ Element.text "STATUS" ]
          ]
      ]
    entries = List.map (showEntry model.currentTimeZone) model.boardEntries
  in
    column [] (toggleStop ++ header ++ entries)

showEntry : Time.Zone -> Entry -> Element Msg
showEntry timezone entry =
  row [ defaultHeight ]
    [ column [ width (px 100) ] [ Element.text "MBTA" ]
    , column [ width (px 150) ] [ Element.text <| Strftime.format "%I:%M %p" timezone entry.time ]
    , column [ width (px 300) ] [ Element.text <| String.toUpper entry.destination ]
    , column [ defaultWidth ] [ Element.text <| String.toUpper entry.trainNumber ]
    , column [ defaultWidth ] [ Element.text <| String.toUpper entry.trackNumber ]
    , column [ defaultWidth ] [ Element.text <| String.toUpper entry.status ]
    ]


-- *** Entry point ***

init : Flags -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init flags url key = (initialModel, Task.perform SetTimeZone Time.here)

main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = \_ -> NoOp
    , onUrlRequest = \_ -> NoOp
    }
