import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Signal exposing (..)
import String exposing (..)
import StartApp.Simple exposing (start)
import Debug exposing (..)

type alias Model = {
  memory : Array Int,
  pointer : Int,
  stack : List Int,
  source : String,
  sourceIndex : Int,
  running : Bool
}

type Action
  = Next
  | Run
  | Stop
  | Source String

initMemory : Array Int
initMemory = Array.repeat 16 0

model : Model
model = {
    memory = initMemory,
    pointer = 0,
    stack = [],
    source = "",
    sourceIndex = 0,
    running = False
  }

incSource : Model -> Model
incSource model =
  let
    newModel = { model | sourceIndex = model.sourceIndex + 1 }
    start = newModel.sourceIndex
    end = newModel.sourceIndex + 1
    ch = String.slice start end model.source
  in
    if (start >= String.length model.source) || (isToken ch) then
      newModel
    else
      incSource newModel

incPointer : Model -> Model
incPointer model = { model |
    pointer = model.pointer + 1
  }

decPointer : Model -> Model
decPointer model = { model |
    pointer = model.pointer - 1
  }

incData : Model -> Model
incData model = { model |
    memory = Array.indexedMap (\i v -> if i == model.pointer then v + 1 else v) model.memory
  }

decData : Model -> Model
decData model = { model |
    memory = Array.indexedMap (\i v -> if i == model.pointer then v - 1 else v) model.memory
  }

skipBlock : Int -> Model -> Model
skipBlock counter model =
  let
    start = model.sourceIndex
    end = model.sourceIndex + 1
  in
    if start >= String.length model.source || counter == 0 then
      model
    else
      case String.slice start end model.source of
        "[" -> skipBlock (counter+1) <| incSource model
        "]" -> skipBlock (counter-1) <| incSource model
        _ -> skipBlock counter <| incSource model

pushStack : Model -> Model
pushStack model =
  let
    value = Array.get model.pointer model.memory
  in
    case value of
      Just value ->
        if value /= 0 then
          incSource { model | stack = model.sourceIndex :: model.stack }
        else
          skipBlock 1 <| incSource model

      Nothing -> model

popStack : Model -> Model
popStack model =
  let
    head = List.head model.stack
  in
    case head of
      Just index -> { model | sourceIndex = index, stack = List.drop 1 model.stack }
      Nothing -> model

execute : Model -> Model
execute model =
  let
    start = model.sourceIndex
    end = model.sourceIndex + 1
  in
    case String.slice start end model.source of
      ">" -> incSource <| incPointer model
      "<" -> incSource <| decPointer model
      "+" -> incSource <| incData model
      "-" -> incSource <| decData model
      "." -> incSource <| model
      "," -> incSource <| model
      "[" -> pushStack model
      "]" -> popStack model
      _ -> incSource model
  
update : Action -> Model -> Model
update action model =
  case action of
    Next -> execute model

    Run -> { model | memory = initMemory, pointer = 0, sourceIndex = 0, running = True }

    Stop -> { model | running = False }

    Source source -> { model | source = source }

viewCellStyle : Bool -> Attribute
viewCellStyle selected = Html.Attributes.style
  [ ("float", "left")
  , ("width", "35px")
  , ("height", "35px")
  , ("background-color", "lightgray")
  , ("text-align", "center")
  , ("vertical-align", "middle")
  , ("line-height", "35px")
  , if selected then  ("border", "2px solid red") else ("border", "2px solid white")
  ]

viewMemoryCell : Model -> Int -> Int -> Html
viewMemoryCell model index value =
  Html.div [ viewCellStyle (model.pointer == index) ] [
    Html.text <| toString value
  ]

styleRow : Attribute
styleRow = Html.Attributes.style
  [ ("margin-top", "10px")
  , ("margin-bottom", "10px")
  ]

styleClear : Attribute
styleClear = Html.Attributes.style [ ("clear", "both") ]

viewMemory : Signal.Address Action -> Model -> Html
viewMemory address model =
  Html.div [ styleRow ] [
    Html.div [] <| Array.toList <| Array.indexedMap (\i v -> viewMemoryCell model i v) model.memory,
    Html.div [ styleClear ] []
  ]

viewSourceCell : Model -> Int -> Char -> Html
viewSourceCell model index value =
  Html.div [ viewCellStyle (model.sourceIndex == index) ] [
    Html.text <| String.fromChar value
  ]

isToken : String -> Bool
isToken ch =
  case ch of
    ">" -> True 
    "<" -> True 
    "+" -> True 
    "-" -> True 
    "." -> True 
    "," -> True 
    "[" -> True 
    "]" -> True 
    _ -> False

viewSource : Signal.Address Action -> Model -> Html
viewSource address model =
  let
    charMapped = List.indexedMap (,) <| String.toList model.source

    tokens = List.filter (\(_,ch) -> isToken <| String.fromChar ch) charMapped 
  in
    Html.div [ styleRow ] [
      Html.div [] <| List.map (\(index, ch) -> viewSourceCell model index ch) tokens,
      Html.div [ styleClear ] []
    ]

handleInput : Signal.Address Action -> Attribute
handleInput address =
  on "input" targetValue (\str -> Signal.message address (Source str))

inputStyle : Attribute
inputStyle = Html.Attributes.style
  [ ("font-size", "x-large")
  , ("width", "300px")
  , ("height", "300px")
  , ("margin", "10px")
  ]

isRunning : Model -> Bool
isRunning model =
  model.sourceIndex < String.length model.source && model.running

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] [
    Html.text "Memory",
    viewMemory address model,
    Html.text "Source",
    viewSource address model,
    Html.div [] [
      Html.button [onClick address Next, Html.Attributes.disabled <| not <| isRunning model] [ Html.text "Next" ],
      Html.button [onClick address Run, Html.Attributes.disabled <| isRunning model] [ Html.text "Run" ],
      Html.button [onClick address Stop, Html.Attributes.disabled <| not <| isRunning model] [ Html.text "Stop" ]
    ],
    Html.textarea [ handleInput address, inputStyle, Html.Attributes.disabled <| isRunning model ] [
      Html.text model.source
    ]
  ]

main : Signal Html
main =
  StartApp.Simple.start {
    model = model,
    update = update,
    view = view
  }

