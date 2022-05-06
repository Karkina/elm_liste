module Simple exposing (..)

import Browser
import Html exposing (Html,Attribute,div,input, text, button)
import Html.Events exposing (onClick,onInput)
import Html.Attributes exposing (id,value,placeholder)



type alias Model =
  { todos : Int,
    text : String,
    hello : String }


type Msg
  = ButtonClick1
  | ButtonClick2
  | ButtonClick3
  | ButtonClick4
  | Change  String
  | Salutation

initialModel : Model
initialModel =
  { todos =0,
    text = "",
    hello = ""
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    ButtonClick1 ->
      { model |
        todos = model.todos + 1 }
    ButtonClick2 ->
      { model | todos = model.todos + 2   }
    ButtonClick3 ->
      { model |
        todos = model.todos - 1 }
    ButtonClick4 ->
      { model | todos = model.todos  - 2   }
    Change newContent-> { model | text = newContent}
    Salutation -> { model | hello = "Bonjour Madame/Monsieur " ++ model.text}

viewTodo : Int -> Html Msg
viewTodo todo =
  Html.p [] [ text (String.fromInt todo) ]

view : Model -> Html Msg
view model =
  let todos =  viewTodo model.todos in
  div [ id "First" ][
     div [ id "Second" ] [
     button [ onClick ButtonClick1 ] [ text "+1" ]
    , button [ onClick ButtonClick2 ] [ text "+2" ]
    ,  todos
    , button [ onClick ButtonClick3 ] [ text "-1" ]
    , button [ onClick ButtonClick4 ] [ text "-2" ]
    ],
    div [][ input [ placeholder "Give me your name ", value model.text, onInput Change ] [text ""],
    button[onClick Salutation][text "Bonjour"],
    text model.hello],
    div [][ input [ placeholder "Nombre un ", value model.text, onInput Change ] [text ""],
    button[onClick Salutation][text "Bonjour"],
    text model.hello],
    div [][ input [ placeholder "Nombre deux ", value model.text, onInput Change ] [text ""],
    button[onClick Salutation][text "Bonjour"],
    text model.hello]

  ]
    

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , update = update
    , view = view
    }
