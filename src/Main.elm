module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button,input,p,h1)
import Html.Events exposing (onClick,onInput)
--import Time exposing (every, second)
--import Date exposing (year, hour, minute, second, fromTime)
import Html.Attributes exposing (id,value,placeholder)
import Task

--type alias Etats = {enCours: Bool,aVenir:bool,terminee: bool}
type States 
  = Pending
  | InProgress 
  | Ended 

type alias Task = (String,String,States)
type alias Model =
  { todos : List Task,
    currentTask : String,
    currentTitle : String}

type Msg
  = ButtonClickenCours
  | ClearList
  | ButtonClickenterminee
  | ButtonClickenaVenir
  | ButtonClickDelete Task
  | Change  String
  | ChangeTitle String


{-
currentTime t =
  let date' = fromTime t
      hour' = toString (Date.hour date')
      minute' = toString (Date.minute date')
      second' = toString (Date.second date')
      year' = toString (year date')
      now = "The current time is: " ++ hour' ++ ":" ++ minute' ++ ":" ++ second' in
  show now-}

{-
etatEnCours : Etats
etatEnCours = {enCours=True,aVenir=False,terminee=False}

etataVenir : Etats
etataVenir = {enCours=False,aVenir=True,terminee=False}

etatTerminee : Etats
etatTerminee = {enCours=False,aVenir=False,terminee=True}

stringFromEtat : Etats -> String
stringFromEtat value = case (value.enCours,value.aVenir,value.terminee) of
  (True,False,False) -> "En cours !"
  (False,True,False) -> "A venir !"
  (False,False,True) -> "En cours !"
  _ -> "Error de tâche "
-}

estPending etat =
  case etat of 
  (_,_,Pending)-> True
  _ -> False

estEnded etat =
  case etat of 
  (_,_,Ended)-> True
  _ -> False

estInProgress etat =
  case etat of 
  (_,_,InProgress)-> True
  _ -> False



stringFromEtat : States -> String
stringFromEtat value = case value of
  Pending -> "A venir !"
  InProgress -> "en cours !"
  Ended -> "Terminus !"


initialModel : Model
initialModel =
  { todos =
    [ 
    ],
    currentTask="",
    currentTitle = ""
  }

deleteTask : Task -> List Task -> List Task -> List Task
deleteTask task listTask acc=
  case (listTask, task) of
    ([],_)-> acc
    ((titlehead,bodyHead,stateHead)::tail, (title,bodyTask,_)) -> 
      if titlehead  /= title && bodyTask/=bodyHead then
        deleteTask task tail ((titlehead,bodyHead,stateHead)::acc)
      else
        List.append acc tail

update : Msg -> Model -> Model
update msg model =
  case msg of
    ButtonClickenCours ->
      { model |
        todos = (model.currentTitle,model.currentTask,InProgress) :: model.todos}
    ButtonClickenterminee ->
      { model |
        todos = (model.currentTitle,model.currentTask,Ended):: model.todos}
    ButtonClickenaVenir ->
      { model |
        todos = (model.currentTitle,model.currentTask,Pending) :: model.todos}
    Change newContent -> { model | currentTask = newContent}
    ChangeTitle newContentTitle -> { model | currentTitle = newContentTitle}
    ClearList ->
      { model | todos = [] }
    ButtonClickDelete taskToDelete ->
      { model | todos = deleteTask taskToDelete model.todos []}


viewTodo : Task -> Html Msg
viewTodo todo =
  case todo of
  (title,textValue,etat) -> --let time = currentTime (Time.every Time.second) in
                      Html.p [] [ Html.h1[][text title], text (textValue ++(" : ") ++ (stringFromEtat etat)),button [ onClick (ButtonClickDelete todo)] [ text "X" ],button [ onClick (ButtonClickDelete todo)] [ text "DOWN" ],button [ onClick (ButtonClickDelete todo)] [ text "UP" ] ]

viewEtat : States -> Html Msg
viewEtat etat =
  Html.p [] [ text (stringFromEtat etat) ]

view : Model -> Html Msg
view model =
  --let todos = (List.map viewTodo model.todos) in
  --let etats = (List.map viewEtat model.etats) in
  let inprogressTask = List.map viewTodo (List.filter estInProgress model.todos) in
  let pendingTask = List.map viewTodo (List.filter estPending model.todos) in
  let endedTask = List.map viewTodo (List.filter estEnded model.todos) in
  div
    [ id "First" ]
    [ div [ id "Second" ][]
    , input [ placeholder "Give me a title ", value model.currentTitle, onInput ChangeTitle ] [text ""]
    ,input [ placeholder "Give me your task ", value model.currentTask, onInput Change ] [text ""]
    , button [ onClick ButtonClickenCours ] [ text "Inprogress" ]
    , button [ onClick ButtonClickenterminee ] [ text "Ended" ]
    , button [ onClick ButtonClickenaVenir ] [ text "Pending" ]
    , button [ onClick ClearList ] [ text "Clear list" ]
    ,div[] (List.append[text "Inprogress task" ] inprogressTask)
    ,div[] (List.append[text "Pending task"] pendingTask)
    ,div[] (List.append[text "Ended task" ] endedTask)]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , update = update
    , view = view
    }
