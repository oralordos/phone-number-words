module Main exposing (main)

import Html exposing (Html, div, input, li, p, text, ul)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)
import Browser


-- Model --

type alias Model = String

init : Model
init = ""


-- Update --

type Msg
  = UpdateNumber String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateNumber n -> n


-- View --

parseDigits : Char -> List Char
parseDigits c =
  case c of
    '0' -> [' ']
    '1' -> [' ']
    '2' -> ['a', 'b', 'c']
    '3' -> ['d', 'e', 'f']
    '4' -> ['g', 'h', 'i']
    '5' -> ['j', 'k', 'l']
    '6' -> ['m', 'n', 'o']
    '7' -> ['p', 'q', 'r', 's']
    '8' -> ['t', 'u', 'v']
    '9' -> ['w', 'x', 'y', 'z']
    _ -> []


wordCombinations : List (List Char) -> List (List Char)
wordCombinations charList =
  let
    wordCombiner : List Char -> List (List Char) -> List (List Char)
    wordCombiner w cs =
      case cs of
        [] -> [w]
        charChoice::css ->
          charChoice
          |> List.map (\c -> wordCombiner (c::w) css)
          |> List.concat
  in
    charList
    |> wordCombiner []
    |> List.map List.reverse


words : String -> List String
words phoneNumber =
  phoneNumber
  |> String.toList
  |> List.map parseDigits
  |> wordCombinations
  |> List.map String.fromList


wordToLi : String -> Html msg
wordToLi n =
  li []
    [ text n ]


numberListItems : String -> List (Html msg)
numberListItems phoneNumber =
  phoneNumber
  |> words
  |> List.map wordToLi


numberList : String -> Html msg
numberList phoneNumber =
  if String.isEmpty phoneNumber then
    ul [] []
  else if String.all Char.isDigit phoneNumber then
    ul [] (numberListItems phoneNumber)
  else
    p [class "error"] [text "All characters must be numbers!"]

view : Model -> Html Msg
view model =
  div []
    [ input [onInput UpdateNumber, placeholder "Phone Number"] []
    , numberList model
    ]

main : Program () Model Msg
main =
    Browser.sandbox
      { init = init
      , update = update
      , view = view
      }
