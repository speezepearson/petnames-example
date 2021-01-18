module Main exposing (main)

import Browser
import Browser.Dom
import Dict as D exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Task
import Json.Decode as JD

type alias Uid = Int
type alias Message = { sender : Uid , content : List MessagePart }
type MessagePart = StringPart String | UidPart Uid


type alias Model =
    { petnames : Dict Uid String
    , focusedUid : Maybe Uid
    , draft : String
    , messages : List Message
    , loggedInUid : Uid
    }


init : () -> ( Model , Cmd Msg )
init _ =
  ( { petnames = D.singleton 13786 "self"
    , focusedUid = Nothing
    , draft = ""
    , loggedInUid = 13786
    , messages = 
      [ { sender=14804, content=[StringPart "HEY ", UidPart 4506] }
      , { sender=14804, content=[StringPart "INSULT"] }
      , { sender=4506, content=[StringPart "RETORT"] }
      , { sender=14804, content=[StringPart "COUNTER-RETORT"] }
      , { sender=4506, content=[StringPart "QUESTIONING OF SEXUAL PREFERENCE"] }
      , { sender=14804, content=[StringPart "SUGGESTION TO SHUT THE FUCK UP"] }
      , { sender=4506, content=[StringPart "NOTATION THAT YOU CREATE A VACUUM"] }
      , { sender=14804, content=[StringPart "RIPOSTE"] }
      , { sender=14804, content=[StringPart "ADDON RIPOSTE"] }
      , { sender=4506, content=[StringPart "COUNTER-RIPOSTE"] }
      , { sender=14804, content=[StringPart "COUNTER-COUNTER RIPOSTE"] }
      , { sender=4506, content=[StringPart "NONSENSICAL STATEMENT INVOLVING PLANKTON"] }
      , { sender=11496, content=[StringPart "RESPONSE TO RANDOM STATEMENT AND THREAT TO BAN OPPOSING SIDES"] }
      , { sender=4506, content=[StringPart "WORDS OF PRAISE FOR FISHFOOD"] }
      , { sender=11496, content=[StringPart "ACKNOWLEDGEMENT AND ACCEPTENCE OF TERMS"] }
      ]
    }
  , Task.attempt (always Ignore) <| Browser.Dom.focus "draft"
  )


type Msg
    = SetPetname Uid String
    | FocusUid (Maybe Uid)
    | SetDraft String
    | SendDraft (List MessagePart)
    | Ignore


update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
  case msg of
    SetPetname uid name ->
      ( { model | petnames = model.petnames |> (if String.length name > 0 then D.insert uid name else D.remove uid) }
      , Cmd.none
      )
        
    FocusUid uid ->
      ( { model | focusedUid = uid }
      , case uid of
          Just _ -> Task.attempt (always Ignore) <| Browser.Dom.focus "petname-field"
          Nothing -> Task.attempt (always Ignore) <| Browser.Dom.focus "draft"
      )

    SetDraft s ->
      ( { model | draft = s }
      , Cmd.none
      )

    SendDraft content ->
      ( { model | draft = "", messages = model.messages ++ [{sender=model.loggedInUid, content=content}]}
      , Cmd.none
      )

    Ignore ->
      ( model
      , Cmd.none
      )


view : Model -> Html Msg
view model =
    H.div [HA.style "padding" "1em"]
        [ case model.focusedUid of
            Just uid -> H.div [] [ H.text <| "User id " ++ String.fromInt uid ++ ": petname = "
                                 , H.input
                                    [ HE.onInput (SetPetname uid)
                                    , onEnter (FocusUid Nothing)
                                    , HA.placeholder "display name"
                                    , HA.id "petname-field"
                                    , HA.value (D.get uid model.petnames |> Maybe.withDefault "")
                                    ]
                                    []
                                 ]
            Nothing -> H.div [HA.style "color" "red"] [H.text "Click on a user-id (u/...) to set a petname for them."]
        , model.messages
            |> List.map (viewMessage model.petnames)
            |> H.div []
        , let
            rendered : List MessagePart
            rendered = parseMessageParts {loggedInUid=model.loggedInUid, petnames=model.petnames, raw=model.draft}
          in
            H.div [HA.style "background-color" "#cccccc", HA.style "padding" "0.5em"]
            [ H.text "Type a message, reference a user with '@petname' or '@u/12345', and hit Enter to send:"
            , H.br [] []
            , H.input
                [ HE.onInput SetDraft
                , onEnter (SendDraft rendered)
                , HA.id "draft"
                , HA.value model.draft
                , HA.style "margin" "0.5em"
                , HA.style "width" "40em"
                ]
                []
            , H.br [] []
            , viewMessage model.petnames {sender=model.loggedInUid, content=rendered}
            ]
        ]
        
viewMessage : Dict Uid String -> Message -> Html Msg
viewMessage petnames {sender, content} =
    H.div []
        [ viewUid petnames sender
        , H.text ": "
        , content |> List.map (viewMessagePart petnames) |> H.span []
        ]

viewUid : Dict Uid String -> Uid -> Html Msg
viewUid petnames uid =
    case D.get uid petnames of
        Just name -> H.strong [HE.onClick (FocusUid (Just uid)), HA.style "outline" "1px solid lightgreen"] [H.text <| "@" ++ name]
        Nothing -> H.strong [HE.onClick (FocusUid (Just uid)), HA.style "outline" "1px solid red"] [H.text <| "@u/" ++ (String.fromInt uid)]

viewMessagePart : Dict Uid String -> MessagePart -> Html Msg
viewMessagePart petnames part =
  case part of
    StringPart s -> H.text s
    UidPart uid -> viewUid petnames uid

parseMessageParts : {loggedInUid : Uid, raw : String, petnames : Dict Uid String} -> List MessagePart
parseMessageParts {loggedInUid, raw, petnames} =
  let
    splitFirstPart : String -> Maybe (MessagePart, String)
    splitFirstPart rem =
      if rem == "" then
        Nothing 
      else Just <|
        case String.indices "@" rem of
          [] -> (StringPart rem, "")
          0 :: _ -> 
            let
              firstWord : String
              firstWord = case List.head <| String.words rem of
                Nothing -> Debug.todo "impossible"
                Just s -> s

              ident : String
              ident = String.dropLeft 1 firstWord

              nextRem : String
              nextRem = String.dropLeft (String.length firstWord) rem

              -- _ = Debug.log <| Debug.toString {firstWord=firstWord, ident=ident, nextRem=nextRem}
              
              referencedUid : Maybe Uid
              referencedUid =
                if String.startsWith "u/" ident
                  then String.toInt (String.dropLeft 2 ident)
                  else 
                    petnames
                    |> D.toList
                    |> List.filter (\(uid, name) -> name == ident)
                    |> List.map (\(uid, name) -> uid)
                    |> List.head
            in
              case referencedUid of
                Nothing -> (StringPart ("@" ++ ident), nextRem)
                Just uid -> (UidPart uid, nextRem)
          i :: _ -> (StringPart (String.left i rem), String.dropLeft i rem)
  in
    unfold splitFirstPart raw

unfold : (seed -> Maybe (a, seed)) -> seed -> List a
unfold f seed =
  case f seed of
    Just (x, next) -> x :: unfold f next
    Nothing -> []

onEnter : Msg -> H.Attribute Msg
onEnter m =
  HE.on "keydown" (JD.map (\k -> if k == 13 then m else Ignore) HE.keyCode)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
