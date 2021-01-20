module Main exposing (main)

import Browser
import Browser.Dom
import Dict as D exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Task
import Json.Decode as JD
import Parser as P exposing (Parser, (|.), (|=))
import Set as S
import Levenshtein

type alias Uid = Int
type alias Message = { sender : Uid , content : List MessagePart }
type alias Petnames = Dict Uid String
type MessagePart = StringPart String | UidPart Uid
type alias UidRenderConfig =
  { petnames : Petnames
  , focusedUid : Maybe Uid
  }

type alias Model =
    { petnames : Petnames
    , focusedUid : Maybe Uid
    , draft : String
    , messages : List Message
    , loggedInUid : Uid
    , userInfo : Dict Uid {preferredNickname : String}
    }


init : () -> ( Model , Cmd Msg )
init _ =
  ( { petnames = D.fromList
        [ (13786, "self")
        , (12345, "Eurokarte")
        ]
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
    , userInfo = D.fromList
        [ (14804, {preferredNickname="Donut"})
        , (4506, {preferredNickname="Eurakarte"})
        , (11496, {preferredNickname="Miles Prower"})
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
  let
    uidcfg : UidRenderConfig
    uidcfg = { petnames = model.petnames , focusedUid = model.focusedUid }
  in
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
                                  , case D.get uid model.userInfo of
                                      Nothing -> H.text ""
                                      Just {preferredNickname} ->
                                        H.span []
                                          [ H.text <| " (User's preferred nickname is " ++ preferredNickname ++ "; "
                                          , case model.petnames
                                                  |> D.toList
                                                  |> List.filter (\(_, existingName) -> resembles existingName preferredNickname)
                                                  |> List.map (\(u, _) -> u)
                                                  of
                                              [] -> H.button [HE.onClick (SetPetname uid preferredNickname)] [H.text "use it?"]
                                              similarUids -> H.span []  [ H.text "but that resembles your petnames for: "
                                                                        , H.span [] <| List.map (viewUid uidcfg) similarUids
                                                                        ]
                                          , H.text ")"
                                          ]
                                  ]
            Nothing -> H.div [HA.style "color" "red"] [H.text "Click on a user-id (u/...) to set a petname for them."]
        , model.messages
            |> List.map (viewMessage uidcfg)
            |> H.div []
        , let
            rendered : List MessagePart
            rendered =
              case P.run (messagePartsParser model.petnames) model.draft of
                Ok mps -> mps
                Err e -> Debug.todo <| Debug.toString e
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
            , viewMessage uidcfg {sender=model.loggedInUid, content=rendered}
            ]
        ]
        
viewMessage : UidRenderConfig -> Message -> Html Msg
viewMessage uidcfg {sender, content} =
    H.div [HA.style "font-family" "'Courier New', Courier, monospace"]
        [ viewUid uidcfg sender
        , H.text ": "
        , content |> List.map (viewMessagePart uidcfg) |> H.span []
        ]

viewUid : UidRenderConfig -> Uid -> Html Msg
viewUid {petnames, focusedUid} uid =
  let
    background =
      if focusedUid == Just uid then
        "lightgray"
      else if D.member uid petnames then
        "lightgreen"
      else
        "pink"
    
    text = "@" ++ (D.get uid petnames |> Maybe.withDefault ("u/" ++ String.fromInt uid))
  in
    H.strong
      [ HE.onClick (FocusUid (Just uid))
      , HA.style "background-color" background
      ]
      [H.text text]

viewMessagePart : UidRenderConfig -> MessagePart -> Html Msg
viewMessagePart cfg part =
  case part of
    StringPart s -> H.text s
    UidPart uid -> viewUid cfg uid

messagePartsParser : Petnames -> P.Parser (List MessagePart)
messagePartsParser petnames =
  P.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = P.symbol ""
    , item = P.oneOf
        [ P.succeed UidPart
            |. P.backtrackable (P.symbol "@u/")
            |= P.int
        , P.succeed UidPart
            |. P.backtrackable (P.symbol "@")
            |= P.oneOf (petnames
                        |> D.toList
                        |> List.map (\(uid, petname) -> P.map (always uid) (P.keyword petname))
                        )
        , P.succeed (StringPart "@")
            |. P.symbol "@"
        , P.succeed StringPart
            |= P.variable
                { start = (always True)
                , inner = (\c -> c /= '@')
                , reserved = S.empty
                }
        ]
    , trailing = P.Forbidden
    }

onEnter : Msg -> H.Attribute Msg
onEnter m =
  HE.on "keydown" (JD.map (\k -> if k == 13 then m else Ignore) HE.keyCode)

resembles : String -> String -> Bool
resembles s t =
  -- string resemblance is hard so I'm gonna phone it in
  (toFloat <| Levenshtein.distance s t) < 0.3 * (toFloat <| String.length s)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
