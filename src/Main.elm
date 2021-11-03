port module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element as E
import Element.Background as EBA
import Element.Border as B
import Element.Events as EV
import Element.Font as F
import Element.Input as I
import Json.Decode
import Json.Encode
import Url
import Webnative exposing (Artifact(..), DecodedResponse(..), State(..))
import Webnative.Path as Path exposing (Path)
import Wnfs



---------------------------------------- PORTS ----------------------------------------


port webnativeRequest : Webnative.Request -> Cmd msg

port webnativeResponse : (Webnative.Response -> msg) -> Sub msg

port signOut : () -> Cmd msg

---------------------------------------- FISSION SETUP ----------------------------------------


appPermissions : Webnative.AppPermissions
appPermissions =
    { creator = "Trillian"
    , name = "fission-with-elm"
    }


fsPermissions : Webnative.FileSystemPermissions
fsPermissions =
    { public =
        { directories = []
        , files = []
        }
    , private =
        { directories = []
        , files = []
        }
    }


permissions : Webnative.Permissions
permissions =
    { app = Just appPermissions
    , fs = Just fsPermissions
    }



---------------------------------------- ELM SETUP ----------------------------------------


type alias Flags =
    {}



-- The main function of the Elm program


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Initializes the Model and sends and Cmd Msg's that should be sent upon
-- initialization of the poject


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init {} url key =
    ( Model Nothing [] ""
    , permissions
        |> Webnative.init
        |> webnativeRequest
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    webnativeResponse GotWebnativeResponse



---------------------------------------- MODEL/UPDATE ----------------------------------------


type alias Model =
    { username : Maybe String
    , logs : List String
    , note : String
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotWebnativeResponse Webnative.Response
    | SignIn
    | SignOut
    | UpdateNote String
    | SaveNote

type Tag
    = WriteNote
    | PublishNote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked browser_request ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        GotWebnativeResponse response ->
            case Webnative.decodeResponse tagFromString response of
                Webnative (Initialisation state) ->
                    case state of 
                        NotAuthorised ->
                            ( { model
                                | username = Nothing
                                , logs = "Auth: NotAuthorised" :: model.logs
                            }
                            , Cmd.none
                            )

                        AuthSucceeded { username } ->
                            ( { model
                                | username = Just username
                                , logs =  "Auth: AuthSucceeded" :: model.logs
                            }
                            , Cmd.none
                            )

                        AuthCancelled _ ->
                            ( { model
                                | username = Nothing
                                , logs = "Auth: AuthCancelled" :: model.logs
                            }
                            , Cmd.none
                            )

                        Continuation { username } ->
                            ( { model
                                | username = Just username
                                , logs = "Auth: Continuation" :: model.logs
                            }
                            , Cmd.none
                            )
                
                Wnfs WriteNote _ ->
                    ( { model
                        | logs = "Note Written..." :: model.logs
                    }
                    , ({ tag = tagToString PublishNote }
                        |> Wnfs.publish
                        |> webnativeRequest)
                    )

                Wnfs PublishNote _ ->
                    ( {model
                        | logs = "Note Published to Fission Drive!" :: model.logs
                      }
                    , Cmd.none
                    ) 

                
                Webnative (Webnative.NoArtifact _) ->
                    ( model
                    , Cmd.none
                    )

                WebnativeError err ->              
                    ( { model
                        | logs = Webnative.error err :: model.logs
                    }
                    , Cmd.none
                    )

                WnfsError err ->
                    ( { model
                        | logs = Wnfs.error err :: model.logs
                    }
                    , Cmd.none
                    )

        SignIn ->
            ( { model
                | logs = "SignIn" :: model.logs
              }
            , permissions
                |> Webnative.redirectToLobby Webnative.CurrentUrl
                |> webnativeRequest )

        SignOut ->
            ( { model
                | username = Nothing
                , logs = "SignOut" :: model.logs
            }
            , signOut ()
            )

        UpdateNote str ->
            ( { model | note = str }
            , Cmd.none )

        SaveNote ->
            ( { model
                | note = ""
                , logs = "Saving Note..." :: model.logs
              }
            , webnativeRequest <| save model.note
            )
            
---------------------------------------- HELPER FUNCTIONS ----------------------------------------

tagFromString : String -> Result String Tag
tagFromString str =
    case str of
        "WriteNote" ->
            Ok WriteNote

        "PublishNote" ->
            Ok PublishNote

        _ ->
            Err <| "Invalid tag: " ++ str

tagToString : Tag -> String
tagToString tag =
    case tag of 
        WriteNote ->
            "WriteNote"

        PublishNote ->
            "PublishNote"

encode : String -> Json.Decode.Value
encode note =
    Json.Encode.object
        [ ("note", Json.Encode.string note)            
        ]

save : String -> Webnative.Request
save note =
    note    
        |> encode
        |> Json.Encode.encode 0
        |> Wnfs.writeUtf8
            Wnfs.Private
            { path = Path.file [ "Apps", "Trillian", "fission-with-elm", "notes", "note.json"]
            , tag = tagToString WriteNote
            }

checkUsername : Maybe String -> ( String, Bool )
checkUsername m_username =
    case m_username of
        Just username ->
            ( username, True )

        Nothing ->
            ( "Login", False )


---------------------------------------- VIEW ----------------------------------------

view : Model -> Browser.Document Msg
view model =
    let
        ( username, logged_in ) =
            checkUsername model.username

        attr_list =
            [ E.width E.fill
            , E.height E.fill
            , EBA.color <| E.rgb255 176 192 222
            ]

        ( attr_list_final, page_view ) =
            if logged_in then
                ( attr_list, baseLayer model username )

            else
                ( loginLayer :: attr_list, E.none )
    in
    { title = "Fission with Elm"
    , body =
        [ E.layout
            attr_list_final
          <|
            page_view
        ]
    }


loginLayer : E.Attribute Msg
loginLayer =
    E.inFront <|
        E.row
            [ E.width E.fill
            , E.height E.fill
            , EBA.color <| E.rgba255 0 0 0 0.9
            ]
            [ E.row
                [ E.centerX
                , E.centerY
                , E.spacingXY 25 0
                , E.pointer
                , F.color <| E.rgb255 255 255 255
                , EV.onClick SignIn
                ]
                [ E.image
                    [ E.width <| E.px 175
                    ]
                    { src = "assets/images/fission.svg"
                    , description = "fission logo"
                    }
                , E.el
                    [ F.size 90
                    , F.bold
                    ]
                  <|
                    E.text "Login"
                ]
            ]


baseLayer : Model -> String -> E.Element Msg
baseLayer model username =
    E.column
        [ E.width E.fill
        , E.height E.fill
        ]
        [ titleBar username
        , E.row
            [ E.width E.fill
            , E.height E.fill
            ]
            [ leftColumn model.logs
            , centerColumn model
            , rightColumn
            ]
        ]


centerColumn : Model -> E.Element Msg
centerColumn model =
    E.column
        [ E.width <| E.fillPortion 5
        , E.height E.fill
        , E.paddingXY 50 0
        ]
        [ E.column
            [ E.padding 42
            , E.width E.fill
            , E.centerY
            , E.centerX
            , E.spacing 20
            , EBA.color <| E.rgba255 0 0 0 0.1
            , B.rounded 25
            ]
            [ I.multiline
                [ E.width E.fill
                , E.height <| E.px 250
                ]
                { onChange =
                    \str ->
                        UpdateNote str
                , text = model.note
                , placeholder =
                    Just
                        (I.placeholder
                            []
                         <|
                            E.text "Type here..."
                        )
                , label =
                    I.labelAbove
                        [ F.center
                        , E.moveUp 20
                        ]
                    <|
                        E.text "Enter the text you want to see in your Fission Drive!"
                , spellcheck = True
                }
            , E.el
                [ B.rounded 25
                , B.width 1
                , EBA.color <| E.rgb255 175 175 175
                , E.padding 10
                , E.alignRight
                , E.pointer
                , EV.onClick SaveNote
                ]
              <|
                E.text "Save note"
            ]
        ]


leftColumn : List String -> E.Element Msg
leftColumn logs =
    E.column
        [ E.height <| E.px 700
        , E.width <| E.fillPortion 1
        , E.padding 20
        , E.scrollbarY
        ]
    <|
        List.map displayLogs logs


displayLogs : String -> E.Element Msg
displayLogs message =
    E.paragraph
        [ E.paddingXY 0 7
        ]
        [ E.text message
        ]


rightColumn : E.Element Msg
rightColumn =
    E.column
        [ E.height E.fill
        , E.width <| E.fillPortion 1
        ]
        [ E.none
        ]


titleBar : String -> E.Element Msg
titleBar username =
    E.row
        [ E.width E.fill
        , E.height <| E.px 125
        , B.shadow
            { offset = ( 0, 1 )
            , size = 1
            , blur = 20
            , color = E.rgb255 0 0 0
            }
        , EBA.color <| E.rgba255 255 255 255 0.8
        ]
        [ E.column
            [ E.centerX
            , E.width <| E.px 500
            ]
            [ E.el
                [ E.centerX
                , E.padding 15
                , F.bold
                , F.size 25
                ]
              <|
                E.text "Fission with Elm"
            , E.row
                [ E.centerX
                , E.spacingXY 42 0
                , E.width <| E.px 300
                ]
                [ E.row
                    [ B.width 1
                    , B.rounded 25
                    , E.centerX
                    , E.height E.fill
                    ]
                    [ E.image
                        [ E.width <| E.px 40
                        ]
                        { src = "assets/images/fission.svg"
                        , description = "fission logo"
                        }
                    , E.el
                        [ E.padding 10
                        ]
                      <|
                        E.text username
                    ]
                , E.row
                    [ EV.onClick SignOut
                    , E.height E.fill
                    , E.pointer
                    , E.centerX
                    , B.width 1
                    , B.rounded 25
                    , E.paddingXY 15 5
                    , EBA.color <| E.rgba255 125 125 125 0.4
                    ]
                    [ E.el
                        []
                      <|
                        E.text "Logout"
                    ]
                ]
            ]
        ]
