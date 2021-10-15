port module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element as E
import Element.Background as EBA
import Element.Font as F
import Url
import Webnative
import Webnative exposing (Artifact(..))

---------------------------------------- PORTS ----------------------------------------


port webnativeRequest : Webnative.Request -> Cmd msg

port webnativeResponse : (Webnative.Response -> msg) -> Sub msg

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

-- The main function of the ELm program
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
init : Flags -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init {} url key =
    ( Model
    , permissions
        |> Webnative.init
        |> webnativeRequest
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    webnativeResponse GotWebnativeResponse


---------------------------------------- MODEL/UPDATE ----------------------------------------
-- Data central to the application. This application has no data.
type alias Model =
    { }


type Msg 
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotWebnativeResponse Webnative.Response

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked browser_request ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        GotWebnativeResponse response ->
            ( model, Cmd.none )

---------------------------------------- VIEW ----------------------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Fission with Elm"
    , body =
        [ E.layout
            [ E.width E.fill
            , E.height E.fill
            , EBA.color <| E.rgb255 176 192 222
            ]
            <| E.row
                [ E.centerX
                , E.centerY
                , E.spacing 42
                ]
                [ E.image
                    [ E.centerX
                    , E.centerY
                    , E.width <| E.px 150
                    ]
                    { src = "./assets/images/fission.svg"
                    , description = "fission"
                    }
                , E.el
                    [ E.centerX
                    , E.centerY
                    , F.bold
                    , F.size 42
                    ]
                    <| E.text "All Set Up!"
                ]
        ]
    }
