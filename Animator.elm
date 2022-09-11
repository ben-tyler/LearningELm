module Main exposing (..)

import Animator
import Animator.Inline
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (Arrows)
import Time


type alias Model =
    { foo : Animator.Timeline Int
    , pressedKeys : List Key
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            -- we tell the animator how
            -- to get the checked timeline using .checked
            .foo
            -- and we tell the animator how
            -- to update that timeline as well
            (\newfoo model ->
                { model | foo = newfoo }
            )


main =
    Browser.document
        { init =
            \() ->
                ( { foo = Animator.init 1
                  , pressedKeys = []
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                -- (4) - turning out Animator into a subscription
                -- this is where the animator will decide to have a subscription to AnimationFrame or not.
                Sub.batch
                    [ animator
                        |> Animator.toSubscription Tick model
                    , Sub.map KeyMsg Keyboard.subscriptions
                    ]
        }


type Msg
    = Tick Time.Posix
    | Foo Int
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Foo newFoo ->
            ( { model
                | foo =
                    -- (6) - Here we're adding a new state to our timeline.
                    model.foo
                        |> Animator.go Animator.slowly newFoo
              }
            , Cmd.none
            )

        KeyMsg keyMsg ->
            ( model
                |> (\m -> { m | pressedKeys = Keyboard.update keyMsg m.pressedKeys })
            , Cmd.none
            )


viewAni : Animator.Timeline Int -> Html Msg
viewAni timeline =
    --Animator.current timeline
    --    |> Debug.toString
    --    |> Html.text
    div
        [ Animator.Inline.backgroundColor timeline <|
            \state ->
                if state == 1 then
                    Color.blue

                else
                    Color.darkOrange
        ]
        [ Animator.current timeline
            |> Debug.toString
            |> Html.text
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Foo"
    , body =
        [ stylesheet
        , div
            [ Attr.class "root"
            ]
            [ div
                [ Attr.class "viewport"
                ]
                [ viewAni model.foo
                ]
            ]
        , div [] [ button [ Html.Events.onClick (Foo 2) ] [ Html.text ">>>" ] ]
        ]
    }


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');
.root {
    width: 80%;

    font-size: 48px;
    user-select: none;
    padding: 50px;
    font-family: 'Roboto', sans-serif;
}
.viewport {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 200px;
}
.checkbox {
    border-width: 10px;
    border-style: solid;
    color: #000;
    width: 160px;
    height: 160px;
    border-radius: 20px;
    font-size: 160px;
    line-height: 1.0;
    text-align: center;
}
"""
        ]
