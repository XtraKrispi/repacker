module Component.Navbar where

import Prelude

import Component.GameSearch (Size(..))
import Component.GameSearch as GameSearch
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.Path (CommandPositionReference(..), h, m)
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Route (Route(..), navigate)
import Type.Proxy (Proxy(..))
import Types (SessionInfo)

type Slots = (search :: forall query. H.Slot query GameSearch.Output Int)
_search = Proxy :: Proxy "search"

type State =
  { currentRoute :: Route
  , session :: Maybe SessionInfo
  }

data Action = GameSearchOutput GameSearch.Output | UpdateRoute Route

data Output = UserLoggedIn SessionInfo

component :: forall query m. MonadAff m => MonadEffect m => H.Component query Route Output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = receive }
  , render
  }

receive :: Route -> Maybe Action
receive r = Just $ UpdateRoute r

initialState :: Route -> State
initialState currentRoute = { currentRoute, session: Nothing }

handleAction :: forall output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction (GameSearchOutput (GameSearch.GameSelected bg)) =
  navigate (GameR bg.bggId)
handleAction (UpdateRoute r) = modify_ _ { currentRoute = r }

render :: forall m. MonadAff m => MonadEffect m => State -> HH.ComponentHTML Action Slots m
render { currentRoute, session } =
  HH.div [ HP.class_ (H.ClassName "max-lg:collapse bg-base-200 shadow-sm w-full rounded-md") ]
    [ HH.input
        [ HP.id "navbar-1-toggle"
        , HP.class_ (H.ClassName "peer hidden")
        , HP.type_ InputCheckbox
        ]
    , HH.label [ HP.for "navbar-1-toggle", HP.class_ (H.ClassName "fixed inset-0 hidden max-lg:peer-checked:block") ] []
    , HH.div [ HP.class_ (H.ClassName "collapse-title navbar") ]
        [ HH.div [ HP.class_ (H.ClassName "navbar-start") ]
            [ HH.label [ HP.for "navbar-1-toggle", HP.class_ (H.ClassName "btn btn-ghost lg:hidden") ]
                [ Svg.svg
                    [ SP.class_ (H.ClassName "h-5 w-5")
                    , SP.fill NoColor
                    , SP.viewBox 0.0 0.0 24.0 24.0
                    , SP.stroke (Named "currentColor")
                    ]
                    [ Svg.path
                        [ SP.strokeLineCap LineCapRound
                        , SP.strokeLineJoin LineJoinRound
                        , SP.strokeWidth 2.0
                        , SP.d
                            [ m Abs 4.0 6.0
                            , h Rel 16.0
                            , m Abs 4.0 12.0
                            , h Rel 8.0
                            , m Rel (-8.0) 6.0
                            , h Rel 16.0
                            ]
                        ]
                    ]
                ]
            , HH.a [ HP.class_ (H.ClassName "btn btn-ghost text-xl"), HP.href "#/" ] [ HH.text "Repacker" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "navbar-end hidden lg:flex relative") ]
            [ HH.ul [ HP.class_ (H.ClassName "menu menu-horizontal px-1 flex gap-2 items-center") ]
                [ if currentRoute == HomeR then
                    HH.text ""
                  else
                    HH.li [ HP.class_ (H.ClassName "w-80") ]
                      [ HH.slot _search 0 GameSearch.component Regular GameSearchOutput
                      ]
                , HH.li []
                    [ case session of
                        Just s -> HH.details_
                          [ HH.summary_ [ HH.text s.name ]
                          , HH.ul [ HP.class_ (H.ClassName "p-2 bg-base-100 w-40 z-1") ]
                              [ HH.li_ [ HH.button_ [ HH.text "My Submissions" ] ]
                              , HH.li_ [ HH.button_ [ HH.text "Logout" ] ]
                              ]
                          ]
                        Nothing -> HH.a [ HP.class_ (H.ClassName "btn btn-primary") ] [ HH.text "Login" ]
                    ]
                ]
            ]
        ]
    , HH.div [ HP.class_ (H.ClassName "collapse-content lg:hidden z-1") ]
        [ HH.ul [ HP.class_ (H.ClassName "menu") ]
            [ HH.li_
                [ HH.button_ [ HH.text "Element 1" ]
                ]
            , HH.li_
                [ HH.button_ [ HH.text "Parent" ]
                , HH.ul_
                    [ HH.li_ [ HH.button_ [ HH.text "Submenu" ] ]
                    ]
                ]
            ]
        ]
    ]
