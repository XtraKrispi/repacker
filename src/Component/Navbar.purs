module Component.Navbar where

import Prelude

import Component.GameSearch (Size(..))
import Component.GameSearch as GameSearch
import Component.Helpers (addToast)
import DOM.HTML.Indexed.FormMethod as Method
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Dialog (close, openModal)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.Path (CommandArcChoice(..), CommandPositionReference(..), CommandSweepChoice(..), a, h, l, m, v)
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Route (Route(..), navigate)
import Store (selectSession)
import Store as S
import Store as Store
import Supabase (Client)
import Supabase as Supabase
import Supabase.Auth (UserEmail(..))
import Type.Proxy (Proxy(..))
import Types (SessionInfo)

type Slots = (search :: forall query. H.Slot query GameSearch.Output Int)
_search = Proxy :: Proxy "search"

type CoreData =
  ( currentRoute :: Route
  , client :: Client
  )

--TODO: Close the signup modal and provide feedback to user
type State =
  { loginEmail :: String
  , session :: Maybe SessionInfo
  | CoreData
  }

type Input =
  { | CoreData
  }

data Action
  = GameSearchOutput GameSearch.Output
  | UpdateState (Maybe SessionInfo) Input
  | LoginClicked
  | SignInUser
  | LoginEmailUpdated String
  | LogOut
  | NavigateTo Route

component
  :: forall query output m
   . MonadAff m
  => MonadEffect m
  => MonadStore S.Action S.Store m
  => H.Component query Input output m
component = connect selectSession $ H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = receive
      }
  , render
  }

receive :: Connected (Maybe SessionInfo) Input -> Maybe Action
receive { context, input } = Just $ UpdateState context input

initialState :: Connected (Maybe SessionInfo) Input -> State
initialState { context, input: { currentRoute, client } } = { currentRoute, session: context, loginEmail: "", client }

handleAction :: forall output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action Slots output m Unit
handleAction (GameSearchOutput (GameSearch.GameSelected bg)) =
  navigate (GameR bg.bggId)
handleAction (UpdateState session input) = modify_ _ { currentRoute = input.currentRoute, session = session, client = input.client }
handleAction LoginClicked = liftEffect $ openModal "#signin-modal"
handleAction SignInUser = do
  { loginEmail, client } <- get
  results <- liftAff $ Supabase.sendOtpToEmail { email: UserEmail loginEmail } client
  case results.error of
    Just _err -> addToast { message: "There was a problem signing in. Please try again.", severity: Store.Error }
    Nothing -> do
      addToast { message: "Please check your email for your signin link.", severity: Store.Info }
      liftEffect $ close "#signin-modal"
handleAction (LoginEmailUpdated str) = modify_ _ { loginEmail = str }
handleAction LogOut = do
  { client } <- get
  _ <- liftAff $ Supabase.signOut client
  updateStore S.LogoutUser
handleAction (NavigateTo route) = do
  navigate route

render :: forall m. MonadAff m => MonadEffect m => State -> HH.ComponentHTML Action Slots m
render state@{ currentRoute, session } =
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
            [ HH.div [ HP.class_ (H.ClassName "px-1 flex gap-2 items-center") ]
                [ if currentRoute == HomeR then
                    HH.text ""
                  else
                    HH.div [ HP.class_ (H.ClassName "w-80") ]
                      [ HH.slot _search 0 GameSearch.component Regular GameSearchOutput
                      ]
                , HH.div []
                    [ case session of
                        Just s ->
                          HH.div [ HP.class_ (H.ClassName "dropdown dropdown-end") ]
                            [ HH.div [ HP.tabIndex 0, HP.attr (H.AttrName "role") "button", HP.class_ (H.ClassName "btn btn-ghost rounded-field") ] [ HH.text $ fromMaybe (unwrap s.email) s.name ]
                            , HH.ul [ HP.tabIndex (-1), HP.class_ (H.ClassName "menu dropdown-content bg-base-200 rounded-box z-1 mt-4 w-52 p-2 shadow-sm") ]
                                [ HH.li_ [ HH.button_ [ HH.text "My Submissions" ] ]
                                , HH.li [ if currentRoute == ProfileR s.userId then HP.class_ (H.ClassName "menu-active") else HP.style "" ]
                                    [ HH.button [ HE.onClick (\_ -> NavigateTo (ProfileR s.userId)) ]
                                        [ HH.text "My Profile" ]
                                    ]
                                , HH.li_
                                    [ HH.button [ HE.onClick (\_ -> LogOut) ]
                                        [ HH.text "Logout" ]
                                    ]
                                ]
                            ]
                        Nothing -> HH.button
                          [ HP.class_ (H.ClassName "btn btn-primary")
                          , HE.onClick (\_ -> LoginClicked)
                          ]
                          [ HH.text "Login" ]
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
    , signinModal state
    ]

signinModal :: forall m. MonadAff m => MonadEffect m => State -> HH.ComponentHTML Action Slots m
signinModal state = HH.dialog
  [ HP.id "signin-modal"
  , HP.class_ (H.ClassName "modal")
  ]
  [ HH.div [ HP.class_ (H.ClassName "modal-box") ]
      [ HH.form [ HP.method Method.Dialog ] [ HH.button [ HP.class_ (H.ClassName "btn btn-sm btn-circle btn-ghost absolute right-2 top-2") ] [ HH.text "✕" ] ]
      , HH.h3 [ HP.class_ (H.ClassName "text-lg font-bold") ] [ HH.text "Sign In to Your Account" ]
      , HH.div [ HP.class_ (H.ClassName "py-4 flex w-full flex-col") ]
          [ HH.div [ HP.class_ (H.ClassName "card bg-base-300 rounded-box grid shadow-sm") ]
              [ HH.div [ HP.class_ (H.ClassName "card-body") ]
                  [ HH.div [ HP.class_ (H.ClassName "card-title flex justify-start") ]
                      [ Svg.svg
                          [ SP.fill NoColor
                          , SP.viewBox 0.0 0.0 24.0 24.0
                          , SP.strokeWidth 1.5
                          , SP.stroke (Named "currentColor")
                          , SP.class_ (H.ClassName "size-10")
                          ]
                          [ Svg.path
                              [ SP.strokeLineCap LineCapRound
                              , SP.strokeLineJoin LineJoinRound
                              , SP.d
                                  [ m Abs 21.75 6.75
                                  , v Rel 10.5
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep1 (-2.25) 2.25
                                  , h Rel (-15.0)
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep1 (-2.25) (-2.25)
                                  , v Abs 6.75
                                  , m Rel 19.5 0.0
                                  , a Abs 2.25 2.25 0.0 Arc0 Sweep0 19.5 4.5
                                  , h Rel (-15.0)
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep0 (-2.25) 2.25
                                  , m Rel 19.5 0.0
                                  , v Rel 0.243
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep1 (-1.07) 1.916
                                  , l Rel (-7.5) 4.615
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep1 (-2.36) 0.0
                                  , l Abs 3.32 8.91
                                  , a Rel 2.25 2.25 0.0 Arc0 Sweep1 (-1.07) (-1.916)
                                  , v Abs 6.75
                                  ]
                              ]
                          ]
                      , HH.div [ HP.class_ (H.ClassName "flex flex-col items-start") ]
                          [ HH.p_ [ HH.text "Magic Email Link" ]
                          , HH.p [ HP.class_ (H.ClassName "text-sm") ] [ HH.text "Sign in instantly via your inbox." ]
                          , HH.p [ HP.class_ (H.ClassName "text-sm") ] [ HH.text "You'll be signed up automatically." ]
                          ]

                      ]
                  , HH.div [ HP.class_ (H.ClassName "flex") ]
                      [ HH.fieldset [ HP.class_ (H.ClassName "fieldset w-full") ]
                          [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ]
                              [ HH.text "Email Address" ]
                          , HH.input
                              [ HP.class_ (H.ClassName "input w-full")
                              , HP.type_ InputEmail
                              , HP.placeholder "you@example.com"
                              , HE.onValueInput LoginEmailUpdated
                              , HP.value state.loginEmail
                              ]
                          ]
                      ]
                  , HH.button [ HP.class_ (H.ClassName "w-full btn btn-primary"), HE.onClick (\_ -> SignInUser) ]
                      [ HH.text "Send Magic Link" ]
                  ]
              ]
          ]
      ]
  , HH.form
      [ HP.method Method.Dialog
      , HP.class_ (H.ClassName "modal-backdrop")
      ]
      [ HH.button_ [ HH.text "close" ] ]
  ]
