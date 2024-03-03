module Html.Session where

import Data.UUID (nil)
import Exercise
import Html.Attribute (crossOrigin_, hxGet_, hxPost_, hxSwap_, hxTarget_)
import Lucid

htmxScript :: Html ()
htmxScript =
  script_
    [ src_ "https://unpkg.com/htmx.org@1.7.0"
    , integrity_
        "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo"
    , crossOrigin_ "anonymous"
    ]
    ("" :: Text)

container :: Text -> Html () -> Html ()
container title body =
  doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title
      -- link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      -- script_ [src_ "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js", defer_ ""] ("" :: Text)
      htmxScript
      style_ "body{background:white}"
    body_ [class_ "flex"] $ do
      -- nav_ [class_ "flex-initial h-screen p-4 bg-slate-300"] $ ul_ $ do
      --   li_ $ a_ [href_ $ show $ _home rootLinks] "home"
      --   li_ $ a_ [href_ $ show $ _households rootLinks] "households"
      --   li_ $ a_ [href_ $ show $ _sessionLogout $ _session rootLinks] "logout"
      div_ [class_ "flex-1 p-4"] body

renderSet :: ExerciseSet -> Html ()
renderSet set = div_
  [ id_ $ "set-" <> show (unExerciseSetId set.id)
  , style_ $ "margin-top: 1em; border: 1px solid blue;" <> completedStyle
  ]
  $ do
    span_ $ toHtml $ unExerciseName set.exercise
    form_
      [ hxGet_ "updateSet"
      , hxTarget_ $ "#set-" <> show (unExerciseSetId set.id)
      , hxSwap_ "outerHTML"
      ] $ do
      input_
        [ name_ "setId"
        , type_ "hidden"
        , value_ $ show $ unExerciseSetId set.id
        ]
      case set.resistance of
        Kg i -> do
          span_ "Kg: "
          -- input_ [name_ "resistanceType", type_ "hidden", value_ "Kg"]
          input_
            [name_ "resistanceAmount", type_ "number", value_ $ show i, class_ inputStyle]
        Pounds i -> do
          span_ "Pounds: "
          -- input_ [name_ "resistanceType", type_ "hidden", value_ "Pounds"]
          input_
            [name_ "resistanceAmount", type_ "number", value_ $ show i, class_ inputStyle]
      span_ "Reps: "
        <> input_
          [ name_ "reps"
          , type_ "number"
          , value_ $ show set.reps
          , class_ inputStyle
          ]
      input_ [type_ "submit", value_ "submit", class_ btnStyle]
 where
  completedStyle = if set.completed then "background-color: gray;" else ""
  inputStyle =
    "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
  btnStyle = "font-bold py-2 px-4 rounded bg-blue-500 text-white"

renderSession :: Session -> Html ()
renderSession session = do
  span_ "exercises"
  br_ []
  br_ []
  foldMap renderSet session.sets
