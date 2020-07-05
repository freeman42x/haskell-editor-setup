module View where

import           Miso
import           Types

viewModel :: Model -> View Action
viewModel m = form_
  []
  [ link_
    [ rel_ "stylesheet"
    , href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
    ]
  , h5_ [class_ "title is-5"] [text "Easy Haskell Editor / IDE Setup"]
  , div_
    [class_ "control"]
    [ "Editor / IDE"
    , br_ []
    , label_
      [class_ "radio"]
      [ input_
        [ type_ "radio"
        , name_ "editor"
        , checked_ (_editorOrIde m == Atom)
        , onChecked (SetEditorOrIde Atom)
        ]
      , "Atom"
      ]
    , label_
      [class_ "radio"]
      [ input_
        [ type_ "radio"
        , name_ "editor"
        , checked_ (_editorOrIde m == VisualStudioCode)
        , onChecked (SetEditorOrIde VisualStudioCode)
        , disabled_ True
        ]
      , "Visual Studio Code"
      ]
    , label_
      [class_ "radio"]
      [ input_
        [ type_ "radio"
        , name_ "editor"
        , checked_ (_editorOrIde m == IntelliJIdeaCommunity)
        , onChecked (SetEditorOrIde IntelliJIdeaCommunity)
        , disabled_ True
        ]
      , "IntelliJ IDEA Community"
      ]
    , label_
      [class_ "radio"]
      [ input_
        [ type_ "radio"
        , name_ "editor"
        , checked_ (_editorOrIde m == SublimeText3)
        , onChecked (SetEditorOrIde SublimeText3)
        , disabled_ True
        ]
      , "Sublime Text 3"
      ]
    , label_
      [class_ "radio"]
      [ input_
        [ type_ "radio"
        , name_ "editor"
        , checked_ (_editorOrIde m == Leksah)
        , onChecked (SetEditorOrIde Leksah)
        , disabled_ True
        ]
      , "Leksah"
      ]
    ]
  , br_ []
  , textarea_ [rows_ "15", cols_ "80", disabled_ True ] [ text $ _log m ]
  , br_ []
  , label_
    [class_ "checkbox"]
    [ input_
      [ type_ "checkbox"
      , name_ "simulate"
      , checked_ True
      , onChecked SetSimulate
      ]
    , "Simulate"
    ]
  , br_ []
  , button_ [clickHandler Install, class_ "button"] [text "Install"]
  ]

clickHandler :: action -> Attribute action
clickHandler action =
  onWithOptions (defaultOptions { preventDefault = True }) "click" emptyDecoder
    $ \() -> action