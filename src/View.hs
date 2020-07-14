module View where

import           Miso
import           Types



viewModel :: Model -> View Action
viewModel m = body_
  []
  [ link_
    [ rel_ "stylesheet"
    , href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
    ]
  , form_
    []
    [ h5_ [class_ "title is-5"] ["Haskell Editor Setup"]
    , br_ []
    , div_
      [class_ "control"]
      [ p_ [class_ "subtitle is-5"] ["Select Editor/IDE:"]
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
    , textarea_
      [class_ "textarea is-info", rows_ "10", cols_ "80", disabled_ True]
      [text $ _log m]
    , br_ []
    , label_
      [class_ "checkbox"]
      [ input_
        [ type_ "checkbox"
        , name_ "run"
        , checked_ (_runConfigure m)
        , onChecked SetRun
        ]
      , "Run"
      ]
    , br_ []
    , button_ [clickHandler Install, class_ "button is-primary"]
              [text "Install"]
    ]
  ]

clickHandler :: action -> Attribute action
clickHandler action =
  onWithOptions (defaultOptions { preventDefault = True }) "click" emptyDecoder
    $ \() -> action
