{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI
  ( main
  ) where

import qualified Data.Text as T
import Lens.Micro ((^.), (.~), (&), (%~))
import Lens.Micro.TH

import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , renderForm
  , handleFormEvent
  , updateFormState
  , editTextField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import HitAndBlow

data Name = GuessField
          deriving (Eq, Ord, Show)

data UI = UI
  { _game    :: Game
  , _guess   :: T.Text
  } deriving (Show)

makeLenses ''UI

mkForm :: UI -> Form UI e Name
mkForm =
  newForm [ (str "Your Guess: " <+>) @@= 
              editTextField guess GuessField (Just 1) ]

app :: App (Form UI e Name) e Name
app = App
  { appDraw         = drawUi
  , appHandleEvent  = \s ev ->
      case ev of
        VtyEvent (V.EvResize {})       -> continue s
        VtyEvent (V.EvKey V.KEsc [])   -> halt s
        VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])  -> halt s
        VtyEvent (V.EvKey V.KEnter []) -> do
          let ui  = formState s
          case chkAnswer (ui ^. game) (T.unpack $ ui ^. guess) of
            Just g -> do
              let ui' = ui & game  .~ g
                           & guess .~ ""
              continue $ updateFormState ui' s
            Nothing -> continue s
        _ -> do
          s' <- handleFormEvent ev s
          continue s'
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = focusRingCursor formFocus
  }

drawInput :: Form UI e Name -> Widget Name
drawInput f =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Input Area")
    $ vLimit 7
    $ C.center
    $ vBox [ str "Input 4-digit number and press Enter to check!"
           , inputBox 
           , messageBox ]
  where
    g = (formState f) ^. game
    inputBox = vLimit 1 $ hLimit 18 $ renderForm f
    messageBox = vLimit 1 $ hLimit 18 $ drawMessage $ g ^. done

drawMessage :: Bool -> Widget Name
drawMessage True  = str "You Got it!"
drawMessage False = str ""

drawList :: UI -> Widget Name
drawList ui =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "View Area")
    $ C.hCenter
    $ vBox (header : history) <=> space
  where
    g = ui ^. game
    header = str "Your guesses   HIT   BLOW"
    history = (map showHint) . reverse $ g ^. hints
    space = hLimit 1 $ fill ' '

drawUi :: Form UI e Name -> [Widget Name]
drawUi f = [ top <=> bottom ]
  where
    top    = drawInput f
    bottom = drawList (formState f)

main :: IO ()
main = do
  g <- initGame
  let initialUi = UI
        { _game  = g
        , _guess = ""
        }
      f = mkForm initialUi
  f' <- defaultMain app f
  putStrLn $ "answer = " <> show (formState f')

showHint :: (Numbers, Hint) -> Widget Name
showHint (n, h) =
  vLimit 1 $ left <+> middle <+> right
  where
    (hit, blow) = hint h
    left   = hLimit 12 $ fill ' ' <+> str (numbers n)
    middle = hLimit  6 $ fill ' ' <+> str (show hit)
    right  = hLimit  7 $ fill ' ' <+> str (show blow)

