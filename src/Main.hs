import Control.Monad.IO.Class
import Data.Char
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.WebKit.DOM.Document
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Buffer
import Colour

getLeft :: KeyVal -> Maybe ()
getLeft 65361 = Just ()
getLeft _     = Nothing

getRight :: KeyVal -> Maybe ()
getRight 65363 = Just ()
getRight _     = Nothing

getInsertion :: KeyVal -> Maybe Char
getInsertion k = do
    c <- keyToChar k
    if isPrint c then Just c else Nothing

getDeleteL :: KeyVal -> Maybe ()
getDeleteL 65288 = Just ()
getDeleteL _     = Nothing

getDeleteR :: KeyVal -> Maybe ()
getDeleteR 65535 = Just ()
getDeleteR _     = Nothing

display :: MonadIO m => Element -> Buffer -> m ()
--display e = setInnerHTML e . Just . uncurry colour . toString
display e b = do
    liftIO . print . uncurry colour . toString $ b
    setInnerHTML e . Just . uncurry colour . toString $ b

networkDesc :: Frameworks t => Window -> Element -> Moment t ()
networkDesc win bar = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        register addHandler print
        win `on` keyPressEvent $ eventKeyVal >>= liftIO . fire >> return True
        return addHandler
    eKey <- fromAddHandler addHandler
    let
        eLeft    = const left      <$> filterJust (getLeft      <$> eKey)
        eRight   = const right     <$> filterJust (getRight     <$> eKey)
        eInsert  = insert          <$> filterJust (getInsertion <$> eKey)
        eDeleteL = const deleteL   <$> filterJust (getDeleteL   <$> eKey)
        eDeleteR = const deleteR   <$> filterJust (getDeleteR   <$> eKey)
        bBuffer  = accumB mkBuffer  $  unions [ eLeft, eRight, eInsert, eDeleteL, eDeleteR ]
    eBufferChanges <- changes bBuffer
    reactimate' $ fmap (display bar) <$> eBufferChanges

main :: IO ()
main = do
    initGUI

    win <- windowNew
    bar <- webViewNew
    scr <- windowGetScreen win

    w <- screenGetWidth scr

    settings <- webViewGetWebSettings bar

    set settings [ webSettingsUserStylesheetUri := Just "file:///home/jakub/puppeteer/Puppeteer.css" ]

    (Just doc)  <- webViewGetDomDocument bar
    (Just body) <- getBody doc

    setAttribute body "style" ("width:" ++ show w ++ "px;")

    (Just p) <- createElement doc $ Just "div"
    (Just t) <- createElement doc $ Just "div"
    (Just l) <- createElement doc $ Just "div"

    setAttribute p "id" "prompt"
    setAttribute t "id" "type"
    setAttribute l "id" "line"

    appendChild body (Just p)
    appendChild body (Just t)
    appendChild body (Just l)

    setInnerHTML t $ Just ":: IO ()"

    set win [ containerChild := bar
            , widgetCanFocus := True
            --, windowTypeHint := WindowTypeHintDock
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    net <- compile $ networkDesc win l
    actuate net

    mainGUI
