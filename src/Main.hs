import Control.Monad.IO.Class
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.DOM.Document hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Element hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import Reactive.Banana
import Reactive.Banana.Frameworks

import Buffer
import Colour

networkDesc :: Frameworks t => Window -> Element -> Moment t ()
networkDesc win bar = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        register addHandler print
        win `on` keyPressEvent $ eventKeyVal >>= liftIO . fire >> return True
        return addHandler
    eKey <- fromAddHandler addHandler
    reactimate $ fmap (const (return ())) eKey

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

    setInnerHTML l $ Just (colour 7 "copy >>= spellcheck >>= paste ")
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
