module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Maybe

import Language.Haskell.Interpreter hiding (set, (:=))

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

import Puppet.Master.Buffer
import Puppet.Master.Colour

getKeyAction :: KeyVal -> Maybe BufferChange
getKeyAction 65288 = Just deleteL
getKeyAction 65293 = Just (const mkBuffer)
getKeyAction 65360 = Just start
getKeyAction 65361 = Just left
getKeyAction 65363 = Just right
getKeyAction 65367 = Just end
getKeyAction 65535 = Just deleteR
getKeyAction k = do
    c <- keyToChar k
    if isPrint c then Just (insert c) else Nothing

displayCode :: Element -> Buffer -> IO ()
displayCode e = setInnerHTML e . Just . uncurry colour . toString

displayType :: Element -> String -> IO ()
displayType e b = do
    runInterpreter $ do
        setImports [ "Prelude" ]
        setInnerHTML e . Just <=< typeOf $ b
    return ()

networkDesc :: Frameworks t => Window -> Element -> Element -> Moment t ()
networkDesc win l t = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        win `on` keyPressEvent $ eventKeyVal >>= liftIO . fire >> return True
        return addHandler
    eKey <- fromAddHandler addHandler
    eBufferChanges <- changes $ accumB mkBuffer (filterJust $ getKeyAction <$> eKey)

    let
        eBufferContentChanges = fmap (snd . toString) <$> eBufferChanges

    --liftIO $ register addHandler print
    --reactimate' $ fmap (liftIO . print . uncurry colour . toString) <$> eBufferChanges

    reactimate' $ fmap (displayCode l) <$> eBufferChanges
    reactimate' $ fmap (displayType t) <$> eBufferContentChanges

main :: IO ()
main = do
    initGUI

    win <- windowNew
    bar <- webViewNew
    scr <- windowGetScreen win

    --root <- screenGetRootWindow scr
    --mon  <- screenGetMonitorAtWindow scr root

    --(Rectangle x y w h)  <- screenGetMonitorGeometry scr mon
    (Rectangle x y w h)  <- screenGetMonitorGeometry scr 0

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

    displayCode l mkBuffer
    displayType t ""

    set win [ containerChild := bar
            , widgetCanFocus := True
            , windowTypeHint := WindowTypeHintDock
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    bh <- fmap floor $ getOffsetHeight l

    windowMove win x (y + h - bh)

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    net <- compile $ networkDesc win l t
    actuate net

    mainGUI
