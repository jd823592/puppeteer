module Main (main) where

import Control.Monad
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

import Puppet.Master.Buffer
import Puppet.Master.Colour
import Puppet.Master.Interpreter

getKeyAction :: KeyVal -> Maybe BufferChange
getKeyAction 65288 = Just deleteL
getKeyAction 65360 = Just start
getKeyAction 65361 = Just left
getKeyAction 65362 = Just up
getKeyAction 65363 = Just right
getKeyAction 65364 = Just down
getKeyAction 65367 = Just end
getKeyAction 65535 = Just deleteR
getKeyAction k = do
    c <- keyToChar k
    if isPrint c then Just (insert c) else Nothing

getConfirm :: KeyVal -> Maybe BufferChange
getConfirm 65293 = Just remember
getConfirm _     = Nothing

displayBuffer :: InterpreterWorker -> Element -> Element -> Buffer -> IO ()
displayBuffer w l t b = do
    setInnerHTML l . Just . uncurry colour . toString $ b

    ask w $ setInnerHTML t . Just <=< typeOf . snd . toString $ b

    return ()

evalBuffer :: InterpreterWorker -> Buffer -> IO ()
evalBuffer w b = void $ print <=< ask w . eval . snd . toString $ b

networkDesc :: Window -> InterpreterWorker -> Element -> Element -> MomentIO ()
networkDesc win w l t = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        win `on` keyPressEvent $ eventKeyVal >>= liftIO . fire >> return True
        return addHandler
    eKey <- fromAddHandler addHandler

    let eBuffer  = filterJust $ getKeyAction <$> eKey
        eConfirm = filterJust $ getConfirm   <$> eKey

    bBuffer <- accumB mkBuffer $ unions [ eBuffer, eConfirm ]

    let eEval = bBuffer <@ eConfirm :: Event Buffer

    eBufferChanges <- changes bBuffer

    reactimate  $          evalBuffer w      <$> eEval
    reactimate' $ fmap (displayBuffer w l t) <$> eBufferChanges

main :: IO ()
main = do
    int <- newInterpreterWorker

    initGUI

    win <- windowNew
    bar <- webViewNew
    scr <- windowGetScreen win

    root <- screenGetRootWindow scr
    mon  <- screenGetMonitorAtWindow scr root

    Rectangle x y w h <- screenGetMonitorGeometry scr mon

    settings <- webViewGetWebSettings bar

    set settings [ webSettingsUserStylesheetUri := Just "file:///home/jakub/puppeteer/Puppeteer.css" ]

    Just doc  <- webViewGetDomDocument bar
    Just body <- getBody doc

    setAttribute body "style" ("width:" ++ show w ++ "px;")

    Just p <- createElement doc $ Just "div"
    Just t <- createElement doc $ Just "div"
    Just l <- createElement doc $ Just "div"

    setAttribute p "id" "prompt"
    setAttribute t "id" "type"
    setAttribute l "id" "line"

    appendChild body (Just p)
    appendChild body (Just t)
    appendChild body (Just l)

    displayBuffer int l t mkBuffer

    set win [ containerChild  := bar
            , widgetCanFocus  := True
            --, windowTypeHint  := WindowTypeHintDock
            , windowDecorated := False
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    bh <- floor <$> getOffsetHeight l

    windowMove win x (y + h - bh)

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    net <- compile $ networkDesc win int l t
    actuate net

    mainGUI
