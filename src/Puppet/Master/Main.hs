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
getKeyAction 65360 = Just start
getKeyAction 65361 = Just left
getKeyAction 65363 = Just right
getKeyAction 65367 = Just end
getKeyAction 65535 = Just deleteR
getKeyAction k = do
    c <- keyToChar k
    if isPrint c then Just (insert c) else Nothing

getConfirm :: KeyVal -> Maybe BufferChange
getConfirm 65293 = Just (const mkBuffer)
getConfirm _     = Nothing

displayBuffer :: Element -> Element -> Buffer -> IO ()
displayBuffer l t b = do
    setInnerHTML l . Just . uncurry colour . toString $ b

    runInterpreter $ do
        setImports [ "Prelude" ]
        setInnerHTML t . Just <=< typeOf . snd . toString $ b
    return ()

evalBuffer :: Buffer -> IO ()
evalBuffer b = putStrLn $ "eval: " ++ show (toString b)

networkDesc :: Frameworks t => Window -> Element -> Element -> Moment t ()
networkDesc win l t = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        win `on` keyPressEvent $ eventKeyVal >>= liftIO . fire >> return True
        return addHandler
    eKey <- fromAddHandler addHandler

    let
        eBuffer  = filterJust $ getKeyAction <$> eKey
        eConfirm = filterJust $ getConfirm   <$> eKey
        bBuffer  = accumB mkBuffer $ unions [ eBuffer, eConfirm ]
        eEval    = bBuffer <@ eConfirm

    eBufferChanges <- changes bBuffer

    reactimate  $          evalBuffer      <$> eEval
    reactimate' $ fmap (displayBuffer l t) <$> eBufferChanges

main :: IO ()
main = do
    initGUI

    win <- windowNew
    bar <- webViewNew
    scr <- windowGetScreen win

    root <- screenGetRootWindow scr
    mon  <- screenGetMonitorAtWindow scr root

    (Rectangle x y w h)  <- screenGetMonitorGeometry scr mon

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

    displayBuffer l t mkBuffer

    set win [ containerChild  := bar
            , widgetCanFocus  := True
            , windowTypeHint  := WindowTypeHintDock
            , windowDecorated := False
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    bh <- fmap floor $ getOffsetHeight l

    windowMove win x (y + h - bh)

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    net <- compile $ networkDesc win l t
    actuate net

    mainGUI
