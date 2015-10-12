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

import Buffer
import Colour

getLeft :: KeyVal -> Maybe BufferChange
getLeft 65361 = Just left
getLeft _     = Nothing

getRight :: KeyVal -> Maybe BufferChange
getRight 65363 = Just right
getRight _     = Nothing

getInsertion :: KeyVal -> Maybe BufferChange
getInsertion k = do
    c <- keyToChar k
    if isPrint c then Just (insert c) else Nothing

getDeleteL :: KeyVal -> Maybe BufferChange
getDeleteL 65288 = Just deleteL
getDeleteL _     = Nothing

getDeleteR :: KeyVal -> Maybe BufferChange
getDeleteR 65535 = Just deleteR
getDeleteR _     = Nothing

getConfirm :: KeyVal -> Maybe BufferChange
getConfirm 65293 = Just (const mkBuffer)
getConfirm _     = Nothing

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
    let
        eLeft    = filterJust (getLeft      <$> eKey)
        eRight   = filterJust (getRight     <$> eKey)
        eInsert  = filterJust (getInsertion <$> eKey)
        eDeleteL = filterJust (getDeleteL   <$> eKey)
        eDeleteR = filterJust (getDeleteR   <$> eKey)
        eConfirm = filterJust (getConfirm   <$> eKey)
        bBuffer  = accumB mkBuffer $ unions [ eLeft, eRight, eInsert, eDeleteL, eDeleteR, eConfirm ]

    eBufferChanges <- changes bBuffer

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

    displayCode l mkBuffer
    displayType t ""

    set win [ containerChild := bar
            , widgetCanFocus := True
            , windowTypeHint := WindowTypeHintDock
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    net <- compile $ networkDesc win l t
    actuate net

    mainGUI
