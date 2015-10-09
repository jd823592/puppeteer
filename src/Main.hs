import Control.Monad.IO.Class
import Data.Maybe

import Language.Haskell.HsColour.Classify

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.DOM.Document hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Element hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

import Reactive.Banana
import Reactive.Banana.Frameworks

colour :: Int -> String -> String
colour n = concat . render n . classify where

    classify :: String -> [(TokenType, String)]
    classify = map fix . tokenise

    fix :: (TokenType, String) -> (TokenType, String)
    fix (Definition, d) = (Varid, d)
    fix t               = t

    render :: Int -> [(TokenType, String)] -> [String]
    render _ []       = []
    render n ((t, s) : ts) = ("<span class='" ++ tokencls t ++ "'>" ++ renderText n s ++ "</span>") : render (n - length s) ts

    renderText :: Int -> String -> String
    renderText n s | 0 <= n && n < length s = let (ls, r : rs) = splitAt n s in escape ls ++ "<span id='cursor'>" ++ escape [r] ++ "</span>" ++ escape rs
                   | otherwise              = s

    tokencls :: TokenType -> String
    tokencls Keyword    = "hs-keyword"
    tokencls Keyglyph   = "hs-keyglyph"
    tokencls Layout     = "hs-layout"
    tokencls Comment    = "hs-comment"
    tokencls Conid      = "hs-conid"
    tokencls Varid      = "hs-varid"
    tokencls Conop      = "hs-conop"
    tokencls Varop      = "hs-varop"
    tokencls String     = "hs-str"
    tokencls Char       = "hs-chr"
    tokencls Number     = "hs-num"
    tokencls Cpp        = "hs-cpp"
    tokencls Error      = "hs-sel"
    tokencls Definition = "hs-definition"
    tokencls _          = ""

    escape :: String -> String
    escape [] = []
    escape ('<' : rs) = "&lt;"  ++ escape rs
    escape ('>' : rs) = "&gt;"  ++ escape rs
    escape ('&' : rs) = "&amp;" ++ escape rs
    escape ( c  : rs) = c       :  escape rs

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

    set win [ containerChild      := bar
            , widgetCanFocus      := True
            , windowTypeHint      := WindowTypeHintDock
            ]

    win `on` deleteEvent   $ liftIO mainQuit >> return False
    win `on` keyPressEvent $ liftIO (putStrLn "Key") >> return True

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    mainGUI
