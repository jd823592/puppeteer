import Control.Monad.IO.Class
import Data.Maybe

import Language.Haskell.HsColour.CSS
import Language.Haskell.HsColour.Classify

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.DOM.Document hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Element hiding (drop)
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView

classify :: String -> [(TokenType, String)]
classify = unwrap . tokenise . wrap where
    wrap :: String -> String
    wrap = (prefix ++)

    unwrap :: [a] -> [a]
    unwrap = drop (length (tokenise prefix))

    prefix :: String
    prefix = "x = do "

render :: [(TokenType, String)] -> String
render = concat . map renderToken

splitCursor :: Int -> [(a, String)] -> ([(a, String)], Char, [(a, String)])
splitCursor n rs = splitCursor' n [] rs where
    splitCursor' :: Int -> [(a, String)] -> [(a, String)] -> ([(a, String)], Char, [(a, String)])
    splitCursor' n ls (r@(t, w) : rs) | length w <= n = splitCursor' (n - length w) (r : ls) rs
                                      | otherwise     = let (as, b : bs) = splitAt n w in (reverse ((t, as) : ls), b, ((t, bs) : rs))

escape :: Char -> String
escape '<' = "&lt;"
escape '>' = "&gt;"
escape '&' = "&amp;"
escape  c  = [c]

colour :: Int -> String -> String
colour n s = let (ls, c, rs) = splitCursor n (classify s) in render ls ++ "<pre id='cursor'>" ++ escape c ++ "</pre>" ++ render rs

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

    p <- createElement doc $ Just "div"
    t <- createElement doc $ Just "div"
    l <- createElement doc $ Just "div"

    setAttribute (fromJust p) "id" "prompt"
    setAttribute (fromJust t) "id" "type"
    setAttribute (fromJust l) "id" "line"

    appendChild body p
    appendChild body t
    appendChild body l

    setInnerHTML (fromJust l) $ Just (colour 7 "copy >>= spellcheck >>= paste ")
    setInnerHTML (fromJust t) $ Just ":: IO ()"

    set win [ containerChild := bar
            , windowTypeHint := WindowTypeHintDock
            ]

    win `on` deleteEvent $ liftIO mainQuit >> return False

    widgetSetSizeRequest win w (-1)
    widgetShowAll win

    mainGUI
