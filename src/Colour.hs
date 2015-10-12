module Colour (colour) where

import Language.Haskell.HsColour.Classify

colour :: Int -> String -> String
colour n = concat . render n . classify

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
