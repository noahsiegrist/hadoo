module Hadoo.Helpers (createPage, htmlString, e, ea, concatArr, multiLineTextParam) where

import Web.Scotty 
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)


htmlString :: String -> ActionM ()
htmlString = html . LT.pack


-- | Type Alias für Html Strings
type Html = String

-- | Erzeugt ein Element ohne Attribute
e :: String -> String -> String
e tag kids = ea tag [] kids

-- | Erzeugt ein Element mit Attributen
ea :: String -> [(String, String)] -> String -> String
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"

-- | Diese Funktion entfernt `\r` Control Characters aus den übertragenen Daten.
-- Sie müssen diese Funktion verwenden um Multiline Textinput ("content") aus einer 
-- Textarea auszulesen.
multiLineTextParam :: String -> ActionM String
multiLineTextParam paramName = fmap (filter (/='\r')) (param (LT.pack paramName)) 

concatArr :: [String] -> String
concatArr = foldr (++) "" 

createPage :: String -> IO String
createPage content = do
    return $
        "<!DOCTYPE html>" ++
        ea "html" [("lang", "en")] (
            e "head" $
                ea "link" [("rel", "stylesheet"), ("href", "/styles.css")] ""  
                ++ 
                e "body" (
                    ea "h1" [("class", "body")] "Hadoo" ++ 
                    content
                )
        )

