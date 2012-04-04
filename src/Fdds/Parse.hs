{-# LANGUAGE OverloadedStrings #-}

module Fdds.Parse where

import           Control.Applicative ((<$>))
import           Data.Time.Calendar
import           Data.Time.Format
import           System.Locale

import qualified Data.Text.Lazy.Read as L
import qualified Data.Text.Lazy as L
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Parsec
import           Text.Parsec


valid :: TagParser L.Text Bool
valid = do
    ts <- td
    if (null ts)
      then unexpected "empty list in valid"
      else
          case parseValid $ head [png | TagOpen "img" attrs <- ts, ("src", png) <- attrs] of
            Right b -> return b
            Left e  -> unexpected e
  where
    parseValid :: L.Text -> Either String Bool
    parseValid s
        = case s of
            "/design/images/ok.png" -> Right True
            "/design/images/cancel.png" -> Right False
            _ -> Left "valid column"


info :: TagParser L.Text (L.Text, L.Text, L.Text)
info = do
    [make, model, plate] <- map strip <$> count 3 record
    skipMany $ notOpenTag "td"
    return (make, model, plate)


record :: TagParser L.Text L.Text
record = do
    skipMany $ notCloseTag "b"
    closeTag "b"
    TagText t <- anyToken
    return t


text :: TagParser L.Text L.Text
text = do
    ts <- td
    let t = head [t | TagText t <- ts ]
    return $ strip t


int :: TagParser L.Text Int
int = do
    t <- text
    let Right (res,_) = L.decimal t
    return res


fint :: TagParser L.Text Int
fint = do
    ts <- td
    let TagText i = ts !! 2
        Right (res,_) = L.decimal i
    return res


date :: TagParser L.Text Day
date = do
    t <- text
    return $ readTime defaultTimeLocale "%d.%m.%Y" $ L.unpack t


tr = do
    skipMany $ notOpenTag "td"
    res <- many td
    skipMany $ notOpenTag "tr"
    return res


td  = do
    (_,res,_) <- wholeTag "td"
    return res

strip = L.filter (\c -> c /= '\t' && c /= '\n' && c /= '\r')
