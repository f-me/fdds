{-# LANGUAGE OverloadedStrings #-}


module Fdds where

import Control.Applicative ((<$>))

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (find)
import Data.Maybe
import Data.Time.Calendar
import Network.HTTP
import Network.Browser
import Network.URI (parseURI, URI(..))
import Text.HTML.TagSoup

import Control.Monad.State
import Text.Parsec
import Text.HTML.TagSoup.Parsec

import Fdds.Parse


data Conf = Conf
        { cUri      :: URI
        , cUser     :: L.Text
        , cPassword :: L.Text
        }


data Result = Result
        { rValid        :: Bool
        , rMake         :: L.Text
        , rModel        :: L.Text
        , rPlate        :: L.Text
        , rType         :: L.Text
        , rMileage      :: Int
        , rValidFrom    :: Day
        , rValidTill    :: Day
        , rCountrySell  :: L.Text
        , rCountryLegal :: L.Text
        , rFdds         :: Int
        } deriving Show


test = do    -- TODO: check vin format
    vinState <- vinSearchInit
    res <- vinSearch conf vinState vin
    print res
  where
    vin  = "X9FDXXEEBD9P29355"
    conf = Conf
             { cUri      = fromJust . parseURI $ "http://fdds.arctransistance.com/FrontEnd/SearchVin.aspx" -- Vin=xxxx
             , cUser     = "user"
             , cPassword = "secret"
             }


vinSearchInit = browse $ do
        -- TODO: setup logging, proxies, etc
        -- TODO: handle browser events, errors, etc
        -- TODO: network errors: 404, unreachable, gone away, reset by peer, etc
        setDebugLog $ Just "vin-search"
        -- TODO: setMaxErrorRetries
        -- TODO: multithreading? shared state
        setMaxPoolSize $ Just 10
        get


-- FIXME: return updated state (или положить в монаду)
vinSearch
  :: Conf
     -> BrowserState (HandleStream BL.ByteString)
     -> [Char]
     -> IO [Result]
vinSearch conf bState vin
        = browse $ withBrowserState bState $ do
                let url = (cUri conf) {uriQuery = "?Vin=" ++ vin}
                (url,rsp) <- request $ mkRequest GET url
                -- FIXME: check request result

                rsp' <- when'
                        (uriPath url == "/login/login.aspx")
                        (login conf url) rsp

                either (error . show) return (getResults $ parseResponse rsp')


getResults :: [Tag L.Text] -> Either ParseError [Result]
getResults = parse go "html"
  where
    go = do
      skipMany $ notOpenTag "table"
      openTag "table"
      try results <|> go


results :: TagParser L.Text [Result]
results = do
    skipMany $ notOpenTag "tr"
    header <- tr

    when (length header < 9) $ unexpected "table header"

    if (header !! 8 == [TagOpen "b" [],TagText "FDDS",TagClose "b"])
      then many result
      else unexpected "table header"


result :: TagParser L.Text Result
result = do
    openTag "tr"
    anyToken
    b      <- valid
    (make,model,plate) <- info
    t      <- text
    milage <- int
    from   <- date
    till   <- date
    sell   <- text
    legal  <- text
    fdds   <- fint
    anyToken
    closeTag "tr"
    return $ Result { rValid        = b
                    , rMake         = make
                    , rModel        = model
                    , rPlate        = plate
                    , rType         = t
                    , rMileage      = milage
                    , rValidFrom    = from
                    , rValidTill    = till
                    , rCountrySell  = sell
                    , rCountryLegal = legal
                    , rFdds         = fdds
                    }


-- TODO: check successful login
login conf url rsp = do
        (url,rsp') <- request $ BL.pack <$> formToRequest (Form POST url loginData)
        return rsp'
        where
            loginData
                = [(L.unpack name, L.unpack value)
                | TagOpen tag attrs <- parseResponse rsp
                , L.toLower tag == "input"
                , let Just name  = findAttr "name" attrs
                , let value = case name of
                        "txtName" -> cUser conf
                        "txtPassword" -> cPassword conf
                        _ -> fromMaybe "" $ findAttr "value" attrs
                ]


parseResponse = parseTags . L.decodeUtf8 . rspBody
findAttr a attrs = snd <$> find ((==a).L.toLower.fst) attrs


instance Functor Request where
        fmap f r = r {rqBody = f $ rqBody r}


when' b m a = if b then m a else return a
