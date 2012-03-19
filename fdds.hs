
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.Trans

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L (putStrLn)
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.List (find)

import Data.Maybe
import System.Environment (getArgs)
import Network.HTTP
import Network.Browser
import Network.URI (parseURI, URI(..))
import Text.HTML.TagSoup

-- TODO: configs
main = do
        vin:_ <- getArgs -- TODO: check vin format
        vinState <- vinSearchInit
        res <- vinSearch vinState vin
        mapM_ L.putStrLn res


vinSearchUrl
        = fromJust . parseURI
        $ "http://fdds.arctransistance.com/FrontEnd/SearchVin.aspx" -- Vin=xxxx
usr = "user"
pwd = "secret"


vinSearchInit = browse $ do
        -- TODO: setup logging, proxies, etc
        -- TODO: handle browser events, errors, etc
        -- TODO: network errors: 404, unreachable, gone away, reset by peer, etc
        setDebugLog $ Just "vin-search"
        -- TODO: setMaxErrorRetries
        -- TODO: connectionPool
        -- TODO: multithreading? shared state
        getBrowserState


-- FIXME: return updated state (или положить в монаду)
vinSearch bState vin
        = browse $ withBrowserState bState $ do
                let url = vinSearchUrl {uriQuery = "?Vin=" ++ vin}
                (url,rsp) <- request $ mkRequest GET url
                -- FIXME: check request result

                rsp' <- when'
                        (uriPath url == "/login/login.aspx")
                        (login url) rsp

                return $ getInfo $ parseResponse rsp'

getInfo :: [Tag L.Text] -> [L.Text]
getInfo html = [txt' | TagText txt <- gotoTable html, let txt' = strip txt, txt' /= ""]
				where
						gotoTable [] = error $ show html ++ "\n\ngotoTable failed"
						gotoTable (TagText "FDDS" : TagClose "b" : rest)
								= takeWhile (/= (TagClose "table")) rest
						gotoTable (_:xs) = gotoTable xs

						strip = L.filter (\c -> c /= '\t' && c /= '\n')
    
-- TODO: check successful login
login url rsp = do
        (url,rsp') <- request $ BL.pack <$> formToRequest (Form POST url loginData)
        return rsp'
        where
            loginData
                = [(L.unpack name, L.unpack value)
                | TagOpen tag attrs <- parseResponse rsp
                , L.toLower tag == "input"
                , let Just name  = findAttr "name" attrs
                , let value = case name of
                        "txtName" -> usr
                        "txtPassword" -> pwd
                        _ -> fromMaybe "" $ findAttr "value" attrs
                ]

         
parseResponse = parseTags . L.decodeUtf8 . rspBody
findAttr a attrs = snd <$> find ((==a).L.toLower.fst) attrs

instance Functor Request where
        fmap f r = r {rqBody = f $ rqBody r}

when' b m a = if b then m a else return a
 
