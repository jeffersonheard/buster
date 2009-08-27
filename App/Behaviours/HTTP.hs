{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  App.Behaviours.HTTP
-- Copyright   :  2009 Renaissance Computing Institute
-- License     :  BSD3
--
-- Maintainer  :  Jeff Heard <jeff@renci.org>
-- Stability   :  Experimental
-- Portability :
--
-- | Behaviours for HTTP requests.  Looks for Events named
--   HTTP\//MethodName/ with event data of [EString uri, EByteString senddata, EStringL headers] and consumes them.  Produces
--   Events named HTTPResponse with source httpBehaviour\//MethodName/ and the contents of the response as the event data in a ByteString.
--   They also produce Exceptions with the same source and name ConnectionError if there is no network connection or HTTP Service
--   or HTTPErrorResponseCode if the Server sent back an error code or ParseFailure if the URI didn't parse.
--
-----------------------------------------------------------------------------

module App.Behaviours.HTTP (


) where

import Control.Applicative ((<$>))
import App.EventBus
import Network.HTTP.HandleStream
import Network.HTTP
import Network.URI
import Network.Stream
import qualified Data.ByteString as BS

maybeHead (x:xs) = Just x
maybeHead [] = Nothing

httpBehaviour :: RequestMethod -> Behaviour [EData a]
httpBehaviour method b = consumeNamedEventsWith b ("HTTP/" ++ show method) $ \evt ->
    let EString uriS = head . eventdata $ evt
        postdata = maybe BS.empty (\(EByteString getdata) -> getdata) . maybeHead . tail . eventdata $ evt
        postheaders = headers . tail . tail . eventdata $ evt

        headers (EString nm:EString val:hs) = Header (HdrCustom nm) val : headers hs
        headers [] = []

        httpGet uri = (Network.HTTP.HandleStream.simpleHTTP $ Request uri method postheaders postdata) >>=
            either (\_ -> produce "Exception" ("httpBehaviour" ++ show method) "ConnectionError" once [])
                   (\(Response code reason rspheaders contents) ->
                                    case code of
                                        (1,_,_) -> produce "HTTPResponse" ("httpBehaviour/" ++ show method) (show uri) Persistent [EByteString contents]
                                        (2,_,_) -> produce "HTTPResponse" ("httpBehaviour/" ++ show method) (show uri) Persistent [EByteString contents]
                                        _       -> produce "Exception" ("httpBehaviour/" ++ show method) "HTTPErrorResponseCode" once [EString (show code), EStringL . map show $ rspheaders, EByteString contents])
       in case parseURI uriS of
            Just uri -> listM $ httpGet uri
            Nothing -> listM $ produce "Exception" ("httpBehaviour" ++ show method) "ParseFailure" once [EString uriS]






