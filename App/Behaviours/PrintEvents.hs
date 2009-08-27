-- | 
-- Module      :  App.Behaviours.PrintEvents
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
-- Printouts of events for debugging purposes
module App.Behaviours.PrintEvents where

import qualified Data.ByteString as B
import Text.PrettyPrint
import Data.Time
import System.Locale
import App.EventBus
import System.IO

printEventsBehaviour :: Behaviour [EData a]
printEventsBehaviour b = pollAllEventsWith b $ (\(Event n g lifetime edata source t) -> do
    putStrLn.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

showIfPossible (EString x) = text x
showIfPossible (EByteString x) = text "ByteString"
showIfPossible (EByteStringL x) = text "[ByteString]"
showIfPossible (EInt x) = text (show x)
showIfPossible (EDouble x) = text (show x)
showIfPossible (EBool x) = text (show x)
showIfPossible (EStringL x) = text (show x)
showIfPossible (EIntL x) = text (show x)
showIfPossible (EDoubleL x) = text (show x)
showIfPossible (EBoolL x) = text (show x)
showIfPossible (EChar x) = char x
showIfPossible (EOther x) = text "Custom Data"
showIfPossible (EOtherL x) = text "Custom Data List"
showIfPossible (EAssoc (x,y)) = text "x -> " <> showIfPossible y
showIfPossible (EAssocL assocs) = vcat (map showAssoc assocs)
showAssoc (x, y) = text x <> showIfPossible y
	
printEventGroupBehaviour :: String -> Behaviour [EData a]
printEventGroupBehaviour grp b = pollEventGroupWith b grp $ (\(Event n g lifetime edata source t) -> do
    putStrLn.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

	
printEventNameBehaviour :: String -> Behaviour [EData a]
printEventNameBehaviour grp b = pollNamedEventsWith b grp $ (\(Event n g lifetime edata source t) -> do
    putStrLn.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

	
printEventSourceBehaviour :: String -> Behaviour [EData a]
printEventSourceBehaviour grp b = pollEventsFromSourceWith b grp $ (\(Event n g lifetime edata source t) -> do
    putStrLn.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

printQNameBehaviour :: Behaviour a
printQNameBehaviour bus = pollAllEventsWith bus $ \(Event n g _ _ s _) -> do
    print (g,s,n)
    return []

checkpoint :: String -> Behaviour a
checkpoint message bus = do
    putStrLn message
    passthrough bus

logEventsBehaviour :: Handle -> Behaviour [EData a]
logEventsBehaviour handle  b = pollAllEventsWith b $ (\(Event n g lifetime edata source t) -> do
    hPutStrLn handle.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

logEventGroupBehaviour :: Handle -> String -> Behaviour [EData a]
logEventGroupBehaviour handle grp b = pollEventGroupWith b grp $ (\(Event n g lifetime edata source t) -> do
    hPutStrLn handle.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

logEventNameBehaviour :: Handle -> String -> Behaviour [EData a]
logEventNameBehaviour handle grp b = pollNamedEventsWith b grp $ (\(Event n g lifetime edata source t) -> do
    hPutStrLn handle . render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])

logEventSourceBehaviour :: Handle -> String -> Behaviour [EData a]
logEventSourceBehaviour handle grp b = pollEventsFromSourceWith b grp $ (\(Event n g lifetime edata source t) -> do
    hPutStrLn handle.render $ text "name:    " <+> text n $+$
	                  text "source:  " <+> text source $+$
	                  text "group:   " <+> text g $+$
	                  text "ttl:     " <+> text (show lifetime) $+$
	                  text "emitTime:" <+> text (formatTime defaultTimeLocale "%T" t) $+$
	                  (vcat.map showIfPossible $ edata)
    return [])


logQNameBehaviour handle bus  = pollAllEventsWith bus $ \(Event n g _ _ s _) -> do
    hPutStrLn handle . show $ (g,s,n)
    return []

logCheckpoint handle message bus = hPutStrLn handle message >> passthrough bus
