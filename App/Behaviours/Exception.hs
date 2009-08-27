-- |  
-- Module      :  App.Behaviours.Exception
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
-- Handle exceptions slightly more gracefully than the Haskell runtime does.
module App.Behaviours.Exception where

import Data.List (filter)
import Control.Applicative
import qualified Control.Exception as Ex
import App.EventBus
import Text.PrettyPrint
import qualified Data.Set as Set
import System.IO
import Data.Time
import System.Locale

renderException (Event nm _ _ edata source tm) =
            brackets (text (formatTime defaultTimeLocale "%T" tm)) <+> text "Exception thrown from" <+> text source <> colon <+> text nm $+$
            (nest 4 . vcat . map text . map (safeShow (Just 80)) $ edata)

-- | Bork the program when an unhandled exception makes it to this behaviour.
--   Catches all events with the group \"Exception\" and throws them as one big exception.
unhandledExceptionBehaviour :: Behaviour [EData a]
unhandledExceptionBehaviour b = consumeEventGroupCollectivelyWith b "Exception" $
    (return []) <$ (Ex.throwIO . Ex.ErrorCall . render . vcat . map renderException . Set.toList)

-- | Handle exceptions by completely ignoring them. Not recommended, really, but hey, who am I to judge?
disregardExceptionsFromSource :: String -> Behaviour [EData a]
disregardExceptionsFromSource s b = pollEventGroupWith b "Exception" $
    (\e -> return $ if src e == s then [Deletion e] else [])

-- | Handle exceptions by completely ignoring them. Not recommended, really, but hey, who am I to judge?
disregardExceptionsNamed :: String -> Behaviour [EData a]
disregardExceptionsNamed n b = pollEventGroupWith b "Exception" $
    (\e -> return $ if ename e == n then [Deletion e] else [])

-- | Handle exceptions by printing them to stdout and then completely ignoring them.
printAndDisregardExceptionsFromSource :: String -> Behaviour [EData a]
printAndDisregardExceptionsFromSource s b = pollEventGroupWith b "Exception" $ \e ->
    if src e == s then (return . return $ [Deletion e]) =<<  (putStrLn . render . renderException $ e) else return []

-- | Handle exceptions by printing them to stdout and then completely ignoring them
printAndDisregardExceptionsNamed :: String -> Behaviour [EData a]
printAndDisregardExceptionsNamed n b = pollEventGroupWith b "Exception" $ \e ->
    if ename e == n then (return . return $ [Deletion e]) =<< (putStrLn . render . renderException $ e) else return []

-- | Handle exceptions by printing them to a handle and then completely ignoring them.
logAndDisregardExceptionsFromSource :: Handle -> String -> Behaviour [EData a]
logAndDisregardExceptionsFromSource h s b = pollEventGroupWith b "Exception" $ \e ->
    if src e == s then (return . return $ [Deletion e]) =<<  (hPutStrLn h . render . renderException $ e) else return []

-- | Handle exceptions by printing them to handle and then completely ignoring them
logAndDisregardExceptionsNamed :: Handle -> String -> Behaviour [EData a]
logAndDisregardExceptionsNamed h n b = pollEventGroupWith b "Exception" $ \e ->
    if ename e == n then (return . return $ [Deletion e]) =<< (hPutStrLn h . render . renderException $ e) else return []

