-----------------------------------------------------------------------------
--
-- Module      :  App.Widgets.Pacer
-- Copyright   :  2009 Renaissance Computing Institute
-- License     :  BSD3
--
-- Maintainer  :  Jeff Heard <jeff@renci.org>
-- Stability   :  Experimental
-- Portability :
--
-- | Widgets for sending a heartbeat out onto the bus to be caught by other behaviours.
--
-----------------------------------------------------------------------------
module App.Widgets.Pacer where

import App.EventBus
import Control.Concurrent

-- | @paceMicrosecondsWidget timeout timername bus@
-- |
-- | Send a heartbeat event out every /timeout/ microseconds to the bus
paceMicrosecondsWidget :: Int -> String -> Widget [a]
paceMicrosecondsWidget timeout timername b = do
    let loop = do
            threadDelay timeout
            produce' "Timers" "PacerWidget" timername once [] b
            loop
    forkIO $ loop
    return ()

-- | @paceMillisecondsWidget timeout timername bus@
-- |
-- | Send a heartbeat event out every /timeout/ milliseconds to the bus
paceMillisecondsWidget :: Int -> String -> Widget [a]
paceMillisecondsWidget timeout = paceMicrosecondsWidget (timeout*1000)

-- | @paceSecondsWidget timeout timername bus@
-- |
-- | Send a heartbeat event out every /timeout/ seconds to the bus
paceSecondsWidget :: Double -> String -> Widget [a]
paceSecondsWidget timeout = paceMillisecondsWidget (round $ timeout*1000*1000)




