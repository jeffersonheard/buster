-- | Gtk mouse keyboard widget.
--
--   For a mouse button press or release, add events named SingleClick or ClickRelease respectively to the bus.
--   For this widget, all events have source \"KeyboardMouseWidget\", and group \"Mouse\"
--   Additionally, the data attached to the event follows the form [EString SingleClick|ClickRelease, EDouble x, EDouble y, EStringL [Gtk modifier names]]
--
--   For a keyboard press or release, add events named KeyDown or KeyUp respectively to the bus.
--   All keyboard events have group ''Keyboard'' and source ''WidgetName.KeyboardMouseWidget''
--   Additionally, the data attached to a keyboard event follows the form [EString keyName | EChar keyChar, EStringL [Gtk modifier names]]
--
--   For a tablet proximity, add events named \"Proximity\" with source WidgetName.KeyboardMouseWidget, group \"Mouse\" and with attached data
--   [EBool True] for the tablet is in proximity and [EBool False] for the tablet is out of proximity.
--
--   For mouse motion, add events named \"Position\" with group \"Mouse\" and attached data [EDouble x, EDouble y, EStringL modifiers]
--
module App.Widgets.GtkMouseKeyboard where

import Control.Applicative
import Control.Concurrent
import Data.Maybe
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import App.EventBus

-- Gtk's button click event system is annoying, so we're ignoring it and only bothering with the single clicks.
-- when we receive a click, fire off a thread (once) that waits for 100ms to see how many clicks we get total in that time.  Then fire off that number.
buttonHandler _ _ (Gtk.Button _ Gtk.DoubleClick _ _ _ _ _ _ _) = return True
buttonHandler _ _ (Gtk.Button _ Gtk.TripleClick _ _ _ _ _ _ _) = return True
buttonHandler wname b (Gtk.Button sent click time x y modifiers button _ _) = do
    produce' "Mouse" (wname ++ ".KeyboardMouseWidget") (show click) once [EString . show $ button, EDouble x, EDouble y, EStringL . map show $ modifiers] b
    return True

scrollWheelHandler wname b (Gtk.Scroll _ _ x y direction _ _) = do
    produce' "Mouse" (wname ++ ".KeyboardMouseWidget") (show direction) once [EDouble x, EDouble y] b
    return True

keyboardHandler wname b (Gtk.Key released sent time modifiers withCapsLock withNumLock withScrollLock keyVal keyName keyChar) = do
    produce' "Keyboard" (wname ++ "KeyboardMouseWidget") (if released then "KeyUp" else "KeyDown") once
            [ fromMaybe (EString . show $ keyName) (EChar <$> keyChar)
            , EStringL . map show $ modifiers ] b
    return False

motionHandler wname w b evt = do
    produce' "Mouse" (wname ++ ".KeyboardMouseWidget") "Position" once [EDouble . Gtk.eventX $ evt, EDouble . Gtk.eventY $ evt, EStringL . map show . Gtk.eventModifier $ evt] b
    dwin <- Gtk.widgetGetDrawWindow w
    Gtk.drawWindowGetPointer dwin
    return False

proximityHandler wname b evt = do
    produce' "Mouse" (wname ++ ".KeyboardMouseWidget") "Proximity" once [EBool . Gtk.eventInContact $ evt] b
    return False

-- | Bind a keyboard mouse widget to the given Gtk widget. Se module documentation for description of events.
bindMouseKeyboardWidget :: Gtk.Widget -> Widget [EData a]
bindMouseKeyboardWidget w b = do
    ref <- newEmptyMVar
    wname <- Gtk.widgetGetName w
    Gtk.onButtonPress w (buttonHandler wname b)
    Gtk.onButtonRelease w (buttonHandler wname b)
    Gtk.onScroll w (scrollWheelHandler wname b)
    Gtk.onKeyPress w (keyboardHandler wname b)
    Gtk.onKeyRelease w (keyboardHandler wname b)
    Gtk.onMotionNotify w True (motionHandler wname w b)
    Gtk.onProximityIn w (proximityHandler wname b)
    Gtk.onProximityOut w (proximityHandler wname b)
    return ()
