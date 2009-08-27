{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
-- | 
-- Module      :  App.EventBus
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
-- Not exactly the FRP model, but rather a model of a large application with
-- heterogenous data and many inputs and outputs.  An application is in its
-- essence a collection of widgets and behaviours and events with a bus.
-- The bus holds events and manages the event timeline.  Behaviours and
-- widgets are continuous. Widgets applied to the bus make insertions and
-- never deletions. Behaviours applied to the bus make insertions and deletions.
--
-- Behaviours are composable using combinators that set one Behaviour as either
-- behind, in front, or beside another behaviour on the bus.  The in front and
-- behind combinators  establish that the behaviour "behind" the others
-- sees the results of the other behaviours' application to the bus. The beside
-- combinator says that the combinators see the same bus.
--
module App.EventBus where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.List (foldl', foldl1')
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Clock
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO.Unsafe
import Debug.Trace

-- generic functions for key ordering.  move somewhere else later
EQ />/ b = b
a />/ _ = a

a /</ EQ = a
_ /</ b = b

g %=> f = f `on` g

on f g a b = f (g a) (g b)

-- IO version of the <* Applicative operator
a =<<^ b = \m -> b >> a m

-- | Defines the amount of time that an event exists.
data TimeSpan =
      Persistent            -- ^ The event exists forever
    | Time DiffTime         -- ^ The event exists for a specified amount of real time
    | Iterations Int        -- ^ The event exists for a certain number of samples of time from its inception.
    deriving (Eq,Ord,Show)

seconds :: Integer -> TimeSpan
seconds = Time . secondsToDiffTime

minutes :: Integer -> TimeSpan
minutes = Time . secondsToDiffTime . (60*)

hours :: Integer -> TimeSpan
hours = Time . secondsToDiffTime . (3600*)

days :: Integer -> TimeSpan
days = Time . secondsToDiffTime . (86400*)

once :: TimeSpan
once = Iterations 1

-- | Defines time in terms of the differences from time t0 to the next instant. This is the type
--   returned by Behaviours to describe time directly after the Behaviour.
data Diff a =
      Insertion (Event a)   -- ^ Time t1 contains all events at time t0 plus this event.
    | Deletion (Event a)    -- ^ Time t1 contains all events at time t0 minus this event.

instance Show (Diff a) where
    show (Insertion a) = show ("Insertion",group a, src a, ename a, timespan a)
    show (Deletion a) = show ("Deletion",group a, src a, ename a, timespan a)


-- | Defines the data attachable to events.
data EData a =
      EString String
    | EByteString B.ByteString
    | EByteStringL [B.ByteString]
    | ELByteString LB.ByteString
    | ELByteStringL [LB.ByteString]
    | EChar Char
    | EDouble Double
    | EInt Int
    | EBool Bool
    | EStringL [String]
    | EDoubleL [Double]
    | EIntL [Int]
    | EBoolL [Bool]
    | EOther a
    | EAssoc (String,EData a)
    | EAssocL [(String,EData a)]
    | EOtherL [a]
    deriving (Eq, Show, Read)

fromEString (EString a) = a
fromEByteString (EByteString a) = a
fromEByteStringL (EByteStringL a) = a
fromELByteString (ELByteString a) = a
fromELByteStringL (ELByteStringL a) = a
fromEChar (EChar a) = a
fromEDouble (EDouble a) = a
fromEInt (EInt a) = a
fromEBool (EBool a) = a
fromEStringL (EStringL a) = a
fromEDoubleL (EDoubleL a) = a
fromEIntL (EIntL a) = a
fromEBoolL (EBoolL a) = a
fromEOther (EOther a) = a
fromEAssoc (EAssoc a) = a
fromEAssocL (EAssocL a) = a
fromEOtherL (EOtherL a) = a

-- | Show without risking running into an unshowable type.
safeShow :: Maybe Int -> EData a -> String
safeShow n (EString s) = maybe s ((flip take) s) n
safeShow n (EStringL s) = maybe (show s) ((flip take) (show s)) n
safeShow n (EByteString _) = "ByteString data"
safeShow n (EByteStringL _) = "ByteString list data"
safeShow n (EChar c) = [c]
safeShow n (EDouble x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EDoubleL x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EInt x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EIntL x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EBool x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EBoolL x) = maybe (show x) ((flip take) (show x)) n
safeShow n (EAssoc (x,y)) = x ++ " -> " ++ safeShow n y
safeShow n (EAssocL xs) = concat $ (\(a,b) -> "(" ++ a ++ " -> " ++ safeShow n b ++ ")\n" ) <$> xs
safeShow n (EOther _) = "Other data"
safeShow n (EOtherL _) = "Other data list"

-- | An discrete event in time
data Event a = Event
    { ename :: String            -- ^ The unique name of an event.  Group + src + name = the fully qualified name FQN of the event.
    , group :: String           -- ^ The group of an event.
    , timespan :: TimeSpan      -- ^ The timespan from "time" that an event exists.
    , eventdata :: a    -- ^ The data attached to the event.
    , src :: String             -- ^ The behaviour or widget that assigned the event to time.
    , time :: UTCTime }         -- ^ The time of the event's inception.


instance Ord (Event a) where
    compare l r = ((src %=> compare) l r) />/
                  ((group %=> compare) l r) />/
                  ((ename %=> compare) l r)

instance Eq (Event a) where
    x == y = (ename %=> (==)) x y &&
             (group %=> (==)) x y &&
             (src %=> (==)) x y

-- | The type of a discrete sample of continuous time.
data Bus a = Bus
    { nameMap :: Map.Map String (Set.Set (Event a))                     -- ^ The map of just Event.name to events.
    , srcMap :: Map.Map String (Set.Set (Event a))                      -- ^ The map of just Event.src to events.
    , groupMap :: Map.Map String (Set.Set (Event a))                    -- ^ The map of just Event.group to events.
    , fullyQualifiedMap :: Map.Map (String,String,String) (Event a) }   -- ^ The map of FQNs to events.

instrument bname behave = behave 

instance Show (Bus a) where
    show = concat . map showQName . Map.elems . fullyQualifiedMap

showQName ev = show (group ev, src ev, ename ev, timespan ev)

eventsByName :: String -> Bus a -> Set.Set (Event a)
eventsByName n = fromMaybe Set.empty . Map.lookup n . nameMap

eventsBySource :: String -> Bus a -> Set.Set (Event a)
eventsBySource s = fromMaybe Set.empty . Map.lookup s . srcMap

eventsByGroup :: String -> Bus a -> Set.Set (Event a)
eventsByGroup g = fromMaybe Set.empty . Map.lookup g . groupMap

eventByQName:: String -> String -> String -> Bus a -> Maybe (Event a)
eventByQName g s n = Map.lookup (g,s,n) . fullyQualifiedMap

eventsFor (Just g) Nothing Nothing b = eventsByGroup g b
eventsFor Nothing (Just s) Nothing b = eventsBySource s b
eventsFor Nothing Nothing (Just n) b = eventsByName n b
eventsFor (Just g) (Just s) (Just n) b = maybe Set.empty (Set.singleton) (eventByQName g s n b)
eventsFor g s n b = persection gset . persection sset $ nset
    where gset = fromMaybe Set.empty $ (flip eventsByGroup) b <$> g
          sset = fromMaybe Set.empty $ (flip eventsBySource) b <$> s
          nset = fromMaybe Set.empty $ (flip eventsByName) b <$> n
          persection a b | a == Set.empty = b
                         | b == Set.empty = a
                         | otherwise = Set.intersection a b

filteredEventsFor (Left g) (Right sfilter) (Right nfilter) b = filter nfilter . filter sfilter . Set.toList $ eventsByGroup g b
filteredEventsFor (Right gfilter) (Left s) (Right nfilter) b = filter gfilter . filter nfilter . Set.toList $ eventsBySource s b
filteredEventsFor (Right gfilter) (Right sfilter) (Left n) b = filter gfilter . filter sfilter . Set.toList $ eventsByName n b
filteredEventsFor (Left g) (Left s) (Left n) b = maybe [] (\a -> [a]) (eventByQName g s n b)
filteredEventsFor (Right gfilter) (Right sfilter) (Right nfilter) b = filter gfilter . filter sfilter . filter nfilter . map snd . Map.toList . fullyQualifiedMap $ b
filteredEventsFor (Right gfilter) (Left s) (Left n) b = filter gfilter . Set.toList$ eventsFor Nothing (Just s) (Just n) b
filteredEventsFor (Left g) (Right sfilter) (Left n) b = filter sfilter . Set.toList $ eventsFor (Just g) Nothing (Just n) b
filteredEventsFor (Left g) (Left s) (Right nfilter) b = filter nfilter . Set.toList $ eventsFor (Just g) (Just s) Nothing b

topEvent = head . Set.toList

instance Monoid (Bus a) where
    mempty = emptyBus
    mappend (Bus n0 s0 g0 f0) (Bus n1 s1 g1 f1) = Bus (Map.union n0 n1) (Map.union s0 s1) (Map.union g0 g1) (Map.union f0 f1)

-- | The empty bus
emptyBus :: Bus a
emptyBus = Bus Map.empty Map.empty Map.empty Map.empty

-- | Add an event to time within the bus
addEvent :: Event a -> Bus a -> Bus a
addEvent edata b = b{ nameMap = Map.insertWith (Set.union) (ename edata) (singleton edata) (nameMap b)
                    , srcMap = Map.insertWith (Set.union) (src edata) (singleton edata) (srcMap b)
                    , groupMap = Map.insertWith (Set.union) (group edata) (singleton edata) (groupMap b)
                    , fullyQualifiedMap = Map.insert (group edata, src edata, ename edata) edata (fullyQualifiedMap b) }

-- | The type of widgets.
--   A widget is an input-only way to assign Events to time.  A mouse is a widget.  A keyboard is a
--   widget.  A webcam is a widget, and so on.
type Widget a = MVar (Bus a) -> IO ()

-- | The type of future events..
--   A behaviour doesn't know about the time that it assigns events, only that they exist
--   at some point after the time that the Behaviour sampled.
type Future a = IO (Bus a, MVar [Diff a])

-- | An IO action sometime in the future.
future :: Bus a -> IO [Diff a] -> Future a
future b thunk = do
    ref <- newEmptyMVar
    thunk >>= putMVar ref
    return (b,ref)

-- | Obtain the final value of a Future.  Blocks until the value is available
immediate = takeMVar

-- | The type of a Behaviour.  A behaviour maps the bus to a list of differences to apply to the bus
--   before the next Behaviour's sample of time.
type Behaviour a = Bus a -> Future a

instance Monoid (Behaviour a) where
    mempty = passthrough
    mappend = (>~>) -- x behind y

-- | The null Behaviour.  Samples the bus and adds and deletes nothing.
passthrough :: Behaviour a
passthrough a = future a (return [])

-- | the in front of behaviour combinator. behaviour 1 is in front of behaviour 0, so behavour 0 will see the bus filtered through behaviour 1
(<~<) :: Behaviour a -> Behaviour a -> Behaviour a
behaviour1 <~< behaviour0 = \m -> behaviour0 m >>= applyDiff >>= behaviour1

-- | the behind behaviour combinator. behaviour 0 is behind behaviour 1, so behaviour 0 will see the bus filtered through behaviour 1
(>~>) :: Behaviour a -> Behaviour a -> Behaviour a
behaviour0 >~> behaviour1 = \m -> behaviour0 m >>= applyDiff >>= behaviour1

-- | the beside behaviour combinator. All behaviours that are side-by-side see the same bus.
(|~|) :: Behaviour a -> Behaviour a -> Behaviour a
behaviour0 |~| behaviour1 = \m -> future m $ do
    (_,mv0) <- behaviour0 m
    (_,mv1) <- behaviour1 m
    value0 <- takeMVar mv0 -- :: IO [Diff a]
    value1 <- takeMVar mv1 -- :: IO [Diff a]
    return $ value0 ++ value1

behind = (>~>)
beside = (|~|)
infrontof = (<~<)

applyDiff (m,ds) = immediate ds >>= (\k -> {- trace ("------\n" ++ show k ++ "\n" ++ show m ++ "\n\nthen after diff applied\n\n" ++ (show . foldl' busDiff m $ k)) -} (return . foldl' busDiff m) $ k)
    where busDiff b (Insertion ev) = b{ nameMap = Map.insertWith (union') (ename ev) (singleton ev) (nameMap b)
                                      , srcMap = Map.insertWith (union') (src ev) (singleton ev) (srcMap b)
                                      , groupMap = Map.insertWith (union') (group ev) (singleton ev) (groupMap b)
                                      , fullyQualifiedMap = Map.insert (group ev, src ev, ename ev) ev (fullyQualifiedMap b) }
          busDiff b (Deletion ev) = b { nameMap = deleteOneFrom ev (ename ev) (nameMap b) -- should change this to alter instead of delete, check for empty lists
                                      , srcMap = deleteOneFrom ev (src ev) (srcMap b)
                                      , groupMap = deleteOneFrom ev (group ev) (groupMap b)
                                      , fullyQualifiedMap = Map.delete (group ev, src ev, ename ev) (fullyQualifiedMap b) }
          deleteOneFrom ev key mp = case Map.lookup key mp of
					Just eset -> let eset' = Set.delete ev eset in if eset' == Set.empty then Map.delete key mp else Map.insert key eset' mp
					Nothing -> mp
          union' v st = Set.union (Set.difference st v) v				


-- | An infinite loop of behaviours and widgets over time, sampled forward.
bus :: [Widget a] -> IO b -> Behaviour a -> IO ()
bus widgets widgetThunk behaviour = do
    evBus <- newMVar emptyBus
    forM_ widgets ($evBus)

    let loop = do
        widgetThunk
        busIteration evBus behaviour
        loop

    loop

-- | Sample time and apply the behaviour to that sample.
busIteration :: MVar (Bus a) -> Behaviour a -> IO ()
busIteration b behaviour = do
    v <- tryTakeMVar b
    case v of
        Nothing -> return ()
        Just m -> do diffs <- behaviour m
                     bus' <- applyDiff diffs
                     bus'' <- expire <$> decrementTimeSpan bus'
                     putMVar b bus''

-- | Assign an event to time given some event data and a TimeSpan.
--
--   @produce group source nm timetolive edata@
produce :: String -> String -> String -> TimeSpan -> a -> IO (Diff a)
produce group source nm timetolive edata =
    (return . Insertion . Event nm group timetolive edata source) =<< getCurrentTime

-- | Assign an event to time from a widget.
--
-- @produce' group source nm timetolive edata bus@
produce' :: String -> String -> String -> TimeSpan -> a -> MVar (Bus a) -> IO ()
produce' group source nm timetolive edata b = getCurrentTime >>= \t -> modifyMVar_ b (return . addEvent (Event nm group timetolive edata source t))

-- | Sample all events with a given name at the current time and output their deletions as Diffs as
--   well as any additional Diffs returned by the behaviour.
consumeNamedEventsCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
consumeNamedEventsCollectivelyWith em nm f =
    maybe (future em . return $ [])
          (\ev -> future em $ (map Deletion (Set.toList ev) ++) <$> f ev)
          (Map.lookup nm (nameMap em))

consumeNamedEvents :: String -> Behaviour a
consumeNamedEvents nm b =
    maybe (future b . return $ [])
          (\ev -> future b . return $ Deletion <$> Set.toList ev)
          (Map.lookup nm . nameMap $ b)

consumeEventGroup :: String -> Behaviour a
consumeEventGroup g b =
    maybe (future b . return $ [])
          (\ev -> future b . return $ Deletion <$> Set.toList ev)
          (Map.lookup g . groupMap $ b)

consumeEventsFromSource :: String -> Behaviour a
consumeEventsFromSource s b =
    maybe (future b . return $ [])
          (\ev -> future b . return $ Deletion <$> Set.toList ev)
          (Map.lookup s . srcMap $ b)

consumeFullyQualifiedEvent :: String -> String -> String -> Behaviour a
consumeFullyQualifiedEvent g s n b =
    maybe (future b . return $ [])
          (\ev -> future b . return $ [Deletion ev])
          (Map.lookup (g, s, n) . fullyQualifiedMap $ b)

modifyEventData :: Event a -> (a -> a) -> [Diff a]
modifyEventData ev f = [Insertion ev{ eventdata = f . eventdata $ ev }]

modifyEvent :: Event a -> (Event a -> Event a) -> [Diff a]
modifyEvent ev f = let ev' = f ev in if ev==ev' then [Insertion ev'] else [Deletion ev, Insertion ev']

consumeNamedEventsWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
consumeNamedEventsWith b n f =
    future b $ concat <$> ((\l -> (map Deletion l :) <$>  mapM f l) . Set.toList $ fromMaybe Set.empty (Map.lookup n (nameMap b)))

-- | Sample all events with a given group at the current time and output their deletions as Diffs as
--   well as any additional Diffs returned by the behaviour.
consumeEventGroupCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
consumeEventGroupCollectivelyWith em gp f =
    maybe (future em . return $ [])
          (\ev -> future em $ (map Deletion (Set.toList ev) ++) <$> f ev)
          (Map.lookup gp (groupMap em))

consumeEventGroupWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
consumeEventGroupWith b n f =
    future b $ concat <$> ((\l -> (map Deletion l :) <$>  mapM f l) . Set.toList $ fromMaybe Set.empty (Map.lookup n (groupMap b)))

-- | Sample all events with a given source at the current time and output their deletions as Diffs as
--   well as any additional Diffs returned by the behaviour.
consumeEventsFromSourceCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
consumeEventsFromSourceCollectivelyWith em source f =
    maybe (future em . return $ [])
          (\ev -> future em $ (map Deletion (Set.toList ev) ++) <$> f ev)
          (Map.lookup source (srcMap em))

consumeEventsFromSourceWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
consumeEventsFromSourceWith b n f =
    future b $ concat <$> ((\l -> (map Deletion l :) <$>  mapM f l) . Set.toList $ fromMaybe Set.empty (Map.lookup n (srcMap b)))

-- | Sample a single fully qualified event at the current time and output their deletions as Diffs as
--   well as any additional Diffs returned by the behaviour.  Parameter order is bus, group, source, name
consumeFullyQualifiedEventWith :: Bus a -> String -> String -> String -> (Event a -> IO [Diff a]) -> Future a
consumeFullyQualifiedEventWith em group source name f =
    maybe (future em . return $ [])
          (\ev -> future em $ (Deletion ev :) <$> f ev)
          (Map.lookup (group,source,name) (fullyQualifiedMap em))

-- | Sample all events with a given name and apply a Behaviour
pollNamedEventsCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
pollNamedEventsCollectivelyWith b nm f = maybe (future b . return $[]) (future b . f) (Map.lookup nm (nameMap b))

-- | Sample all events with a given name and apply a Behaviour to each
pollNamedEventsWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
pollNamedEventsWith b nm f = future b $ concat <$> (mapM f . Set.toList $ fromMaybe Set.empty (Map.lookup nm (nameMap b)))

-- | Sample all events with a given group and apply a Behaviour
pollEventGroupCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
pollEventGroupCollectivelyWith b nm f = maybe (future b . return $[]) (future b . f) (Map.lookup nm (groupMap b))

-- | Sample all events with a gien group and apply a Behaviour to each.
pollEventGroupWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
pollEventGroupWith b nm f = future b $ concat <$> (mapM f . Set.toList $ fromMaybe Set.empty (Map.lookup nm (groupMap b)))

-- | Sample all events with a given source and apply a Behaviour
pollEventsFromSourceCollectivelyWith :: Bus a -> String -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
pollEventsFromSourceCollectivelyWith b nm f = maybe (future b . return $[]) (future b . f) (Map.lookup nm (srcMap b))

-- | Sample all events with a given source and apply a Behaviour to each.
pollEventsFromSourceWith :: Bus a -> String -> (Event a -> IO [Diff a]) -> Future a
pollEventsFromSourceWith b nm f = future b $ concat <$> (mapM f . Set.toList $ fromMaybe Set.empty (Map.lookup nm (srcMap b)))

-- | Sample a single fully qualified event and output some Diffs.
--   Parameter order is bus, group, source, name.
pollFullyQualifiedEventWith :: Bus a -> String -> String -> String -> (Event a -> IO [Diff a]) -> Future a
pollFullyQualifiedEventWith b gp source nm f = maybe (future b . return $ []) (future b . f) (Map.lookup (gp,source,nm) (fullyQualifiedMap b))

-- | Apply a behaviour to all events in the bus, one event at a time.
pollAllEventsWith :: Bus a -> (Event a -> IO [Diff a]) -> Future a
pollAllEventsWith b f = future b $ concat <$> (mapM f . Map.elems . fullyQualifiedMap $ b)

-- | Apply a behaviour to the collection of all events on the bus at once
pollAllEventsCollectivelyWith :: Bus a -> (Set.Set (Event a) -> IO [Diff a]) -> Future a
pollAllEventsCollectivelyWith b f = future b $ f . Set.fromList . Map.elems . fullyQualifiedMap $ b

					
singleton a = Set.fromList [a]					
	
decrementTimeSpan b = return $ b{ nameMap = Map.map decrements (nameMap b)
                           , srcMap = Map.map decrements (srcMap b)
                           , groupMap = Map.map decrements (groupMap b)
                           , fullyQualifiedMap = Map.map decrement (fullyQualifiedMap b) }
    where decrement e = e{ timespan = decTimeSpan e (timespan e) }
          decrements = Set.map (\e -> e{timespan = decTimeSpan e (timespan e)} )
          decTimeSpan _ Persistent = Persistent
          decTimeSpan e (Time x) = Time . realToFrac $ diffUTCTime (addUTCTime (realToFrac x) (time e)) (unsafePerformIO getCurrentTime)
          decTimeSpan _ (Iterations x) = (Iterations (x-1))

expire b = b'
	where current (Time x) = x > 0
	      current Persistent = True
	      current (Iterations x) = x > 0
	      b' = Bus (Map.filter (/=Set.empty) . Map.map (Set.filter (current . timespan)) . nameMap $ b)
                   (Map.filter (/=Set.empty) . Map.map (Set.filter (current . timespan)) . srcMap $ b)
                   (Map.filter (/=Set.empty) . Map.map (Set.filter (current . timespan)) . groupMap $ b)
                   (Map.filter (current . timespan) . fullyQualifiedMap $ b)

listM v = v >>= return . (:[])	
	
{- example usage...
 -
 - handleDataLoad :: Behaviour
 - ...
 -
 - handleZoom :: Behaviour
 - ...
 -
 - handlePan :: Behaviour
 - ...
 -
 - handleRot :: Behaviour
 - ...
 -
 - handleWriteData :: Behaviour
 - ...
 -
 - main = do
 -  ui <- getUIFromFile "something.glade"
 -  mapM_ makeGtkProducers ui
 -  multitouch <- getMultitouchProducer "localhost" 8080
 -  bus (multitouch:ui) $ handleDataLoad >~> handleZoom |~| handlePan |~| handleRot >~> handleWriteData
 -
 -}

{- example of generically wrapping a Gtk widget into a EventBus.Widget
 - buttonWidget :: Gtk.Widget -> Bus a -> IO (Behaviour a)
 - buttonWidget button em = Gtk.onClick button $ do
 -      name <- Gtk.getWidgetName button
 -      value <- EString <$> Gtk.getText button
 -      produce' "ui" name "Click" once em
 -}
