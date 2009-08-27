{-# LANGUAGE ScopedTypeVariables #-}
-- |  
-- Module      :  App.Behaviours.FileOps
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
--   This module handles read, write, encode, and decode of files.  It also cleanly handles exceptions
--   by introducing Exception events that are handlable by the behaviours in "App.Behaviours.Exception"
--   which exit your program gracefully, or by your own user defined exception handlers.
--
--   It can handle datatypes @EData a@ with Binary, Show, and Read instances as well.
module App.Behaviours.FileOps where

import App.EventBus
import Control.Applicative
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Binary
import qualified Control.Exception as Ex

emitException :: Event [EData a] -> String -> String -> Ex.IOException -> IO [Diff [EData a]]
emitException e n str ex = produce "Exception" str n once (EString (show ex):eventdata e) >>= return . (:[])
-- | @readFileBehaviour name datatype@ looks for any event with the name /name/ and reads the file
--   into an event following the pattern:
--
--      * name: same as filename.
--
--      * group: same as name of the behaviour, @name@
--
--      * source: \"ReadSource\"
--
--      * timespan: Persistent
--
--      * eventdata: the file, read in and processed using @read@ to be of the datatype that
--        corresponds to the constructor in the @datatype@ parameter.
--
--  NOTE: This function can only be used with @EData a@ where @a@ has a 'Read' instance.  For event
--  data without a read instance, use 'readFileBehaviourNR'
readFileBehaviour :: Read a => String -> EData a -> Behaviour [EData a]
readFileBehaviour n d b = consumeNamedEventsWith b n readFileCatch
    where readFileCatch e = Ex.catch (readFile0 e) (emitException e n "readFileBehaviour")
          readFile0 e = (rFile0 d . eventdata $ e) >>= produce n "ReadSource" (filename e) Persistent >>= return . (:[])
          filename = (\(EString x) -> x) . head . eventdata

-- | @readFileBehaviourNR name datatype@ looks for any event with the name /name/ and reads the file
--   into an event following the pattern:
--
--      * name: same as filename.
--
--      * group: same as name of the behaviour, @name@
--
--      * source: \"ReadSource\"
--
--      * timespan: Persistent
--
--      * eventdata: the file, read in and processed using 'read' to be of the datatype that
--        corresponds to the constructor in the @datatype@ parameter.  The constructor itself should
--        not be serialized.
--
--  NOTE: Attempting to read datatype @EOther a@ using this will cause the program to emit an
--  event with \"Exception\" as the group and /name/ as the source.
readFileBehaviourNR :: String -> EData a -> Behaviour [EData a]
readFileBehaviourNR n d b = consumeNamedEventsWith b n readFileCatch
    where readFileCatch e = Ex.catch (readFile0 e) (emitException e n "readFileBehaviour")
          readFile0 e = (rFile d . eventdata $ e) >>= produce n "ReadSource" (filename e) Persistent >>= return . (:[])
          filename = (\(EString x) -> x) . head . eventdata

-- | @decodeFileBehaviour name datatype@ looks for any event with the name /name/ and reads the file
--   into an event following the pattern:
--
--      * name: same as filename.
--
--      * group: same as name of the behaviour, @name@
--
--      * source: \"ReadSource\"
--
--      * timespan: Persistent
--
--      * eventdata: the file, read in and processed using 'Data.Binary.decodeFile' to be of the
--        datatype that corresponds to the constructor in the @datatype@ parameter.  The constructor
--        itself need not be serialized.
--
--  NOTE: This function can only be used with @EData a@ where @a@ has a 'Binary' instance.  For event
--  data without a read instance, use 'decodeFileBehaviourNB'
decodeFileBehaviour :: Binary a => String -> EData a -> Behaviour [EData a]
decodeFileBehaviour n d b = consumeNamedEventsWith b n decodeFileCatch
    where decodeFileCatch e = Ex.catch (decodeFile0 e) (emitException e n "decodeFileBehaviour")
          decodeFile0 e = (dFile0 d . eventdata $ e) >>= produce n "ReadSource" (filename e) Persistent >>= return . (:[])
          filename = (\(EString x) -> x) . head . eventdata

-- | @readFileBehaviour name datatype@ looks for any event with the name /name/ and reads the file
--   into an event following the pattern:
--
--      * name: same as filename.
--
--      * group: same as name of the behaviour, @name@
--
--      * source: \"ReadSource\"
--
--      * timespan: Persistent
--
--      * eventdata: the file, read in and processed using @read@ to be of the datatype that
--        corresponds to the constructor in the @datatype@ parameter.
--
--  NOTE: Attempting to read datatype @EOther a@ using this will cause the program to raise an
--  Event with \"Exception\" as the group.
decodeFileBehaviourNB :: String -> EData a -> Behaviour [EData a]
decodeFileBehaviourNB n d b = consumeNamedEventsWith b n decodeFileCatch
    where decodeFileCatch e = Ex.catch (decodeFile0 e) (emitException e n "decodeFileBehaviourNB")
          decodeFile0 e = (dFile d . eventdata $ e) >>= (produce n "ReadSource" (filename e) Persistent) >>= return . (:[])
          filename = (\(EString x) -> x) . head . eventdata

-- | @writeFileBehaviour@ looks for \"WriteFile\" named events with event data corresponding to
--   @[EString filepath,@ /data constructor/ @contents]@ and removes them from the bus, writing
--   the file named @filepath@.  Any error is placed on the bus with an Exception event with
--    \"WriteFile\" as the source.
writeFileBehaviourNS :: Behaviour [EData a]
writeFileBehaviourNS b = consumeNamedEventsWith b "WriteFile" $ \e -> Ex.catch
                            (wFile . eventdata $ e)
                            (emitException e "writeFileBehaviourNS" "WriteFile" )

-- | @writeFileBehaviour@ looks for \"WriteFile\" named events with event data corresponding to
--   @[EString filepath,@ /data constructor/ @contents]@ and removes them from the bus, writing
--   the file named @filepath@.  Any error is placed on the bus with an Exception event with
--    \"WriteFile\" as the source.
--
--   NOTE: Attempting to encode 'EOther a' using this will raise an Exception.
writeFileBehaviour :: Show a => Behaviour [EData a]
writeFileBehaviour b = consumeNamedEventsWith b "WriteFile" $ \e -> Ex.catch
                            (wFile0 . eventdata $ e)
                            (emitException e "writeFileBehaviourNS" "WriteFile" )

-- | @writeFileBehaviour@ looks for \"WriteFile\" named events with event data corresponding to
--   @[EString filepath,@ /data constructor/ @contents]@ and removes them from the bus, writing
--   the file named @filepath@.  Any error is placed on the bus with an Exception event with
--    \"WriteFile\" as the source.
--
--  NOTE: Attempting to encode 'EOther a' using this will raise an Exception.
encodeFileBehaviourNB :: Behaviour [EData a]
encodeFileBehaviourNB b = consumeNamedEventsWith b "WriteBinary" $ \e -> Ex.catch
                            (wBinary . eventdata $ e)
                            (emitException e "encodeFileBehaviourNS" "WriteFile")

-- | @writeFileBehaviour@ looks for \"WriteFile\" named events with event data corresponding to
--   @[EString filepath,@ /data constructor/ @contents]@ and removes them from the bus, writing
--   the file named @filepath@.  Any error is placed on the bus with an Exception event with
--    \"WriteFile\" as the source.
--
--  NOTE: This can only be used with an EData a where a has a 'Data.Binary.Binary' instance.
encodeFileBehaviour :: Binary a => Behaviour [EData a]
encodeFileBehaviour b = consumeNamedEventsWith b "WriteBinary" $ \e -> Ex.catch
                            (wBinary0 . eventdata $ e)
                            (emitException e "encodeFileBehaviourNS" "WriteFile" )

wFile [EString filepath, EString contents] = [] <$ writeFile filepath contents
wFile [EString filepath, EStringL contents] = [] <$ (writeFile filepath . unlines $ contents)
wFile [EString filepath, ELByteString contents] = [] <$ (LB.writeFile filepath contents)
wFile [EString filepath, ELByteStringL contents] = [] <$ (LB.writeFile filepath . LB.unlines $ contents)
wFile [EString filepath, EByteString contents] = [] <$ SB.writeFile filepath contents
wFile [EString filepath, EByteStringL contents] = [] <$ (SB.writeFile filepath . SB.unlines $ contents)
wFile [EString filepath, EInt contents] = [] <$ (writeFile filepath . show $ contents)
wFile [EString filepath, EIntL contents] = [] <$ (writeFile filepath . show $ contents)
wFile [EString filepath, EDouble contents] = [] <$ (writeFile filepath . show $ contents)
wFile [EString filepath, EDoubleL contents] = [] <$ (writeFile filepath . show $ contents)
wFile [EString filepath, EBool contents] = [] <$ (writeFile filepath . show $ contents)
wFile [EString filepath, EBoolL contents] = [] <$ (writeFile filepath . show $ contents)

wFile0 [EString filepath, EString contents] = [] <$ writeFile filepath contents
wFile0 [EString filepath, EStringL contents] = [] <$ (writeFile filepath . unlines $ contents)
wFile0 [EString filepath, ELByteString contents] = [] <$ (LB.writeFile filepath contents)
wFile0 [EString filepath, ELByteStringL contents] = [] <$ (LB.writeFile filepath . LB.unlines $ contents)
wFile0 [EString filepath, EByteString contents] = [] <$ SB.writeFile filepath contents
wFile0 [EString filepath, EByteStringL contents] = [] <$ (SB.writeFile filepath . SB.unlines $ contents)
wFile0 [EString filepath, EInt contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EIntL contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EDouble contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EDoubleL contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EBool contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EBoolL contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EOther contents] = [] <$ (writeFile filepath . show $ contents)
wFile0 [EString filepath, EOtherL contents] = [] <$ (writeFile filepath . show $ contents)

wBinary [EString filepath, EString contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EStringL contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EByteString contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EByteStringL contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, ELByteString contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, ELByteStringL contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EInt contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EIntL contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EDouble contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EDoubleL contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EBool contents] = [] <$ (encodeFile filepath contents)
wBinary [EString filepath, EBoolL contents] = [] <$ (encodeFile filepath contents)


wBinary0 [EString filepath, EString contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EStringL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EByteString contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EByteStringL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, ELByteString contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, ELByteStringL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EInt contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EIntL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EDouble contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EDoubleL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EBool contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EBoolL contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EOther contents] = [] <$ (encodeFile filepath contents)
wBinary0 [EString filepath, EOtherL contents] = [] <$ (encodeFile filepath contents)

--rFile :: EData a -> [EData a] -> IO [EData a]
rFile (EString _) [EString filepath] = (:[]) . EString . LB.unpack <$> LB.readFile filepath
rFile (EStringL _) [EString filepath] = (:[]) . EStringL . read . LB.unpack <$> LB.readFile filepath
rFile (EByteString _) [EString filepath] = (:[]) . EByteString <$> SB.readFile filepath
rFile (EByteStringL _) [EString filepath] = (:[]) . EByteStringL . SB.lines <$> SB.readFile filepath
rFile (ELByteString _) [EString filepath] = (:[]) . ELByteString <$> LB.readFile filepath
rFile (ELByteStringL _) [EString filepath] = (:[]) . ELByteStringL . LB.lines <$> LB.readFile filepath
rFile (EInt _) [EString filepath] = (:[]) . EInt . read . LB.unpack <$> LB.readFile filepath
rFile (EIntL _) [EString filepath] = (:[]) . EIntL . read . LB.unpack <$> LB.readFile filepath
rFile (EDouble _) [EString filepath] = (:[]) . EDouble . read . LB.unpack <$> LB.readFile filepath
rFile (EDoubleL _) [EString filepath] = (:[]) . EDoubleL .  read . LB.unpack <$> LB.readFile filepath
rFile (EBool _) [EString filepath] = (:[]) . EBool . read . LB.unpack <$> LB.readFile filepath
rFile (EBoolL _) [EString filepath] = (:[]) . EBoolL . read . LB.unpack <$> LB.readFile filepath

--dFile :: EData a -> [EData a] -> IO [EData a]
dFile (EString _) [EString filepath] = (:[]) .EString <$> decodeFile filepath
dFile (EStringL _) [EString filepath] = (:[]) .EStringL <$>  decodeFile filepath
dFile (EByteString _) [EString filepath] = (:[]) .EByteString <$> decodeFile filepath
dFile (EByteStringL _) [EString filepath] = (:[]) .EByteStringL <$> decodeFile filepath
dFile (ELByteString _) [EString filepath] = (:[]) .ELByteString <$> decodeFile filepath
dFile (ELByteStringL _) [EString filepath] = (:[]) .ELByteStringL <$> decodeFile filepath
dFile (EInt _) [EString filepath] =(:[]) . EInt <$> decodeFile filepath
dFile (EIntL _) [EString filepath] =(:[]) . EIntL <$> decodeFile filepath
dFile (EDouble _) [EString filepath] =(:[]) . EDouble <$> decodeFile filepath
dFile (EDoubleL _) [EString filepath] =(:[]) . EDoubleL <$> decodeFile filepath
dFile (EBool _) [EString filepath] =(:[]) . EBool <$> decodeFile filepath
dFile (EBoolL _) [EString filepath] =(:[]) . EBoolL <$> decodeFile filepath

--rFile0 :: Read a => EData a -> [EData a] -> IO [EData a]
rFile0 (EString _) [EString filepath] =(:[]) . EString . LB.unpack <$> LB.readFile filepath
rFile0 (EStringL _) [EString filepath] =(:[]) . EStringL . read . LB.unpack <$> LB.readFile filepath
rFile0 (EByteString _) [EString filepath] =(:[]) . EByteString <$> SB.readFile filepath
rFile0 (EByteStringL _) [EString filepath] =(:[]) . EByteStringL . SB.lines <$> SB.readFile filepath
rFile0 (ELByteString _) [EString filepath] =(:[]) . ELByteString <$> LB.readFile filepath
rFile0 (ELByteStringL _) [EString filepath] =(:[])  . ELByteStringL . LB.lines <$> LB.readFile filepath
rFile0 (EInt _) [EString filepath] =(:[]) . EInt . read . LB.unpack <$> LB.readFile filepath
rFile0 (EIntL _) [EString filepath] =(:[]) . EIntL . read . LB.unpack <$> LB.readFile filepath
rFile0 (EDouble _) [EString filepath] = (:[]) . EDouble . read . LB.unpack <$> LB.readFile filepath
rFile0 (EDoubleL _) [EString filepath] = (:[]) . EDoubleL .  read . LB.unpack <$> LB.readFile filepath
rFile0 (EBool _) [EString filepath] =(:[]) . EBool . read . LB.unpack <$> LB.readFile filepath
rFile0 (EBoolL _) [EString filepath] =(:[]) . EBoolL . read . LB.unpack <$> LB.readFile filepath
rFile0 (EOther _) [EString filepath] =(:[]) . EOther . read . LB.unpack <$> LB.readFile filepath
rFile0 (EOtherL _) [EString filepath] =(:[]) . EOtherL . read . LB.unpack <$> LB.readFile filepath

--dFile0 :: Binary a => EData a -> [EData a] -> IO [EData a]
dFile0 (EString _) [EString filepath] = (:[]) . EString <$> decodeFile filepath
dFile0 (EStringL _) [EString filepath] =(:[]) . EStringL <$>  decodeFile filepath
dFile0 (EByteString _) [EString filepath] =(:[]) . EByteString <$> decodeFile filepath
dFile0 (EByteStringL _) [EString filepath] = (:[]) . EByteStringL <$> decodeFile filepath
dFile0 (ELByteString _) [EString filepath] = (:[]) . ELByteString <$> decodeFile filepath
dFile0 (ELByteStringL _) [EString filepath] = (:[]) . ELByteStringL <$> decodeFile filepath
dFile0 (EInt _) [EString filepath] = (:[]) . EInt <$> decodeFile filepath
dFile0 (EIntL _) [EString filepath] = (:[]) . EIntL <$> decodeFile filepath
dFile0 (EDouble _) [EString filepath] = (:[]) . EDouble <$> decodeFile filepath
dFile0 (EDoubleL _) [EString filepath] = (:[]) . EDoubleL <$> decodeFile filepath
dFile0 (EBool _) [EString filepath] = (:[]) . EBool <$> decodeFile filepath
dFile0 (EBoolL _) [EString filepath] = (:[]) . EBoolL <$> decodeFile filepath
dFile0 (EOther _) [EString filepath] = (:[]) . EOther <$> decodeFile filepath
dFile0 (EOtherL _) [EString filepath] = (:[]) . EOtherL <$> decodeFile filepath
