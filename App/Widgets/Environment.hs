module App.Widgets.Environment where

import Control.Monad
import Data.Either
import Data.List (elem)
import App.EventBus
import System.Environment
import Data.Maybe
import Text.Parsec hiding (many)
import Control.Applicative

isNotBlankLine = (/=0) . length . filter (/=' ') . filter (/='\t')
isNotCommentLine = not . (=='#') . head
hasValue = elem '='
parseConfigLine = liftA2 (,) (many1 alphaNum) (spaces *> char '=' *> many anyChar)

-- | Place the command line arguments on the bus as an Event following the pattern
--
--    * name : argv
--
--    * group : Environment
--
--    * source : CommandLineArgsWidget
--
--    * timespan : Persistent
--
--    * data : EStringL of the command line args
commandLineArgsWidget :: Widget [EData a]
commandLineArgsWidget b = getArgs >>=
    \args -> produce' "Environment" "CommandLineArgsWidget" "argv" Persistent [EStringL args] b

-- | Read a config file and place it on the bus as individual events for each config item following the pattern:
--
--     * name : config item name
--
--     * group : Environment
--
--     * source : /filename/.ConfigFileWidget
--
--     * timespan : Persistent
--
--     * data : EString config item value
--
--   Config files follow a fairly simple grammar:
--
--   ConfigFile := [ConfigLine]
--
--   ConfigLine := <key> spaces = spaces <value> endl | CommentLine | BlankLine
--
--   CommentLine := # anychars endl
--
--   BlankLine := spaces endl
configFileWidget :: String -> Widget [EData a]
configFileWidget f b = configLines >>= mapM_ produceConfigDataEvent
    where produceConfigDataEvent (n,v) = produce' "Environment" (f ++ ".ConfigFileWidget") n Persistent [EString v] b
          configLines = rights . map (parse parseConfigLine "(Unknown line in config file)")
                            . filter isNotBlankLine
                            . filter isNotCommentLine
                            . filter hasValue
                            . lines
                            <$> readFile f

-- | Read in all environment variables and place them on the bus individually as events following the pattern:
--
--     * name : variable name
--
--     * group : Environment
--
--     * source : EnvironmentWidget
--
--     * timespan : Persistent
--
--     * data : EString variable value
--
environmentWidget :: Widget [EData a]
environmentWidget b = getEnvironment >>=
    mapM_ (\(k,v) -> produce' "Environment" "EnvironmentWidget" k Persistent [EString v] b)

-- | Set the program name as an event on the bus using the following pattern:
--
--     * name : ProgramName
--
--     * group : Environment
--
--     * source : ProgramNameWidget
--
--     * timespan : Persistent
--
--     * data : EString progran name
progNameWidget :: Widget [EData a]
progNameWidget b = getProgName >>=
    \v -> produce' "Environment" "ProgramNameWidget" "ProgramName" Persistent [EString v] b
