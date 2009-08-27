 
-- Module      :  App.Behaviours.XmlRpc
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
module App.Behaviours.XmlRpc where

import Control.Monad.Error
import Network.XmlRpc.Client
import Network.XmlRpc.Internals
import Control.Applicative
import App.EventBus
import qualified Codec.Binary.Base64
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe

edata2value :: XmlRpcType a => EData a -> Value
edata2value (EString x) = ValueString x
edata2value (EStringL x) = ValueArray $ ValueString <$> x
edata2value (EByteString x) = ValueBase64 . Codec.Binary.Base64.encode . SB.unpack $ x
edata2value (EByteStringL x) = ValueArray $ ValueBase64 . Codec.Binary.Base64.encode . SB.unpack <$> x
edata2value (ELByteString x) = ValueBase64 . Codec.Binary.Base64.encode . LB.unpack $ x
edata2value (ELByteStringL x) = ValueArray $ ValueBase64 . Codec.Binary.Base64.encode . LB.unpack <$> x
edata2value (EChar x) = ValueString [x]
edata2value (EDouble x) = ValueDouble x
edata2value (EDoubleL x) = ValueArray $ ValueDouble <$> x
edata2value (EInt x) = ValueInt x
edata2value (EIntL x) = ValueArray $ ValueInt <$> x
edata2value (EBool x) = ValueBool x
edata2value (EBoolL x) = ValueArray $ ValueBool <$> x
edata2value (EOther x) = toValue x
edata2value (EOtherL x) = ValueArray $ toValue <$> x
edata2value (EAssoc (k,v)) = ValueStruct [(k, edata2value v)]
edata2value (EAssocL xs) = ValueStruct $ (\(k,v) -> (k, edata2value v)) <$> xs

edata2valueNX :: EData a -> Value
edata2valueNX (EString x) = ValueString x
edata2valueNX (EStringL x) = ValueArray $ ValueString <$> x
edata2valueNX (EByteString x) = ValueBase64 . Codec.Binary.Base64.encode . SB.unpack $ x
edata2valueNX (EByteStringL x) = ValueArray $ ValueBase64 . Codec.Binary.Base64.encode . SB.unpack <$> x
edata2valueNX (ELByteString x) = ValueBase64 . Codec.Binary.Base64.encode . LB.unpack $ x
edata2valueNX (ELByteStringL x) = ValueArray $ ValueBase64 . Codec.Binary.Base64.encode . LB.unpack <$> x
edata2valueNX (EChar x) = ValueString [x]
edata2valueNX (EDouble x) = ValueDouble x
edata2valueNX (EDoubleL x) = ValueArray $ ValueDouble <$> x
edata2valueNX (EInt x) = ValueInt x
edata2valueNX (EIntL x) = ValueArray $ ValueInt <$> x
edata2valueNX (EBool x) = ValueBool x
edata2valueNX (EBoolL x) = ValueArray $ ValueBool <$> x
edata2valueNX (EAssoc (k,v)) = ValueStruct [(k, edata2valueNX v)]
edata2valueNX (EAssocL xs) = ValueStruct $ (\(k,v) -> (k, edata2valueNX v)) <$> xs

value2edata :: Value -> EData a
value2edata (ValueInt x) = EInt x
value2edata (ValueBool x) = EBool x
value2edata (ValueString x) = EString x
value2edata (ValueDateTime x) = EString (show x)
value2edata (ValueBase64 x) = EByteString . SB.pack . fromJust . Codec.Binary.Base64.decode $ x
value2edata (ValueStruct xs) = EAssocL $ (\(x,y) -> (x, value2edata y)) <$> xs
value2edata (ValueArray xs) = EAssocL . zip (show<$>[0..]) $ value2edata <$> xs



xmlrpcMethodBehaviour :: XmlRpcType a => String -> String -> Behaviour [EData a]
xmlrpcMethodBehaviour service method b = consumeEventGroupWith b (service ++ "/" ++ method) $ \evt -> do
    let parms = map edata2value . eventdata $ evt
        exceptionEvent errmsg = produce "Exception" (service ++ "/" ++ method) "XmlRpcException" once [EString errmsg]
        responseEvent val = produce "XmlRpcResponse" service method once [value2edata val]
    r <- runErrorT $ call service method parms
    case r of
        Left err -> listM $ exceptionEvent err
        Right res -> listM $ responseEvent res

xmlrpcServiceBehaviour :: XmlRpcType a => String -> Behaviour [EData a]
xmlrpcServiceBehaviour service b = consumeEventGroupWith b service $ \evt -> do
    let parms = map edata2value . tail . eventdata $ evt
        EString method = head . eventdata $ evt
        exceptionEvent errmsg = produce "Exception" (service ++ "/" ++ method) "XmlRpcException" once [EString errmsg]
        responseEvent val = produce "XmlRpcResponse" service method once [value2edata val]
    r <- runErrorT $ call service method parms
    case r of
        Left err -> listM $ exceptionEvent err
        Right res -> listM $ responseEvent res

xmlrpcMethodBehaviourNX :: String -> String -> Behaviour [EData a]
xmlrpcMethodBehaviourNX service method b = consumeEventGroupWith b (service ++ "/" ++ method) $ \evt -> do
    let parms = map edata2valueNX . eventdata $ evt
        exceptionEvent errmsg = produce "Exception" (service ++ "/" ++ method) "XmlRpcException" once [EString errmsg]
        responseEvent val = produce "XmlRpcResponse" service method once [value2edata val]
    r <- runErrorT $ call service method parms
    case r of
        Left err -> listM $ exceptionEvent err
        Right res -> listM $ responseEvent res

xmlrpcServiceBehaviourNX :: String -> Behaviour [EData a]
xmlrpcServiceBehaviourNX service b = consumeEventGroupWith b service $ \evt -> do
    let parms = map edata2valueNX . tail . eventdata $ evt
        EString method = head . eventdata $ evt
        exceptionEvent errmsg = produce "Exception" (service ++ "/" ++ method) "XmlRpcException" once [EString errmsg]
        responseEvent val = produce "XmlRpcResponse" service method once [value2edata val]
    r <- runErrorT $ call service method parms
    case r of
        Left err -> listM $ exceptionEvent err
        Right res -> listM $ responseEvent res
