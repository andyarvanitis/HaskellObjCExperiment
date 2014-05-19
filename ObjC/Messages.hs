
{-# LANGUAGE FlexibleInstances #-}

module ObjC.Messages where

import Foreign.Ptr
import Foreign.C
import ObjC.Runtime

---------------------------------------------------------------------------------------------------
data Message = Message { 
                  selectorName  :: String, 
                  arguments     :: [Argument]
               }

---------------------------------------------------------------------------------------------------
makeMessage :: MessageArgument a => String -> a -> Message
---------------------------------------------------------------------------------------------------
makeMessage s a =  Message (s ++ ":") (asPlainArg a)

---------------------------------------------------------------------------------------------------
makeMessageAndBox :: MessageArgument a => String -> a -> Message
---------------------------------------------------------------------------------------------------
makeMessageAndBox s a = Message (s ++ ":") (asBoxedArg a)

---------------------------------------------------------------------------------------------------
concatMessages :: Message -> Message -> Message
---------------------------------------------------------------------------------------------------
concatMessages (Message s1 a1) (Message s2 a2) = Message (s1 ++ s2) (a1 ++ a2)

---------------------------------------------------------------------------------------------------
class Msg a where
   getSel :: a -> IO Pointer
   getArgs :: a -> [Argument]
   
---------------------------------------------------------------------------------------------------
instance Msg Message where
   getSel a = getSelector(selectorName a)
   getArgs a = arguments a

instance Msg String where
   getSel a = getSelector(a)
   getArgs _ = []

---------------------------------------------------------------------------------------------------
class MessageReceiver a where
---------------------------------------------------------------------------------------------------
   receiveMessage :: (Msg b, MessageResult c) => a -> b -> IO c

---------------------------------------------------------------------------------------------------
class MessageArgument a where
   asPlainArg :: a -> [Argument]
   asBoxedArg :: a -> [Argument]

---------------------------------------------------------------------------------------------------
class MessageResult a where
   getMessageResult :: Pointer -> Pointer -> [Argument] -> IO a

---------------------------------------------------------------------------------------------------
sendMessageToClassName :: (Msg a, MessageResult c) => a -> String -> IO c
---------------------------------------------------------------------------------------------------
sendMessageToClassName a b = do
   objPtr <- getClass b
   selPtr <- getSel a
   getMessageResult objPtr selPtr args
      where args = getArgs a

---------------------------------------------------------------------------------------------------
sendMessageToPointer :: (Msg a, MessageResult c) => a -> Pointer -> IO c
---------------------------------------------------------------------------------------------------
sendMessageToPointer a b = do
   selPtr <- getSel a
   getMessageResult b selPtr args
      where args = getArgs a

