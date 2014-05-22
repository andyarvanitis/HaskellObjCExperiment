
{-# LANGUAGE FlexibleInstances #-}

module ObjC (Object (Object), 
             Class (Class), 
             nil, 
             BOOL (YES, NO),
             (|:), (|:@), (|..), (|>>), (<<|),
             Unboxed (value)
             ) where

import Foreign.Ptr

import ObjC.Runtime
import ObjC.Messages
import ObjC.ArgumentTypes
import ObjC.ResultTypes
import ObjC.Operators
import ObjC.Boxing

---------------------------------------------------------------------------------------------------
newtype Object = Object Pointer
   deriving (Eq, Show)
   
---------------------------------------------------------------------------------------------------
newtype Class = Class { name :: String } 
   deriving (Eq, Show)

---------------------------------------------------------------------------------------------------
nil :: Object
nil = Object nullPtr

---------------------------------------------------------------------------------------------------
data BOOL = YES | NO 
   deriving (Eq, Show)

---------------------------------------------------------------------------------------------------
-- Objective-C message receivers can be classes (metaclasses) or objects (class instances)
---------------------------------------------------------------------------------------------------
instance MessageReceiver Class where
   receiveMessage cls msg = sendMessageToClassName msg (name cls)

instance MessageReceiver Object where
   receiveMessage (Object ptr) msg = sendMessageToPointer msg ptr

instance MessageReceiver String where
   receiveMessage name msg = sendMessageToClassName msg name

---------------------------------------------------------------------------------------------------
-- Object argument types
---------------------------------------------------------------------------------------------------
instance MessageArgument Object where
   asPlainArg (Object ptr) = [(argPtr ptr, Nothing)]
   asBoxedArg a = [(fst . head $ asPlainArg a, Nothing)]

---------------------------------------------------------------------------------------------------
-- Object result (return) types
---------------------------------------------------------------------------------------------------
instance MessageResult Object where
   getMessageResult obj sel args = do
      result <- msgSend (retPtr retVoid) obj sel args
      return (Object result)

---------------------------------------------------------------------------------------------------
-- BOOL argument types
---------------------------------------------------------------------------------------------------
instance MessageArgument BOOL where
   asPlainArg a = [(argCChar (fromBOOL a), Nothing)]
                  where fromBOOL a
                          | a == YES = 1
                          | a == NO  = 0                  

   asBoxedArg a = [(fst . head $ asPlainArg a, Just $ boxer "NSNumber" "numberWithBool")]

