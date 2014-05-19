
{-# LANGUAGE FlexibleInstances #-}

module ObjC.ResultTypes where

import ObjC.Messages
import ObjC.Runtime
import ObjC.Boxing

---------------------------------------------------------------------------------------------------
-- Supported result (return) types
---------------------------------------------------------------------------------------------------

instance MessageResult () where
   getMessageResult obj sel args = msgSend retVoid obj sel args   

instance MessageResult Int where
   getMessageResult obj sel args = msgSend retInt obj sel args

instance MessageResult (Unboxed Int) where
   getMessageResult obj sel args = do
      boxed <- msgSend (retPtr retVoid) obj sel args
      value <- unboxer boxed "intValue" retInt 
      return (Unboxed value)

instance MessageResult (Unboxed String) where
   getMessageResult obj sel args = do
      boxed <- msgSend (retPtr retVoid) obj sel args
      value <- unboxer boxed "UTF8String" retString 
      return (Unboxed value)

