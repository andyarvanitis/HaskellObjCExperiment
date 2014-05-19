
{-# LANGUAGE FlexibleInstances #-}

module ObjC.ArgumentTypes where

import Foreign.Ptr

import ObjC.Messages
import ObjC.Runtime
import ObjC.Boxing
import ObjC.Blocks

---------------------------------------------------------------------------------------------------
-- Supported argument types
---------------------------------------------------------------------------------------------------

instance MessageArgument () where
   asPlainArg _ = []
   asBoxedArg _ = []

instance MessageArgument String where
   asPlainArg a = [(argString a, Nothing)]
   asBoxedArg a = [(fst . head $ asPlainArg a, Just $ boxer "NSString" "stringWithUTF8String")]

instance MessageArgument Int where
   asPlainArg a = [(argInt a, Nothing)]
   asBoxedArg a = [(fst . head $ asPlainArg a, Just $ boxer "NSNumber" "numberWithInt")]

instance MessageArgument Block where
   asPlainArg (Block fp) = [(argPtr fp, Nothing)]
   asBoxedArg (Block fp) = [(argPtr fp, Nothing)]

