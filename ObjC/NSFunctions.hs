
module ObjC.NSFunctions (nsLog) where

import ObjC   
   
---------------------------------------------------------------------------------------------------
foreign import ccall unsafe "NSObjCRuntime.h NSLog" 
   nsLog :: Object -> Object -> IO ()


