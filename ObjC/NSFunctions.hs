
module ObjC.NSFunctions (nsLog) where

import ObjC   
   
---------------------------------------------------------------------------------------------------
foreign import ccall "NSObjCRuntime.h NSLog" 
   nsLog :: Object -> Object -> IO ()


