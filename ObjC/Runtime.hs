
module ObjC.Runtime (Pointer,
                     getClass, 
                     getSelector, 
                     msgSend,
                     Argument, Boxer,
                     -- types from libFFI to export
                     Arg, RetType,
                     argPtr, argInt, argString, argFunPtr, argCChar,
                     retPtr, retInt, retString, retVoid
                     ) where

import Data.Maybe
import Foreign.Ptr
import Foreign.C
import Foreign.LibFFI
import System.Posix.DynamicLinker


---------------------------------------------------------------------------------------------------
type Pointer = Ptr ()   

---------------------------------------------------------------------------------------------------
type Boxer = Maybe (Arg -> IO Pointer)

---------------------------------------------------------------------------------------------------
type Argument = (Arg, Boxer)

---------------------------------------------------------------------------------------------------
foreign import ccall "objc/runtime.h objc_getClass" 
   objc_getClass :: CString -> IO Pointer

---------------------------------------------------------------------------------------------------
getClass :: String -> IO Pointer
---------------------------------------------------------------------------------------------------
getClass name = withCString name $ objc_getClass

---------------------------------------------------------------------------------------------------
foreign import ccall "objc/runtime.h sel_registerName" 
   sel_registerName :: CString -> IO Pointer

---------------------------------------------------------------------------------------------------
getSelector :: String -> IO Pointer
---------------------------------------------------------------------------------------------------
getSelector name = withCString name $ sel_registerName

---------------------------------------------------------------------------------------------------
msgSend :: RetType a -> Pointer -> Pointer -> [Argument] -> IO a
---------------------------------------------------------------------------------------------------
msgSend rty obj sel args = do
   f <- dlsym Default "objc_msgSend"
   xs <- boxIfNeeded args
   callFFI f rty $ [argPtr obj, argPtr sel] ++ xs

---------------------------------------------------------------------------------------------------
boxIfNeeded :: [Argument] -> IO [Arg]
---------------------------------------------------------------------------------------------------
boxIfNeeded a = mapM applyBox a

---------------------------------------------------------------------------------------------------
applyBox :: (Arg, Boxer) -> IO Arg
---------------------------------------------------------------------------------------------------
applyBox (a,f) = case f of Nothing -> return a
                           Just f -> do 
                              p <- f a
                              return (argPtr p)

