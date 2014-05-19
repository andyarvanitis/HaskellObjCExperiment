
module ObjC.Boxing where

import ObjC.Runtime

---------------------------------------------------------------------------------------------------
newtype Unboxed a = Unboxed { value :: a }
   deriving (Eq, Show)

---------------------------------------------------------------------------------------------------
boxer :: String -> String -> Arg -> IO Pointer
---------------------------------------------------------------------------------------------------
boxer a b c = do
   cls <- getClass a
   sel <- getSelector (b ++ ":")
   msgSend (retPtr retVoid) cls sel [(c, Nothing)]

---------------------------------------------------------------------------------------------------
unboxer :: Pointer -> String -> RetType c -> IO c
---------------------------------------------------------------------------------------------------
unboxer a b c = do
   sel <- getSelector b
   msgSend c a sel []
