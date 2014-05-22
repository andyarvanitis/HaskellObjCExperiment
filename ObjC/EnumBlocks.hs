
{-# LANGUAGE FlexibleInstances #-}

module ObjC.EnumBlocks (EnumObjectsFunc, 
                        asEnumObjFunc,
                        setBOOL) where
   
import Foreign
import Foreign.Ptr   
import Foreign.C
   
import ObjC
import ObjC.Blocks
import ObjC.Messages
import ObjC.Runtime

---------------------------------------------------------------------------------------------------
-- Enum block argument types
---------------------------------------------------------------------------------------------------
instance MessageArgument EnumObjectsFunc where
   asPlainArg f = [(argPtr nullPtr, Just (mkEnumObjectsBlockArg f))]
   asBoxedArg f = [(argPtr nullPtr, Just (mkEnumObjectsBlockArg f))]

---------------------------------------------------------------------------------------------------
-- The following function type represents (void (^)(id obj, NSUInteger idx, BOOL *stop))block
-- used for many "enumerateObjectsUsingBlock" methods.
---------------------------------------------------------------------------------------------------
type EnumObjectsInvokeFunc = Pointer -> Pointer -> CLong -> Ptr CChar -> IO ()
type EnumObjectsFunc = Object -> CLong -> Ptr CChar -> IO ()

---------------------------------------------------------------------------------------------------
-- Note that block invoke functions have a first parameter of a pointer to the block itself, so
-- we account for and ignore it by adding nesting another function within it.
---------------------------------------------------------------------------------------------------
foreign import ccall "wrapper" 
   mkEnumObjectsFunPtr :: EnumObjectsInvokeFunc -> IO (FunPtr EnumObjectsInvokeFunc)

---------------------------------------------------------------------------------------------------
mkEnumObjectsInvokeFunc :: EnumObjectsFunc -> EnumObjectsInvokeFunc
mkEnumObjectsInvokeFunc f _ ptr i stop = f (Object ptr) i stop

---------------------------------------------------------------------------------------------------
mkEnumObjectsBlockArg :: EnumObjectsFunc -> Arg -> IO Pointer
---------------------------------------------------------------------------------------------------
mkEnumObjectsBlockArg f _ = do 
   fPtr <- mkEnumObjectsFunPtr (mkEnumObjectsInvokeFunc f) 
   (Block block) <- funPtrToBlock fPtr
   return block

---------------------------------------------------------------------------------------------------
asEnumObjFunc :: EnumObjectsFunc -> EnumObjectsFunc
---------------------------------------------------------------------------------------------------
asEnumObjFunc f = f

---------------------------------------------------------------------------------------------------
setBOOL :: Ptr CChar -> BOOL -> IO ()
---------------------------------------------------------------------------------------------------
setBOOL ptr x 
   | x == YES = poke ptr 1
   | x == NO  = poke ptr 0

---------------------------------------------------------------------------------------------------
setBool :: Ptr CChar -> Bool -> IO ()
---------------------------------------------------------------------------------------------------
setBool ptr x 
   | x == True  = poke ptr 1
   | x == False = poke ptr 0

