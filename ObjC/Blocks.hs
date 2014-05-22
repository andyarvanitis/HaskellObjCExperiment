module ObjC.Blocks (Block (Block), funPtrToBlock) where

import Data.List
import Data.Word
import Foreign
import Foreign.C

---------------------------------------------------------------------------------------------------
-- Blocks support
-- Adapted from https://ghc.haskell.org/trac/ghc/wiki/BlockObjects/FakingIt
---------------------------------------------------------------------------------------------------
foreign import ccall "& _NSConcreteGlobalBlock" nsConcreteGlobalBlock :: Ptr ()

-- Layout of the block literal (64-bit runtime)
--
-- .quad	__NSConcreteGlobalBlock           # void *isa;
-- .long	1342177280                        # int  flags = 0x50000000;
-- .long	0                                 # int  reserved;
-- .quad	___block_invoke                   # void (*invoke)(void *, ...);
-- .quad	___block_descriptor               # struct Block_descriptor *descriptor;

long, quad :: Int
long = 4  -- long word = 32 bit
quad = 8  -- quad word = 64 bit

isaOffset, flagsOffset, invokeOffset, descriptorOffset, blockLiteralSize :: Int
isaOffset        = 0
flagsOffset      = isaOffset        + quad
invokeOffset     = flagsOffset      + long + long
descriptorOffset = invokeOffset     + quad
blockLiteralSize = descriptorOffset + quad

newtype Block = Block (Ptr ())

funPtrToBlock :: FunPtr a -> IO Block
funPtrToBlock fPtr = do 
   blockPtr <- mallocBytes blockLiteralSize
   poke (blockPtr `plusPtr` isaOffset)        nsConcreteGlobalBlock
   poke (blockPtr `plusPtr` flagsOffset)      (0x50000000 :: Word32)
   poke (blockPtr `plusPtr` invokeOffset)     fPtr
   dPtr <- descriptorPtr
   poke (blockPtr `plusPtr` descriptorOffset) dPtr
   return $ Block blockPtr
       

-- Block descriptor structure shared between all blocks.
--
-- .quad   0                                 # unsigned long int reserved;
-- .quad   32                                # unsigned long int size = blockLiteralSize;
-- .quad   signature_str                     # const char *signature;
-- .quad   0                                 # <undocumented>

descriptorPtr :: IO (Ptr ())
descriptorPtr = do
   descPtr <- mallocBytes (4 * quad)
   poke (descPtr `plusPtr` (0 * quad)) (0 :: Word64)
   poke (descPtr `plusPtr` (1 * quad)) blockLiteralSizeWord64
   poke (descPtr `plusPtr` (2 * quad)) nullPtr    -- gcc puts a NULL in; should be ok for now
   poke (descPtr `plusPtr` (3 * quad)) (0 :: Word64)
   return descPtr

  where
    blockLiteralSizeWord64 :: Word64
    blockLiteralSizeWord64 = fromIntegral blockLiteralSize




