module Main where

import ObjC
import ObjC.NSFunctions
import ObjC.EnumBlocks

import Foreign
import Foreign.C
import Data.Typeable

import Foreign.LibFFI
import System.Posix.DynamicLinker


main :: IO () 
main = do

    let stringClass = Class "NSString"

    fmt <- "NSString" <<|"stringWithUTF8String"|:"Salutation is: %@" :: IO Object

    fmt <- fmt <<| "release"
    
    str <- "stringWithUTF8String"|:"hello world"|>> "NSString" :: IO Object
    
    nsLog fmt str
       
    str <- "stringByAppendingString"|:@"!"|>> str :: IO Object

    nsLog fmt str
    
    -- s <- unbox str :: IO String
    
    
    len <- "length"|>> str :: IO Int
    
    putStrLn $ "Length: " ++ show len


    -- box "Testing" >>= nsLog fmt

    let charset = 100::Int

    let msg = "stringWithUTF8String"|:"Salutation is: %@"

    result <- msg |>> "NSString" :: IO Object
    
    unboxedInt <- "numberWithInt"|:charset|>> "NSNumber" :: IO (Unboxed Int)    
    putStrLn $ "Unboxed int: " ++ show unboxedInt

    unboxedString <- "stringWithUTF8String"|:"Hello!"|>> "NSString" :: IO (Unboxed String)
    putStrLn $ "Unboxed string: " ++ (value unboxedString)

    s1 <- "stringWithUTF8String"|:"hello"|>> "NSString" :: IO Object
    s2 <- "stringWithUTF8String"|:"world"|>> "NSString" :: IO Object

    array <- "arrayWithObject"|:s1|>> "NSMutableArray" :: IO Object

    "addObject"|:s2|>> array :: IO ()
    
    array <<|"addObject"|:@ "!" :: IO ()
    
    fmt <- "stringWithString"|:@"%@"|>> "NSString" :: IO Object
    
    nsLog fmt array

    "enumerateObjectsUsingBlock"|:asEnumObjFunc (\_ i stop -> print i)|>> array :: IO ()
    
    swiftFoo <- "AXAFoo" <<|"new" :: IO Object
    swiftFoo <<| "print" :: IO ()
    swiftFoo <<| "release" :: IO ()

    return ()
    
    

