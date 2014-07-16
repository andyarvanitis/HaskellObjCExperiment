
Just noodling right now...

```
Some random notes:

$ ghc main -framework Foundation     # (without Swift)

$ xcrun -sdk macosx10.9 swift -emit-library -emit-object Foo.swift

$ ghc main Foo.o -framework Foundation -L/Applications/Xcode6-Beta3.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/macosx

$ DYLD_LIBRARY_PATH=/Applications/Xcode6-Beta3.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/macosx main
```

