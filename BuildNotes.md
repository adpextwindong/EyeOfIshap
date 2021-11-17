Cabal build -v3 is really helpful for finding which executable, specifically ld, is failing.

```
ld -lssl --verbose
```

can help you figure out missing libs issues.

For example my stupid winblows setup has chocolatey install cabal with it using "/cygdrive/c/building/msys64/mingw64/usr/lib" where ld looks up. Not your typical /usr/lib

Look at the file lists cygwin ships in [libssl-devel](https://cygwin.com/packages/x86_64/libssl-devel/libssl-devel-1.1.1d-1) and [libssl](https://cygwin.com/packages/x86_64/libssl1.1/libssl1.1-1.1.1d-1).

In devel

```
usr/lib/libcrypto.dll.a
usr/lib/libssl.dll.a

In release

```
usr/bin/cygcrypto-1.1.dll
usr/bin/cygssl-1.1.dll
```
