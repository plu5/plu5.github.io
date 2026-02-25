---
layout: post
title: 8 — Portable hello worlds
date: 2026-02-24 13:23
modified_date: 2026-02-24 20:06
categories: c emscripten wasm win98 godot
lang: en
redirect_from: /devlog/8
---

## Hello world C: Linux, Windows and Web
Let's build this C hello world program for 3 targets:
```c
#include <stdio.h>

int main() {
  printf("bonjour le monde !\n");
  return 0;
}
```
using a Linux machine.

### Linux
To build for Linux it's easy:
```sh
$ gcc main.c -o test
$ ./test
bonjour le monde !
```

### Web
Install `emscripten` (over 1 GB)

```sh
$ source /etc/profile.d/emscripten.sh
$ emcc -v
emcc (Emscripten gcc/clang-like replacement + linker emulating GNU ld) 5.0.1-git (8c5f43157a3f069ade75876e23061330521eabde)
clang version 23.0.0git (/startdir/llvm-project b447f5d9763010f8c6806c578533291aef2bd484)
Target: wasm32-unknown-emscripten
Thread model: posix
InstalledDir: /opt/emscripten-llvm/bin
```

First time building takes a while and tonnes of output
```sh
$ emcc main.c
shared:INFO: (Emscripten: Running sanity checks)
cache:INFO: generating system headers: sysroot_install.stamp... (this will be cached in "/home/pm/.cache/emscripten/sysroot_install.stamp" for subsequent builds)
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libGL-getprocaddr.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libGL-getprocaddr.a" for subsequent builds)
system_libs:INFO: compiled 4 inputs in 0.64s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libal.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libal.a" for subsequent builds)
system_libs:INFO: compiled 1 inputs in 0.36s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libhtml5.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libhtml5.a" for subsequent builds)
system_libs:INFO: compiled 5 inputs in 0.52s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libstubs-debug.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libstubs-debug.a" for subsequent builds)
system_libs:INFO: compiled 2 inputs in 0.42s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libnoexit.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libnoexit.a" for subsequent builds)
system_libs:INFO: compiled 1 inputs in 0.26s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libc-debug.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libc-debug.a" for subsequent builds)
system_libs:INFO: compiled 1047 inputs in 10.73s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libdlmalloc-debug.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libdlmalloc-debug.a" for subsequent builds)
system_libs:INFO: compiled 2 inputs in 0.80s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libcompiler_rt.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libcompiler_rt.a" for subsequent builds)
system_libs:INFO: compiled 184 inputs in 3.09s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libc++-debug-noexcept.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libc++-debug-noexcept.a" for subsequent builds)
system_libs:INFO: compiled 57 inputs in 25.28s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libc++abi-debug-noexcept.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libc++abi-debug-noexcept.a" for subsequent builds)
system_libs:INFO: compiled 17 inputs in 5.46s
cache:INFO:  - ok
cache:INFO: generating system library: sysroot/lib/wasm32-emscripten/libsockets.a... (this will be cached in "/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten/libsockets.a" for subsequent builds)
system_libs:INFO: compiled 27 inputs in 2.59s
cache:INFO:  - ok
$ ls
a.out.js  a.out.wasm  main.c
```

```sh
$ node a.out.js
bonjour le monde !
```

to run in the browser instead, we need to ask it to generate an html file:
```sh
$ emcc main.c -o test.html
```
it created the files:
```sh
test.html
test.js
test.wasm
```
which we will need to serve on a little web server. Inside the project folder:
```sh
$ python -m http.server
```
[http://127.0.0.1:8000/test.html](http://127.0.0.1:8000/test.html)

but it puts on the page more crap than just our program.

[Emscripten getting started tutorial with more information](https://emscripten.org/docs/getting_started/Tutorial.html).

More globally, [MDN WebAssembly Concepts](https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Concepts) ([fr](https://developer.mozilla.org/fr/docs/WebAssembly/Guides/Concepts))

### Windows
For Windows need mingw (arch package `mingw-w64-gcc`) (which is pretty huge), then:
```sh
# (32 bit. for 64 use x86_64-w64-mingw32)
$ i686-w64-mingw32-gcc main.c -o test.exe
$ wine test.exe
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
014c:fixme:file:NtLockFile I/O completion on lock not implemented yet
014c:fixme:ntdll:NtQuerySystemInformation info_class SYSTEM_PERFORMANCE_INFORMATION
0154:fixme:file:NtLockFile I/O completion on lock not implemented yet
0154:fixme:ntdll:NtQuerySystemInformation info_class SYSTEM_PERFORMANCE_INFORMATION
regsvr32: Successfully unregistered DLL 'C:\windows\\Microsoft.NET\Framework\v4.0.30319\diasymreader.dll'
regsvr32: Successfully unregistered DLL 'C:\windows\\Microsoft.NET\Framework64\v4.0.30319\diasymreader.dll'
014c:fixme:msi:internal_ui_handler internal UI not implemented for message 0x0b000000 (UI level = 1)
014c:fixme:msi:internal_ui_handler internal UI not implemented for message 0x0b000000 (UI level = 1)
bonjour le monde !
```

Subsequent runs have a bit less garbage output:
```sh
$ wine test.exe
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
00ac:fixme:wineusb:query_id Unhandled ID query type 0x5.
bonjour le monde !
```

Check DLLs it depends on:
```sh
$ objdump -p test.exe | grep "DLL Name"
	DLL Name: KERNEL32.dll
	DLL Name: api-ms-win-crt-environment-l1-1-0.dll
	DLL Name: api-ms-win-crt-heap-l1-1-0.dll
	DLL Name: api-ms-win-crt-locale-l1-1-0.dll
	DLL Name: api-ms-win-crt-math-l1-1-0.dll
	DLL Name: api-ms-win-crt-private-l1-1-0.dll
	DLL Name: api-ms-win-crt-runtime-l1-1-0.dll
	DLL Name: api-ms-win-crt-stdio-l1-1-0.dll
	DLL Name: api-ms-win-crt-string-l1-1-0.dll
```

### Windows 98
Next I would like to get the hello world working on Windows 98, for which we need to get rid of these `api-ms-win-crt-*` dependencies. I don't know if this is possible with modern mingw or if I will need to install an older one or Open Watcom.

In [this blog post by Fanael](https://fanael.github.io/stockfish-on-windows-98.html) and [related discussion on GitHub](https://github.com/Fanael/fanael.github.io/issues/1), she claims that a hello world
compiled with what at the time was the latest mingw-w64 gcc (2020, "shortly after the release of version 8", with `-march=i686`) works with no changes.

Searching for crt on mingw-w64.org, I found [v12.0.0: 2024-05-29](https://www.mingw-w64.org/changelog/#v1200-2024-05-29) changelog had:

> Important: UCRT is now the default CRT runtime instead of MSVCRT, check the mingw-w64-doc/howto-build/ucrt-vs-msvcrt.txt document for details. Both header set and CRT must be configured and built with the same settings consistently for proper functionality. Switching runtimes requires all libraries to be rebuilt, including GCC.

i.e. the entire toolchain needs to be recompiled if you want MSVCRT?

and there is no other mingw on pacman. but maybe on aur?

[mingw-w64-crt-msvcrt](https://aur.archlinux.org/packages/mingw-w64-crt-msvcrt)

It took 15 minutes on a Thinkpad T480 i5-8350U.

Only `mingw-w64-crt` was replaced.

```sh
$ i686-w64-mingw32-gcc -### main.c 
Using built-in specs.
COLLECT_GCC=i686-w64-mingw32-gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/i686-w64-mingw32/15.2.0/lto-wrapper
Target: i686-w64-mingw32
Configured with: /build/mingw-w64-gcc/src/gcc/configure --prefix=/usr --libexecdir=/usr/lib --target=i686-w64-mingw32 --with-bugurl=https://bugs.archlinux.org/ --enable-languages=ada,c,c++,fortran,lto,objc,obj-c++ --enable-shared --enable-static --enable-threads=posix --enable-fully-dynamic-string --enable-libstdcxx-time=yes --enable-libstdcxx-filesystem-ts=yes --with-system-zlib --enable-cloog-backend=isl --enable-lto --enable-libgomp --disable-multilib --enable-checking=release --disable-sjlj-exceptions --with-dwarf2
Thread model: posix
Supported LTO compression algorithms: zlib zstd
gcc version 15.2.0 (GCC) 
COLLECT_GCC_OPTIONS='-mtune=generic' '-march=pentiumpro' '-dumpdir' 'a-'
 /usr/lib/gcc/i686-w64-mingw32/15.2.0/cc1 -quiet -D_REENTRANT main.c -quiet -dumpdir a- -dumpbase main.c -dumpbase-ext .c "-mtune=generic" "-march=pentiumpro" -o /tmp/ccfosANn.s
COLLECT_GCC_OPTIONS='-mtune=generic' '-march=pentiumpro' '-dumpdir' 'a-'
 /usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/bin/as -o /tmp/ccYjO2C0.o /tmp/ccfosANn.s
COMPILER_PATH=/usr/lib/gcc/i686-w64-mingw32/15.2.0/:/usr/lib/gcc/i686-w64-mingw32/15.2.0/:/usr/lib/gcc/i686-w64-mingw32/:/usr/lib/gcc/i686-w64-mingw32/15.2.0/:/usr/lib/gcc/i686-w64-mingw32/:/usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/bin/
LIBRARY_PATH=/usr/lib/gcc/i686-w64-mingw32/15.2.0/:/usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/lib/../lib/:/usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/lib/
COLLECT_GCC_OPTIONS='-mtune=generic' '-march=pentiumpro' '-dumpdir' 'a.'
 /usr/lib/gcc/i686-w64-mingw32/15.2.0/collect2 -plugin /usr/lib/gcc/i686-w64-mingw32/15.2.0/liblto_plugin.so "-plugin-opt=/usr/lib/gcc/i686-w64-mingw32/15.2.0/lto-wrapper" "-plugin-opt=-fresolution=/tmp/ccdYxquS.res" "-plugin-opt=-pass-through=-lmingw32" "-plugin-opt=-pass-through=-lgcc" "-plugin-opt=-pass-through=-lgcc_eh" "-plugin-opt=-pass-through=-lmingwex" "-plugin-opt=-pass-through=-lmsvcrt" "-plugin-opt=-pass-through=-lkernel32" "-plugin-opt=-pass-through=-lpthread" "-plugin-opt=-pass-through=-ladvapi32" "-plugin-opt=-pass-through=-lshell32" "-plugin-opt=-pass-through=-luser32" "-plugin-opt=-pass-through=-lkernel32" "-plugin-opt=-pass-through=-lmingw32" "-plugin-opt=-pass-through=-lgcc" "-plugin-opt=-pass-through=-lgcc_eh" "-plugin-opt=-pass-through=-lmingwex" "-plugin-opt=-pass-through=-lmsvcrt" "-plugin-opt=-pass-through=-lkernel32" -m i386pe -Bdynamic /usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/lib/../lib/crt2.o /usr/lib/gcc/i686-w64-mingw32/15.2.0/crtbegin.o -L/usr/lib/gcc/i686-w64-mingw32/15.2.0 -L/usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/lib/../lib -L/usr/lib/gcc/i686-w64-mingw32/15.2.0/../../../../i686-w64-mingw32/lib /tmp/ccYjO2C0.o -lmingw32 -lgcc -lgcc_eh -lmingwex -lmsvcrt -lkernel32 -lpthread -ladvapi32 -lshell32 -luser32 -lkernel32 -lmingw32 -lgcc -lgcc_eh -lmingwex -lmsvcrt -lkernel32 /usr/lib/gcc/i686-w64-mingw32/15.2.0/crtend.o
COLLECT_GCC_OPTIONS='-mtune=generic' '-march=pentiumpro' '-dumpdir' 'a.'
```
Was the gcc executable changed or not?
```sh
$ which i686-w64-mingw32-gcc
/usr/bin/i686-w64-mingw32-gcc
$ file /usr/bin/i686-w64-mingw32-gcc
/usr/bin/i686-w64-mingw32-gcc: ELF 64-bit LSB executable, x86-64, version 1 (GNU/Linux), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, BuildID[sha1]=b070ffc332ba6551c45e06d7e47588d6338b555c, for GNU/Linux 4.4.0, stripped
$ stat /usr/bin/i686-w64-mingw32-gcc
  File: /usr/bin/i686-w64-mingw32-gcc
  Size: 2173600   	Blocks: 4248       IO Block: 4096   regular file
Device: 259,2	Inode: 4612743     Links: 2
Access: (0755/-rwxr-xr-x)  Uid: (    0/    root)   Gid: (    0/    root)
Access: 2026-02-24 15:59:12.543675534 +0200
Modify: 2025-09-01 17:56:17.000000000 +0300
Change: 2026-02-23 05:11:05.591880135 +0200
 Birth: 2026-02-23 05:11:05.589320791 +0200
```
it seems like not.

but nevertheless it works
```sh
$ i686-w64-mingw32-gcc main.c -o test.exe
$ objdump -p test.exe | grep "DLL Name"
	DLL Name: KERNEL32.dll
	DLL Name: msvcrt.dll
```
Windows 1998 MS-DOS Prompt:
```sh
Microsoft(R) Windows 98
   (C)Copyright Microsoft Corp 1981-1999.

C:\>D:

D:\>test.exe
bonjour le monde !
```

## Hello world Godot (4.3 stable)
Create a new project. There are 3 renderer options:

(1) Forward+ (selectionné par défaut)
- Supports desktop platforms only.
- Advanced 3D graphics available.
- Can scale to large complex scenes.
- Uses RenderingDevice backend.
- Slower rendering of simple scenes.

(2) Mobile
- Supports desktop + mobile platforms.
- Less advanced 3D graphics.
- Less scalable for complex scenes.
- Uses RenderingDevice backend.
- Fast rendering of simple scenes.

(3) Compatibility
- Supports desktop, mobile + web platforms.
- Least advanced 3D graphics (currently work-in-progress).
- Intended for low-end/older devices.
- Uses OpenGL 3 backend (OpenGL 3.3/ES 3.0/WebGL2).
- Fastest rendering of simple scenes.

choose Compatibility.

Under Scene on the left sidebar press the + icon to create a node, type Label, double click Label.

Under Inspector on the right sidebar type some text under Text.

I saved the scene under the name `1.tscn`. Now the project folder on disk has:
```sh
.gitattributes
.gitignore
icon.svg
icon.svg.import
.godot
1.tscn
project.godot
```

Menu → Project → Export... → Add... → Web

Press button Export Project...

and export to `test.html`

It now created the following files in the project folder:
```sh
export_presets.cfg
test.apple-touch-icon.png
test.apple-touch-icon.png.import
test.audio.worklet.js
test.html
test.icon.png
test.icon.png.import
test.js
test.pck
test.png
test.png.import
test.wasm
```

to run this we need a little web server. Inside the project folder:
```sh
$ python -m http.server
```

and we can now access this on the browser [http://127.0.0.1:8000/test.html](http://127.0.0.1:8000/test.html) and see the label we created, hurrah.

{% include fin.html %}
