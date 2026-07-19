---
layout: post
title: 25 — Raylib C web fps first steps
date: 2026-07-10 05:49
modified_date: 2026-07-19 00:50
categories: game raylib c web emscripten
lang: en
redirect_from: /devlog/25
wip: true
---

## What
The idea of being able to program games in C for the web is appealing. I saw the raylib example [3d camera first person](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_first_person) which works on the web and is impressive, but some of the other examples don't work so well on the web or not at all (sprite stacking, skybox, basic lighting, mesh instancing, deferred rendering, hybrid rendering, vertex displacement, ...)

The fact [the wiki page on building for the web](https://github.com/raysan5/raylib/wiki/Working-for-Web-(HTML5)) is so long is scary, and it almost made me give up (because of the fear that you're not meant to use raylib for that, that it's possible but not well supported), but part of the length is because the parts of each section give alternative ways of doing the same thing.

I decided to stop stressing and just try it and see for myself. I am encouraged by people who do use it for this purpose and seemingly to great effect, like [zet23t](https://github.com/zet23t) ([itch](https://zet23t.itch.io/)).

## Basic non-web example
Let's start by just making a game the normal way, not for the web. The [docs](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux) suggest to use [raylib-quickstart](https://github.com/raylib-extras/raylib-quickstart) which requires us to run opaque binaries they put on the repository, and I'm massively put off. And even the docs for Linux are really long and complicated.

There is also [raylib-game-template](https://github.com/raysan5/raylib-game-template) which seems to be what zet23t uses. It also has minshell.html included, which we'll need later for the web build. It has some typos and nonsensical English ("Chose one of the follow setup options that fit in you development environment." and then there's not even options after)

I don't mean to be critical, I'm just confused.

I'm not on Windows, but the Windows instructions clarify a bit that you need to have this structure:
```text
some parent directory/
├── raylib/
└── template/
```
So let's try this:
```sh
mkdir cwebfps
cd cwebfps
git clone --depth 1 --branch 6.0 https://github.com/raysan5/raylib
git clone https://github.com/raysan5/raylib-game-template cwebfps --depth 1
make -C raylib/src
make -C cwebfps/src
cwebfps/src/raylib_game
```

I haven't actually installed any of the dependencies yet, but `make -C raylib/src` succeeded. `make -C cwebfps/src` failed with:
```sh
raylib_game.c:13:10: fatal error: raylib.h: No such file or directory
   13 | #include "raylib.h"
      |          ^~~~~~~~~~
compilation terminated.
```
That header is inside raylib/src.

The Makefile seems to define the path to the library as `C:\raylib\raylib`

Rather than messing around modifying that, let's try first:
```sh
make -C cwebfps/src CFLAGS+=' -I../../raylib/src'
```
```sh
gcc -c raylib_game.c -o raylib_game.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -c screen_logo.c -o screen_logo.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -c screen_title.c -o screen_title.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -c screen_options.c -o screen_options.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -c screen_gameplay.c -o screen_gameplay.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -c screen_ending.c -o screen_ending.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -DPLATFORM_DESKTOP
gcc -o ./raylib_game  raylib_game.o  screen_logo.o  screen_title.o  screen_options.o  screen_gameplay.o  screen_ending.o -I../../raylib/src -I. -Iexternal -IC:\raylib\raylib/src -L. -LC:\raylib\raylib/src -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -DPLATFORM_DESKTOP
/usr/bin/ld: cannot find -lraylib: No such file or directory
```
:-|

Changing `RAYLIB_PATH` to `../../raylib` in the makefile

(You can also install raylib globally, but why have we cloned it then?)

This succeeded, and I can indeed launch the game (`cwebfps/src/raylib_game`), hurrah

and it looks good, not like anything is missing, but the output suggests two resources failed to load:
```sh
WARNING: FILEIO: [resources/mecha.png] Failed to open file
INFO: FONT: Data loaded successfully (10 pixel size | 224 glyphs)
WARNING: FILEIO: [resources/coin.wav] Failed to open file
```
Is it because I'm not in the correct folder? Inside the inner cwebfps folder it's the same. Inside cwebfps/src, they do load seemingly:
```sh
INFO: FILEIO: [resources/mecha.png] File loaded successfully
INFO: IMAGE: Data loaded successfully (128x128 | R8G8B8A8 | 1 mipmaps)
INFO: TEXTURE: [ID 3] Texture loaded successfully (128x128 | R8G8B8A8 | 1 mipmaps)
INFO: FONT: Data loaded successfully (16 pixel size | 96 glyphs)
INFO: FILEIO: [resources/coin.wav] File loaded successfully
INFO: WAVE: Data loaded successfully (22050 Hz, 16 bit, 1 channels)
```
Although I don't see or hear anything different

mecha.png is apparently just a font. and coin.wav is a get coin sound which--

ah, I do hear it. I was closing the game straight away but if you keep it open there is another screen where this sound plays when you click, and it goes between "title screen" and "end screen".

Ran from the top folder everything looks the same, I still see the text and all despite "mecha" apparently failing to load, the only difference is there isn't the coin sound.

## Build the template for web
It looks already set up for the web, from all the references to `PLATFORM_WEB` both in the C source code and makefile.

At the beginning of the makefile there is:
```make
# Define target platform: PLATFORM_DESKTOP, PLATFORM_WEB, PLATFORM_DRM, PLATFORM_ANDROID
PLATFORM              ?= PLATFORM_DESKTOP
```
So do we need to just set this to web and make again? Probably not, because according to [the wiki](https://github.com/raysan5/raylib/wiki/Working-for-Web-(HTML5)), we need to recompile the library for html5 with emscripten.

In [devlog 8](/devlog/8) I already installed emscripten, though I'm not sure if it's the same thing as "emscripten toolchain" the wiki talks about. In the wiki they just use emcc and emar to rebuild the library.
```sh
$ which emcc
/usr/lib/emscripten/emcc
$ which emar
/usr/lib/emscripten/emar
```

Attempt:
```sh
cd raylib/src
emcc -c rcore.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
emcc -c rshapes.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
emcc -c rtextures.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
emcc -c rtext.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
emcc -c rmodels.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
emcc -c raudio.c -Os -Wall -DPLATFORM_WEB
emar rcs libraylib.a rcore.o rshapes.o rtextures.o rtext.o rmodels.o raudio.o
```
Doesn't take too long

The output of ls hasn't changed, same list of files in raylib/src

Now let's try to modify the platform in the cwebfps makefile to `PLATFORM_WEB` [and I also changed `PROJECT_NAME` from `raylib_game` to `cwebfps` while I'm at it], and rebuild with `make -C cwebfps/src`.
```sh
emcc: error: ../../raylib/src/libraylib.web.a: No such file or directory ("../../raylib/src/libraylib.web.a" was expected to be an input file, based on the commandline arguments provided)
```
Let me try to rename it.
```sh
mv raylib/src/libraylib.a raylib/src/libraylib.web.a
```

This was the output of ls in cwebfps/src before remaking:
```sh
CMakeLists.txt    raylib_game.rc     screen_logo.c
Info.plist        raylib.icns        screen_logo.o
Makefile          raylib.ico         screen_options.c
Makefile.Android  resources          screen_options.o
minshell.html     screen_ending.c    screens.h
raylib_game       screen_ending.o    screen_title.c
raylib_game.c     screen_gameplay.c  screen_title.o
raylib_game.o     screen_gameplay.o
```

Make errors with:
```sh
wasm-ld: error: unknown file type: raylib_game.o
wasm-ld: error: unknown file type: screen_logo.o
wasm-ld: error: unknown file type: screen_title.o
wasm-ld: error: unknown file type: screen_options.o
wasm-ld: error: unknown file type: screen_gameplay.o
wasm-ld: error: unknown file type: screen_ending.o
wasm-ld: warning: ../../raylib/src/libraylib.web.a: archive member 'rglfw.o' is neither Wasm object file nor LLVM bitcode
emcc: error: '/opt/emscripten-llvm/bin/wasm-ld -o ./cwebfps.wasm /tmp/tmpcilnja3mlibemscripten_js_symbols.so -Bstatic --strip-debug --export=_emscripten_stack_alloc --export=__wasm_call_ctors --export=emscripten_stack_get_current --export=_emscripten_stack_restore --export-if-defined=__start_em_asm --export-if-defined=__stop_em_asm --export-if-defined=__start_em_lib_deps --export-if-defined=__stop_em_lib_deps --export-if-defined=__start_em_js --export-if-defined=__stop_em_js --export-if-defined=main --export-if-defined=__main_argc_argv --export-table -z stack-size=1048576 --no-growable-memory --initial-memory=134217728 --no-entry --no-stack-first --table-base=1 --global-base=1024 raylib_game.o screen_logo.o screen_title.o screen_options.o screen_gameplay.o screen_ending.o -L. -L../../raylib/src ../../raylib/src/libraylib.web.a -L/home/pm/.cache/emscripten/sysroot/lib/wasm32-emscripten -L/usr/lib/emscripten/src/lib -lGL-getprocaddr -lal -lhtml5 -lstubs -lnoexit -lc -ldlmalloc -lcompiler_rt -lc++-noexcept -lc++abi-noexcept -lsockets -mllvm -combiner-global-alias-analysis=false -mllvm -enable-emscripten-sjlj -mllvm -disable-lsr' failed (returned 1)
```

I think we maybe need to clean before building for different platforms
```sh
make -C cwebfps/src clean
```
It says "Cleaning done", but the o files are still there.
```sh
rm cwebfps/src/*.o
```

Now making succeeds, and this is the output of ls in cwebfps/src:
```sh
CMakeLists.txt    raylib_game      screen_gameplay.c
cwebfps.data      raylib_game.c    screen_gameplay.o
cwebfps.html      raylib_game.o    screen_logo.c
cwebfps.js        raylib_game.rc   screen_logo.o
cwebfps.wasm      raylib.icns      screen_options.c
Info.plist        raylib.ico       screen_options.o
Makefile          resources        screens.h
Makefile.Android  screen_ending.c  screen_title.c
minshell.html     screen_ending.o  screen_title.o
```
New files cwebfps.data, cwebfps.html, cwebfps.js, cwebfps.wasm

```sh
cd cwebfps/src
python -m http.server
```

[http://127.0.0.1:8000/cwebfps.html]

It is working, including the coin sound. You can also test this on other devices in your local network by goign to localip:8000/cwebfps.html, where localip is the local ip of the device you're hosting on (`ip address` on Arch, and on other operating systems `ifconfig` or `ipconfig`).

The game doesn't load on my phone, but I'm on iOS 13.5.1, so hardly any website loads anymore.

## Simplify
There is a number of things I don't like about the template.
- It's bloated and feels like a mess before we even start working on our game, which will necessarily turn it into even more of a mess.
- The code style is nonstandard for C, it looks like a C# or Java programmer using C. There is also a CONVENTIONS.md included in the template to enforce this code style.
- Giant fuckoff copyright banners on the top of every file which you have to scroll past every time and are not allowed to remove. Not engine files, your own game's main files which you will be working on for hundreds to thousands of hours, and presumably, if you want to be proper, add another banner above it to indicate your own copyright/licence. I wish he had used SPDX.

When we upload it we need the html file to be called index. It's `PROJECT_NAME` in the makefile that determines what it's called, so I will change that to index.

Next in the top level folder I will create a subfolder called ref, and move everything in there.
```sh
mkdir ref
mv cwebfps/* ref/
```

There is also a .github folder which wasn't moved out, and a .gitignore. I will leave the gitignore but move out github.
```sh
mv cwebfps/.github ref/
```

I don't think an src subfolder is really necessary, so I will just move the makefile to the top level
```sh
mv ref/src/Makefile cwebfps/
```
then modify the source files to only one file which we will create
```make
PROJECT_SOURCE_FILES  ?= \
    game.c
```

There is a giant notice too on the top of the makefile which we are not allowed to remove, but also not allowed to misrepresent as being a product of the original author if it was altered, so I guess add a comment at the top indicating that it was altered?
```make
# Altered from raysan5/raylib-game-template
```

And I just noticed this:
```make
ifeq ($(PLATFORM),PLATFORM_WEB)
    # Emscripten required variables
    EMSDK_PATH         ?= C:/raylib/emsdk
    EMSCRIPTEN_PATH    ?= $(EMSDK_PATH)/upstream/emscripten
    CLANG_PATH          = $(EMSDK_PATH)/upstream/bin
    PYTHON_PATH         = $(EMSDK_PATH)/python/3.13.3_64bit
    NODE_PATH           = $(EMSDK_PATH)/node/22.16.0_64bit/bin
    # NOTE: Using := instead of = so Make expands it immediately and avoids 
    # creating a recursively-expanded variable that references itself
    export PATH        := $(EMSDK_PATH);$(EMSCRIPTEN_PATH);$(CLANG_PATH);$(NODE_PATH);$(PYTHON_PATH);$(PATH)
endif
```
These paths are obviously not correct on my system, but the build still worked earlier. It seems like this is just to add these things to the system PATH, which for most people they already will be. I'll remove this entire section.

And since we're no longer in src, `RAYLIB_PATH` should be changed from `../../raylib` to `../raylib`

For the web build we also need minshell.html
```sh
mv ref/src/minshell.html cwebfps/
```

And a folder called resources (because the makefile puts `--preload-file resources` in the build command)
```sh
mkdir cwebfps/resources
```

Now let's make game.c.
```c
#include "raylib.h"

#if defined(PLATFORM_WEB)
    #include <emscripten/emscripten.h>
#endif

static void draw_frame(void);

int main(void) {
    InitWindow(800, 450, "cwebfps");

#if defined(PLATFORM_WEB)
    emscripten_set_main_loop(draw_frame, 60, 1);
#else
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        draw_frame();
    }
#endif

    CloseWindow();
    return 0;
}

static void draw_frame(void) {
    BeginDrawing();
    ClearBackground(DARKGREEN);
    EndDrawing();
}
```
The colours (like RAYWHITE, DARKGREEN) are defined in raylib.h. I made it something other than white so that we can check if it works rather than just failed to load.

`make -C cwebfps`

It fails, I think due to resources being empty. So even though I don't need it, let's try to move the font image in there.
```sh
mv ref/src/resources/mecha.png cwebfps/resources/
```
```sh
make -C cwebfps
cd cwebfps
python -m http.server
```

Yes, it's a green rectangle. And now it suffices to go on [http://127.0.0.1:8000/] without indicating the file, because it's in index.html.

cwebfps contents (ls -a):
```sh
.       game.o      index.data  index.wasm     resources
..      .git        index.html  Makefile
game.c  .gitignore  index.js    minshell.html
```

I'm going to remove the .git, better to create our own repository with clean history
```sh
rm -r .git
```

## Question: main.c
Why is it that our entrypoint (which I called `game.c`, and in the template is `raylib_game.c`) can be called anything, rather than main.c?

TODO

## Question: Repository location
In the template they put the git repository in the inner folder rather than the top folder where we also have the raylib folder. Would it not be better to put the repository higher up and make raylib a submodule, so that you can more easily clone the project in one go without needing to also clone raylib to the right place in the hierarchy? With git submodules you can even point to a particular version to make it the same version of the engine that we're using in case of any backwards incompatible changes in future.

TODO

## Game environment

TODO

{% include fin.html %}
