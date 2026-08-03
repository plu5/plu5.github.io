---
layout: post
title: 25 — Raylib C web fps
date: 2026-07-10 05:49
modified_date: 2026-07-26 15:43
categories: game raylib c emscripten cwebfps
lang: en
redirect_from: /devlog/25
wip: true
---

## What
The idea of being able to program games in C for the web is appealing. I saw the raylib example [3d camera first person](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_first_person) [it was actually [3d camera fps](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_fps) that I meant to link] which works on the web and is impressive, but some of the other examples don't work so well on the web or not at all (sprite stacking, skybox, basic lighting, mesh instancing, deferred rendering, hybrid rendering, vertex displacement, ...)

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

[Even if I cd inside the directory and `make clean`, it doesn't remove the o files. It should be doing `rm -fv *.o` (as well as a few other files), which does work when I run it myself. It's as if it doesn't enter the ifeq]

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

Now let's create game.c.
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

That's just a C misconception on my part; the file can be called anything.

Tangents:
- [SE: Different function entrypoint name](https://stackoverflow.com/questions/7494244/how-to-change-entry-point-of-c-program-with-gcc)
- [SE: Linux entrypoint](https://stackoverflow.com/questions/4113731/is-a-main-required-for-a-c-program)
- [SE: main function signature with differences between C and C++](https://stackoverflow.com/questions/204476/what-should-main-return-in-c-and-c) (in C empty arguments is discouraged in favour of `void`, to avoid recursive calls being able to pass arguments)
- [SE: main work and conventions in hosted and hostless systems, user29079, 2012](https://softwareengineering.stackexchange.com/a/130826)

## Question: Repository location
In the template they put the git repository in the inner folder rather than the top folder where we also have the raylib folder. Would it not be better to put the repository higher up and make raylib a submodule, so that you can more easily clone the project in one go without needing to also clone raylib to the right place in the hierarchy? With git submodules you can even point to a particular version to make it the same version of the engine that we're using in case of any backwards incompatible changes in future.

Create a file .gitmodules at the top-level folder containing:
```conf
[submodule "raylib"]
    path = raylib
    url = https://github.com/raysan5/raylib.git
```

I also made a .gitignore with:
```conf
ref
```

Then:
```sh
$ git init
$ git add *
The following paths are ignored by one of your .gitignore files:
ref
hint: Use -f if you really want to add them.
hint: Disable this message with "git config set advice.addIgnoredFile false"
warning: adding embedded git repository: raylib
hint: You've added another git repository inside your current repository.
hint: Clones of the outer repository will not contain the contents of
hint: the embedded repository and will not know how to obtain it.
hint: If you meant to add a submodule, use:
hint:
hint: 	git submodule add <url> raylib
hint:
hint: If you added this path by mistake, you can remove it from the
hint: index with:
hint:
hint: 	git rm --cached raylib
hint:
hint: See "git help submodule" for more information.
hint: Disable this message with "git config set advice.addEmbeddedRepo false"
```

Not sure why I got that hint, as the raylib repository did not get added, just a file that contains:
```conf
Subproject commit dbc56a87da87d973a9c5baa4e7438a9d20121d28
```
as it should be.

This will make it so that when the repository gets cloned with submodules, it will clone it at the same version as the one we have locally. To update it later we will be able to run `git submodule update --remote`.

If in future we want to modify the engine: fork it on GitHub, push changes to that fork, and change the submodule to point there.

At this point I am going to commit.
```diff
7 files changed, 476 insertions(+)
.gitignore                  |   1 +
.gitmodules                 |   3 +
cwebfps/.gitignore          |  80 ++++++++++
cwebfps/Makefile            | 362 ++++++++++++++++++++++++++++++++++++++++++++
cwebfps/game.c              |  29 ++++
cwebfps/resources/mecha.png | Bin 0 -> 2355 bytes 
raylib                      |   1 +
```
It's so satisfyingly small.

Most of the makefile is probably unnecessary or broken

I'm going to add COPYING and README.md files and will put the repository on gh [plu5/cwebfps](https://github.com/plu5/cwebfps). At this point it is not an FPS yet, just a green screen, but through the rest of the devlog we will add things to it bit by bit and push the changes there.

Test cloning and building:
```sh
$ git clone --recurse-submodules --shallow-submodules https://github.com/plu5/cwebfps.git testclone
$ cd testclone
$ cd raylib/src && emcc -c rcore.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 && emcc -c rshapes.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 && emcc -c rtextures.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 && emcc -c rtext.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 && emcc -c rmodels.c -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 && emcc -c raudio.c -Os -Wall -DPLATFORM_WEB && emar rcs libraylib.a rcore.o rshapes.o rtextures.o rtext.o rmodels.o raudio.o
$ cd ../..
$ make -C cwebfps
[..]
emcc: error: '--shell-file': file not found: 'minshell.html'
```

It's a problem with the .gitignore, it's got `*.html` and `!src/minshell.html`, and since it's no longer in an src subdir it got excluded. I'll just remove the `src/` bit.

Second attempt, after following the same steps:
```sh
$ make -C cwebfps
emcc: error: ../raylib/src/libraylib.web.a: No such file or directory ("../raylib/src/libraylib.web.a" was expected to be an input file, based on the commandline arguments provided)
```

This happened earlier too, and I just renamed it. Have to fix it more properly. Maybe we just need to change this in the makefile:
```make
ifeq ($(PLATFORM),PLATFORM_WEB)
    # Libraries for web (HTML5) compiling
    LDLIBS = $(RAYLIB_LIB_PATH)/libraylib.web.a
endif
```
to `libraylib.a`.

It builds now. and hosted and confirmed that the game runs.

## 3D environment
Looking at [this example](https://www.reddit.com/r/raylib/comments/1dyuz0z/simplest_3d_i_can_come_up_with/) by Jan Zumwalt (/u/jwzumwalt), here is a modified game.c with a rotating red cube in the middle of our green screen:
```c
#include "raylib.h"

#if defined(PLATFORM_WEB)
    #include <emscripten/emscripten.h>
#endif

static Camera camera = {0};

static void draw_frame(void);

int main(void) {
    InitWindow(800, 450, "cwebfps");

    camera.position = (Vector3){0, 10, 10};
    camera.target = (Vector3){0, 0, 0};
    camera.up = (Vector3){0, 1, 0};
    camera.fovy = 90;
    camera.projection = CAMERA_PERSPECTIVE;

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
    UpdateCamera(&camera, CAMERA_ORBITAL);
    BeginDrawing();
    ClearBackground(DARKGREEN);
    BeginMode3D(camera);
    DrawCube((Vector3){0, 0, 0}, 2, 2, 2, RED);
    EndMode3D();
    EndDrawing();
}
```
The difference between whether it's rotating or not is just the line `UpdateCamera(&camera, CAMERA_ORBITAL);`

It's not actually the cube rotating, but rather the camera.

Mouse wheel up and down zooms in and out, even though I didn't set up anything for it. I guess the camera has that by default. It only works if I have the `UpdateCamera` line.

After I make changes, I run:
```sh
make && python -m http.server
```
and refresh the page.

`UpdateCamera(&camera);` doesn't build, it requires a second argument, `mode`. We have seen `CAMERA_ORBITAL`, which is one of them. Others can be seen in raylib.h:
```c
// Camera system modes
typedef enum {
    CAMERA_CUSTOM = 0,              // Camera custom, controlled by user (UpdateCamera() does nothing)
    CAMERA_FREE,                    // Camera free mode
    CAMERA_ORBITAL,                 // Camera orbital, around target, zoom supported
    CAMERA_FIRST_PERSON,            // Camera first person
    CAMERA_THIRD_PERSON             // Camera third person
} CameraMode;
```

With `UpdateCamera(&camera, CAMERA_FIRST_PERSON);` we have like a player controller set up already. The cube is beneath us, and I don't know how to get to it.

We're supposed to put `DisableCursor();` ("Limit cursor to relative movement inside the window") in main, but I don't notice any difference.

[3d camera first person demo](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_first_person) says, above `UpdateCamera`:
```c
// Update camera computes movement internally depending on the camera mode
// Some default standard keyboard/mouse inputs are hardcoded to simplify use
// For advanced camera controls, it's recommended to compute camera movement manually
```

In the demo, space and ctrl move you up and down. In cwebfps, those keys do nothing. I don't see anything setting that up. I thought maybe this:
```c
CameraYaw(&camera, -135*DEG2RAD, true);
CameraPitch(&camera, -45*DEG2RAD, true, true, false);
```
after we first set up the camera, for which we also have to add `#include "rcamera.h"`, but no.

Another problem: in both the demo and cwebfps I can't centre the cursor.

We can copy the demo as is to game.c (or any other example in [raylib.com/examples](https://www.raylib.com/examples))

but it's a black screen
```js
Aborted(Please compile your program with async support in order to use asynchronous operations like emscripten_sleep)
```

It could be because of the loop.

## Fullscreen
I tried to do fullscreen as shown in [this gist](https://gist.github.com/JeffM2501/6e4630a0e34c0c7dddf066f7192e342d) by JeffM2501 (when you press Alt+Enter), thinking that maybe it would help with the cursor issues, but it's not really fullscreen, it just fills the browser window.

If I do just `ToggleFullscreen();`, I get:
```js
Uncaught TypeError: Module.requestFullscreen is not a function
```

`ToggleBorderlessWindowed();` results in the same error

`MaximizeWindow();` does nothing, no output in console either (didn't expect it to work either, it doesn't sound like something that would work on the web)

[2024 reddit post](https://www.reddit.com/r/raylib/comments/1d1j9wo/iswindowfullscreen_doesnt_seem_to_work_on_web/) by bagelpatrol suggests that the fullscreen is an itch.io thing? ("When putting a game on itch for web, there is an option to add a fullscreen toggle.")

> [–]victorevolves  
2 points 2 years ago  
You might be want to checkout EM_JS macro. It allows you to run JavaScript directly inside C++. (Of course make sure that it is only for Emscripten so you might want to use it with #ifdef block

While trying to research this I landed on [this hackernews thread](https://news.ycombinator.com/item?id=40239164) with people all essentially saying turn back.

> For a while I reported issues as I found them, but almost all of them them were closed as “wont fix”. This was frustrating and discouraging, and it was time consuming to write the bug reports, so I just stopped.  
—[https://danielchasehooper.com/posts/shapeup/]

:-(

[https://github.com/danielchasehooper/ShapeUp-public]

[[his raylib issue reports](https://github.com/raysan5/raylib/issues?q=author%3Adanielchasehooper)]

Back to what we were doing:
I see on [issue 1241](https://github.com/raysan5/raylib/issues/1241) we're maybe supposed to do this with an emscripten call.

In `draw_frame`:
```c
    if (IsKeyPressed(KEY_ENTER) &&  /* M-Enter */
        (IsKeyDown(KEY_LEFT_ALT) || IsKeyDown(KEY_RIGHT_ALT))) {
#ifdef PLATFORM_WEB
        emscripten_request_fullscreen("#canvas", 0);
#else
        ToggleFullscreen();
#endif
    }
```
Need to also include `emscripten/html5.h` at the top to have access to `emscripten_request_fullscreen`:
```c
#if defined(PLATFORM_WEB)
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#endif
```

This works.

But even in fullscreen I still have the problem with my cursor. I can't go past the corners of the screen and I can't recentre it. I do call `DisableCursor();` (in `main`).

Ah oups, I call it inside the else:
```c
#if defined(PLATFORM_WEB)
    emscripten_set_main_loop(draw_frame, 60, 1);
#else
    SetTargetFPS(60);
    DisableCursor();
    while (!WindowShouldClose()) {
        draw_frame();
    }
#endif
```
I'll put it before the if.

That sorts it.

Here's game.c so far:
```c
#include "raylib.h"
#include "rcamera.h"

#if defined(PLATFORM_WEB)
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#endif

static Camera camera = {0};
static const int width = 800;
static const int height = 450;

static void draw_frame(void);

int main(void) {
    InitWindow(width, height, "cwebfps");

    camera.position = (Vector3){0, 10, 10};
    camera.target = (Vector3){0, 0, 0};
    camera.up = (Vector3){0, 1, 0};
    camera.fovy = 90;
    camera.projection = CAMERA_PERSPECTIVE;
    CameraYaw(&camera, -135*DEG2RAD, true);
    CameraPitch(&camera, -45*DEG2RAD, true, true, false);

    DisableCursor();

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
    if (IsKeyPressed(KEY_ENTER) &&  /* M-Enter */
        (IsKeyDown(KEY_LEFT_ALT) || IsKeyDown(KEY_RIGHT_ALT))) {
#ifdef PLATFORM_WEB
        emscripten_request_fullscreen("#canvas", 0);
#else
        ToggleFullscreen();
#endif
    }
    UpdateCamera(&camera, CAMERA_FIRST_PERSON);
    BeginDrawing();
    ClearBackground(DARKGREEN);
    BeginMode3D(camera);
    DrawCube((Vector3){0, 0, 0}, 2, 2, 2, RED);
    EndMode3D();
    EndDrawing();
}
```

[though CameraYaw and CameraPitch are not necessary i think. and if you remove them you can remove the rcamera.h include as well]

## Try to make going up and down work (space and lctrl)
I will try to follow more closely [the demo](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_first_person) and see if at some point they will just work.

It's not like you even need to go up and down like this in an fps, I just want to understand why the demo functions differently despite the fact it doesn't look to be handling space and ctrl explicitly.

... I just tried the demo again and I was wrong. It's only bound in `CAMERA_FREE` mode. In the demo we can switch between the camera modes and I was in free and mistakenly thought it was first person, in first person indeed ctrl and space do nothing.

So to answer the headline of this section, to make it work just do `UpdateCamera(&camera, CAMERA_FREE);` in `draw_frame`, instead of `CAMERA_FIRST_PERSON`.

and now I can fly down and land on the cube if I want. Or clip into it.

But for some reason DisableCursor is back to not working. After refresh it does. After pressing esc it stops working and doesn't come back even in fullscreen. Maybe we need to listen for left click in the window and DisableCursor again? There seems to be the same problem in the demo.

## DisableCursor on click
I can see in raylib.h that there is a function `IsMouseButtonPressed` which takes `int button`. I know from reading Hooper's article earlier that when there's an int like this it's an enum. And indeed there's this:
```c
// Mouse buttons
typedef enum {
    MOUSE_BUTTON_LEFT    = 0,       // Mouse button left
    MOUSE_BUTTON_RIGHT   = 1,       // Mouse button right
    MOUSE_BUTTON_MIDDLE  = 2,       // Mouse button middle (pressed wheel)
    MOUSE_BUTTON_SIDE    = 3,       // Mouse button side (advanced mouse device)
    MOUSE_BUTTON_EXTRA   = 4,       // Mouse button extra (advanced mouse device)
    MOUSE_BUTTON_FORWARD = 5,       // Mouse button forward (advanced mouse device)
    MOUSE_BUTTON_BACK    = 6,       // Mouse button back (advanced mouse device)
} MouseButton;
```

So let's add in `draw_frame`:
```c
    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        DisableCursor();
    }
```

It does work, but do we actually want to do this every frame left mouse button is pressed like this? We probably only want to do this when it's not captured, i.e. after esc, but is there a way to know?

```c
// Cursor-related functions
RLAPI void ShowCursor(void);                                      // Shows cursor
RLAPI void HideCursor(void);                                      // Hides cursor
RLAPI bool IsCursorHidden(void);                                  // Check if cursor is not visible
RLAPI void EnableCursor(void);                                    // Enables cursor (unlock cursor)
RLAPI void DisableCursor(void);                                   // Disables cursor (lock cursor)
RLAPI bool IsCursorOnScreen(void);                                // Check if cursor is on the screen
```
I'm not sure if `IsCursorHidden` or `IsCursorOnScreen` are going to help here.

To test, I added this in `draw_frame` before `EndDrawing()`:
```c
    DrawText(IsCursorHidden() ?
             "IsCursorHidden: true" : "IsCursorHidden: false",
             300, 400, 20, LIGHTGRAY);
    DrawText(IsCursorOnScreen() ?
             "IsCursorOnScreen: true" : "IsCursorOnScreen: false",
             300, 420, 20, LIGHTGRAY);
```
IsCursorHidden remains false always, and IsCursorOnScreen is true when it is over the game, whether or not the cursor is locked.

## Room
Somewhat tangential, but having spoken earlier of clipping into the cube, it disappears when we do this. How would you make it not disappear, to be able to go into it like a room? Give the walls depth? Or I guess we have to draw 4 cubes.

{% include note.html content='
> [!NOTE]
> By default only outside faces are only drawn. Usually in graphics libraries there is a way to specify to render it double sided (for example in three.js you can pass `side: THREE.DoubleSide` to the material), but I\'m not sure how to do this with raylib [rlgl `rlDisableBackfaceCulling`?]. Maybe you can\'t with the basic `DrawCube`.
' %}

We can write a function like this:
```c
static void draw_room
(float x, float y, float z, float w, float h, float l, Color color) {
    float t = 0.1;  /* 0.1 thick walls */
    DrawCube((Vector3){x-(w/2-t/2), y, z}, t, h, l, color);
    DrawCube((Vector3){x, y-(h/2-t/2), z}, w, t, l, color);
    DrawCube((Vector3){x, y, z-(l/2-t/2)}, w, h, t, color);
    DrawCube((Vector3){x+(w/2-t/2), y, z}, t, h, l, color);
    DrawCube((Vector3){x, y+(h/2-t/2), z}, w, t, l, color);
    DrawCube((Vector3){x, y, z+(l/2-t/2)}, w, h, t, color);
}
```

Replace our `DrawCube((Vector3){0, 0, 0}, 2, 2, 2, RED);` call with `draw_room(0, 0, 0, 2, 2, 2, RED);`

The result is still a 2x2x2 red cube, but now we can go inside.

Because it's perfectly red with no lighting, it's kind of hard to see what's going on.

I tried drawing the wireframes:
```c
static void draw_wired_cube
(Vector3 pos, float w, float h, float l, Color color) {
    DrawCube(pos, w, h, l, color);
    DrawCubeWires(pos, w, h, l, BLACK);
}

static void draw_room
(float x, float y, float z, float w, float h, float l, Color color) {
    float t = 0.1;  /* 0.1 thick walls */
    draw_wired_cube((Vector3){x-(w/2-t/2), y, z}, t, h, l, color);
    draw_wired_cube((Vector3){x, y-(h/2-t/2), z}, w, t, l, color);
    draw_wired_cube((Vector3){x, y, z-(l/2-t/2)}, w, h, t, color);
    draw_wired_cube((Vector3){x+(w/2-t/2), y, z}, t, h, l, color);
    draw_wired_cube((Vector3){x, y+(h/2-t/2), z}, w, t, l, color);
    draw_wired_cube((Vector3){x, y, z+(l/2-t/2)}, w, h, t, color);
}
```
but they're not visible from the inside.

Maybe if we vary the colours a little bit:
```c
static Color randc(Color c) {
    c.r += GetRandomValue(-50, 50);
    c.g += GetRandomValue(-50, 50);
    c.b += GetRandomValue(-50, 50);
    return c;
}

static void draw_room
(float x, float y, float z, float w, float h, float l, Color color) {
    float t = 0.1;  /* 0.1 thick walls */
    draw_wired_cube((Vector3){x-(w/2-t/2), y, z}, t, h, l, randc(color));
    draw_wired_cube((Vector3){x, y-(h/2-t/2), z}, w, t, l, randc(color));
    draw_wired_cube((Vector3){x, y, z-(l/2-t/2)}, w, h, t, randc(color));
    draw_wired_cube((Vector3){x+(w/2-t/2), y, z}, t, h, l, randc(color));
    draw_wired_cube((Vector3){x, y+(h/2-t/2), z}, w, t, l, randc(color));
    draw_wired_cube((Vector3){x, y, z+(l/2-t/2)}, w, h, t, randc(color));
}
```
... I forgot this is happening every frame, so now we have a disco cube.

## Cont 3dcfp
Let's continue making an environment like [the demo](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_first_person). I will also make a button to switch between first person and free camera.
```c
#include "raylib.h"

#if defined(PLATFORM_WEB)
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#endif

static const int width = 800;
static const int height = 450;
static Camera camera = {0};
static int camera_mode = CAMERA_FIRST_PERSON;

typedef struct Col {
    float h;
    Vector3 pos;
    Color c;
} Col;

#define NCOLS 20
static Col cols[NCOLS] = {0};

static void gen_cols(void) {
    for (int i = 0; i < NCOLS; i++) {
        float h = GetRandomValue(1, 12);
        Vector3 pos = {GetRandomValue(-15, 15), h/2.0f, GetRandomValue(-15, 15)};
        Color c = {GetRandomValue(20, 255), GetRandomValue(10, 55), 30, 255};
        cols[i] = (Col){h, pos, c};
    }
}

static void draw_frame(void);

int main(void) {
    InitWindow(width, height, "cwebfps");

    camera.position = (Vector3){0, 2, 4};
    camera.target = (Vector3){0, 2, 0};
    camera.up = (Vector3){0, 1, 0};
    camera.fovy = 60;
    camera.projection = CAMERA_PERSPECTIVE;

    gen_cols();

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

static void handle_input(void) {
    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        DisableCursor();
    }
    if (IsKeyPressed(KEY_ENTER) &&  /* M-Enter */
        (IsKeyDown(KEY_LEFT_ALT) || IsKeyDown(KEY_RIGHT_ALT))) {
#ifdef PLATFORM_WEB
        emscripten_request_fullscreen("#canvas", 0);
#else
        ToggleFullscreen();
#endif
    }
    if (IsKeyPressed(KEY_F4)) {
        camera_mode = camera_mode == CAMERA_FREE ?
            CAMERA_FIRST_PERSON : CAMERA_FREE;
    }
}

static void draw_env(void) {
    DrawPlane((Vector3){0, 0, 0}, (Vector2){32, 32}, BLACK);
    DrawCube((Vector3){-16, 2.5f, 0}, 1, 5, 32, BLUE);
    DrawCube((Vector3){16, 2.5f, 0}, 1, 5, 32, LIME);
    DrawCube((Vector3){0, 2.5f, 16}, 32, 5, 1, GOLD);
    for (int i = 0; i < NCOLS; i++) {
        DrawCube     (cols[i].pos, 2, cols[i].h, 2, cols[i].c);
        DrawCubeWires(cols[i].pos, 2, cols[i].h, 2, MAROON);
    }
}

static void draw_frame(void) {
    handle_input();
    UpdateCamera(&camera, camera_mode);
    BeginDrawing();
    ClearBackground(DARKGREEN);
    BeginMode3D(camera);
    draw_env();
    EndMode3D();
    DrawText(camera_mode == CAMERA_FREE ? "CAMERA_FREE" : "",
             300, 420, 20, LIGHTGRAY);
    EndDrawing();
}
```
Free camera key: f4. I wanted it to be S-p but it doesn't work. Then I thought about C-p, but that's print. This reminds me of playing Ultrakill Prelude web port where you could accidentally close the page by crouching and pressing w at the same time, which I did absent-mindedly after almost finishing the game. Most people, like me, will be too lazy to change the bindings just for a game. We have to avoid anything similar to browser bindings, and thus probably avoid ctrl altogether.

We're done with this example.

## 3dcfps
Named almost the same, there is [another example](https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_fps), by [Agnis Aldiņš](https://github.com/nezvers), with nicer movement. It was that one I meant to link at the start, but got confused between them. It is a lot more complex, so I think it is worth having gone through the other one first in any case.

I also notice that when you go inside the towers, you see the wireframe, which I didn't with my cube before, but maybe I did something wrong. With the cols we have now with the maroon wireframe, I do see the wireframe.

Instead of calling `UpdateCamera`, he calls his own function `UpdateCameraFPS` (though he handles most of the movement outside of that). It is `UpdateCamera` with `CAMERA_FIRST_PERSON` or `CAMERA_FREE` that was handling movement inputs for us. We now graduate to handling it ourselves, like you do in most game engines.

He stores player movement information in a struct like this:
```c
typedef struct {
    Vector3 position;
    Vector3 velocity;
    Vector3 dir;
    bool isGrounded;
} Body;
```

Other global variables he stores outside of that are sensitivity (Vector2D), lookRotation (Vector2D), headTimer (float), walkLerp (float), headLerp (float), lean (Vector2D). He also has #defines with movement constants: gravity 32, max speed 20, crouch speed 5, jump force 12, max accel 150, friction .86 ("Grounded drag"), air drag .98 ("Increasing air drag, increases strafing speed"), control 15 ("Responsiveness for turning movement direction to looked direction"), crouch height 0, stand height 1, bottom height .5.

## Camera confusions
To begin with I'm modifying camera settings, and I've hit a confusion. In free camera mode, if I set camera.position to `(Vector3){0, 0, 0}` and don't set camera.up and camera.target, it gets stuck at (0, 0, 0). I see nothing and the free camera movement keys do nothing. I added text that prints the camera position on the screen to verify:
```c
DrawText(TextFormat("(%06.3f, %06.3f, %06.3f)", camera.position.x, camera.position.y, camera.position.z), 0, 420, 20, LIGHTGRAY);
```
Nothing in the console either.

If I set the y of camera.position to something other than 0, like `camera.position = (Vector3){0, 2, 0}`, then only w and s work, only moving in the y axis. x and z stay 0. a, d, ctrl, and space do nothing.

If I set it to `(Vector3){1, 1, 0}`, still only w and s work, and only the x and y values change, both at the same time to the same value. The other keys do nothing, and we still see nothing.

{1,1,1} is the same but with all three coordinates (still only w and s work, and set the coordinates of all three to the same thing).

If I set to different values it's the same, except they don't all get the same values, but they all increase/decrease by the same rate when you press w/s, with the other keys doing nothing, and nothing visible.

Is it that we have to set up and target, otherwise the behaviour is undefined or something? Let's try setting them all to 0 (even though surely that's what they are already after we initialised the camera to `{0}` when declaring the variable).
```c
    camera.position = (Vector3){0, 0, 0};
    camera.target = (Vector3){0, 0, 0};
    camera.up = (Vector3){0, 0, 0};
```
It's stuck as I mentioned before, don't see anything and keys don't do anything, camera coordinates stuck at (0, 0, 0).

Attempt 2:
```c
    camera.position = (Vector3){0, 0, 0};
    camera.target = (Vector3){0, 1, 0};
    camera.up = (Vector3){0, 1, 0};
```
Can't see anything, only w and s work, only y coordinate changes.

Attempt 3:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.target = (Vector3){0, 1, 0};
    camera.up = (Vector3){0, 1, 0};
```
Can't see anything, no keys work, coordinates stuck on (0, 1, 0).

Attempt 4:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.target = (Vector3){0, 2, 0};
    camera.up = (Vector3){0, 1, 0};
```
Can't see anything, only w and s work, only y coordinate changes.

Attempt 5:
```c
    camera.position = (Vector3){0, 1, 1};
    camera.target = (Vector3){0, 2, 0};
    camera.up = (Vector3){0, 1, 0};
```
Can see and all the movement keys work.

I am very confused.

Is this me positioning the camera in a nonsensical way? Position and target of a camera are straight-forward to understand, but what is up? And is the position and target being the same impossible?

From raylib.h:
```c
// Camera, defines position/orientation in 3d space
typedef struct Camera3D {
    Vector3 position;       // Camera position
    Vector3 target;         // Camera target it looks-at
    Vector3 up;             // Camera up vector (rotation over its axis)
    float fovy;             // Camera field-of-view aperture in Y (degrees) in perspective, used as near plane height in world units in orthographic
    int projection;         // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
} Camera3D;
```

> Look at anything. Now tilt your head 90 degrees. Where you are hasn't changed, the direction you're looking at hasn't changed, but the image in your retina clearly has. What's the difference? Where the top of your head is pointing to. That's the up vector.  
—[Carlos Scheidegger, 2010](https://stackoverflow.com/a/3380207/18396947)

and what if this vector is {0,0,0}?

with this:
```c
    camera.position = (Vector3){0, 1, 1};
    camera.target = (Vector3){0, 2, 0};
```
and no camera.up, it's broken in the same way as before where only the w and s keys do anything, move only the y and z coordinates, and we see nothing.

I thought based on it being the rotation that it would not have anything to do with the problem, but I guess it shouldn't be zero. Makes sense, because a zero vector doesn't have direction.

Can the target be a zero vector?

Yes. This is sufficient:
```c
    camera.position = (Vector3){0, 1, 1};
    camera.up = (Vector3){0, 1, 0};
```
but without position, it is back to being broken. so I think position and up cannot be zero vectors.

But that's not the only thing. This is broken too:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.up = (Vector3){0, 1, 0};
```
With position {0,1,1} or {1,1,0} it's fine, but {0,1,0} is broken. {1,0,0} is fine. {0,0,1} is also fine.

Is it something to do with it being the same as up?

This works:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.up = (Vector3){1, 0, 0};
```
(camera looking down)

This is broken:
```c
    camera.position = (Vector3){1, 0, 0};
    camera.up = (Vector3){1, 0, 0};
```

So I guess:
1. position can't be a zero vector
2. up can't be a zero vector
3. position and up can't be equal

But why? Up vector direction is relative to position?

1 doesn't make sense to me either, why can't the camera position be {0,0,0}?

It almost certainly can. It's probably that it has to be different from the target vector. Which would bring us to:
1. position and target can't be equal
2. up can't be a zero vector

which would make more sense

It appears to be true, because this is fine:
```c
    camera.position = (Vector3){0, 0, 0};
    camera.target = (Vector3){0, 1, 0};
    camera.up = (Vector3){1, 0, 0};
```

if it's true it leaves only the confusion for 3

This is broken:
```c
    camera.position = (Vector3){0, 0, 0};
    camera.target = (Vector3){0, 1, 0};
    camera.up = (Vector3){0, 1, 0};
```
but here it's not position and up that are the same, but rather target and up.

This is fine:
```c
    camera.position = (Vector3){0, 0, 0};
    camera.target = (Vector3){1, 0, 0};
    camera.up = (Vector3){0, 1, 0};
```

Postulations:
- position can be anything
- target can be anything
- up can't be a zero vector
- position and target can't be equal
- up and target/position can't be equal?

The last one is maybe that the up vector can't be the same direction as the direction of the camera given its position and target. If it's at {0,0,0} pointing at {0,1,0}, this is the same direction as the up vector {0,1,0}.

I'm going with:
1. position and target can't be equal (because there must be a facing direction for the camera)
2. up can't be a zero vector (because there must be an up direction for the camera)

(trying to be consistent with numbering from before)

But 1 establishes that you need to have two vectors in order to have a direction, so surely 2 has to be in that vein; rather than simply that it can't be a zero vector, that it can't be equal to one of the other vectors, probably position.

But this is broken:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.target = (Vector3){1, 0, 0};
    camera.up = (Vector3){0, 0, 0};
```
and I can't get any values to work while up is {0,0,0}, so it seems like it really can't be a zero vector.

So I'm still a little bit confused.

## 3dcfps basic movement attempts
There are rather a lot of things happening, but let's start with basic wasd. Every frame:
```c
        char sideway = (IsKeyDown(KEY_D) - IsKeyDown(KEY_A));
        char forward = (IsKeyDown(KEY_W) - IsKeyDown(KEY_S));
```
char is just a small number data storage (1 byte), and booleans can be coerced to 0/1 (raylib uses stdbool.h if it's available, so it really is booleans even though [they weren't always in C](https://stackoverflow.com/questions/1921539/using-boolean-values-in-c)), so `sideway` is going to be 1 if D is pressed and A is not, -1 if A is pressed and D is not, 0 if neither is pressed, and 0 if both are pressed. Same thing with `forward` and W and S. This can give a direction to move in, or don't move at all if neither or both are pressed.

He then calls `UpdateBody`, but it has a lot of other things that I don't want to deal with right now.

To begin with remove the call to `UpdateCamera`, so we have no ability to move at all.

Naïve movement attempt: just update the camera position based on the input multiplied by delta time each frame.
```c
float delta = GetFrameTime();
camera.position.x += sideway*delta;
camera.position.z += forward*delta;
```
y is the height of the player, so we'll keep that the same (later on we will need to change it to be able to jump).

I put this at the top of `draw_frame` for now.

The result is pretty weird. w/a/s/d keys take you in some direction, not necessarily forwards/left/backwards/right, it depends on your orientation, which changes as we're moving around. Kind of feels like skating.

The camera.position x and z are in world coordinates, not relative to the camera. We want to go forwards relative to the camera.

And I think we need to move camera.target as well, otherwise the camera will rotate when we move to the side.

If we think of just moving forwards, if camera.position is at {0,1,0} and camera.target is at {10,1,0}, we're facing in the x direction, we really could just do:
```c
camera.position.x += forward*delta;
```
and forwards and backwards work as expected (so long as we don't turn).

To go to the side, modify position.z and target.z at the same time?
```c
camera.position.z += sideway*delta;
camera.target.z += sideway*delta;
```
It works, but:
- going diagonally is faster than going in just one direction
- this will only work when we are facing perfectly towards +x

The two issues can be solved at the same time by using vector maths to do the movements. I like [this 2015 explanation by MAnd](https://gamedev.stackexchange.com/a/111810) (although he is handling 2D movements rather than 3D, the same concepts apply).
1. work out the direction vector by subtracting desired position from current position
2. normalise this vector (turn it into a unit vector; a vector of length 1)
3. add it to current position multiplied by deltatime (and by another scalar controlling speed if you want)

Adding/subtracting vectors is adding/subtracting each component (x and x, y and y, z and z). We don't have to do this manually, raymath.h already has a function for it:
```c
// Add two vectors
RMAPI Vector3 Vector3Add(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };

    return result;
}
```
and:
```c
// Subtract two vectors
RMAPI Vector3 Vector3Subtract(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x - v2.x, v1.y - v2.y, v1.z - v2.z };

    return result;
}
```

Normalising a vector can be done by dividing the vector by its length. To work out the length, it's like using pythagoras theorem to find the length of the hypotenuse: square root of the sum of squares of each component.

There is also a function in raymath.h for normalise:
```c
// Normalize provided vector
RMAPI Vector3 Vector3Normalize(Vector3 v)
{
    Vector3 result = v;

    float length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
    if (length != 0.0f)
    {
        float ilength = 1.0f/length;

        result.x *= ilength;
        result.y *= ilength;
        result.z *= ilength;
    }

    return result;
}
```

and multiply:
```c
// Multiply vector by vector
RMAPI Vector3 Vector3Multiply(Vector3 v1, Vector3 v2)
{
    Vector3 result = { v1.x*v2.x, v1.y*v2.y, v1.z*v2.z };

    return result;
}
```
but since deltatime isn't a vector, what we actually need is scale:
```c
// Multiply vector by scalar
RMAPI Vector3 Vector3Scale(Vector3 v, float scalar)
{
    Vector3 result = { v.x*scalar, v.y*scalar, v.z*scalar };

    return result;
}
```

But the problem with step 1 is what does "desired position" mean? If the user is holding W we want to move the position forwards without changing direction. And the direction is dependent also on camera.target.

So I think we can work out the direction by subtracting camera.target from camera.position.
```c
Vector3 dir = Vector3Subtract(camera.position, camera.target);
Vector3 normalised = Vector3Scale(Vector3Normalize(dir), delta);
camera.position = Vector3Add(camera.position, normalised);
```
(remembering to `#include "raymath.h"` first)

The result is we are sliding backwards

We're not taking account of the input and are just moving away(?) from the target

When `forward` is 1 we want to go towards the target, when it's -1 we want to go away from the target, and when it's 0 we don't want to move. So multiply dir by it?
```c
Vector3 dir = Vector3Scale(Vector3Subtract(camera.position, camera.target), forward);
```
It works but in reverse; W moves back, S forwards. My camera starting position:
```c
    camera.position = (Vector3){0, 1, 0};
    camera.target = (Vector3){10, 1, 0};
    camera.up = (Vector3){0, 1, 0};
```
I don't really know what is considered "forwards". Is the target in front of or behind the camera, in terms of world coordinates?

We could pass the arguments to subtract the other way around (target, position).

For implementing side movement, we could use this raymath function:
```c
// Calculate one vector perpendicular vector
RMAPI Vector3 Vector3Perpendicular(Vector3 v)
{
    Vector3 result = { 0 };

    float min = fabsf(v.x);
    Vector3 cardinalAxis = {1.0f, 0.0f, 0.0f};

    if (fabsf(v.y) < min)
    {
        min = fabsf(v.y);
        Vector3 tmp = {0.0f, 1.0f, 0.0f};
        cardinalAxis = tmp;
    }

    if (fabsf(v.z) < min)
    {
        Vector3 tmp = {0.0f, 0.0f, 1.0f};
        cardinalAxis = tmp;
    }

    // Cross product between vectors
    result.x = v.y*cardinalAxis.z - v.z*cardinalAxis.y;
    result.y = v.z*cardinalAxis.x - v.x*cardinalAxis.z;
    result.z = v.x*cardinalAxis.y - v.y*cardinalAxis.x;

    return result;
}
```

Attempt:
```c
    Vector3 dir = Vector3Subtract(camera.target, camera.position);
    Vector3 ws = Vector3Normalize(Vector3Scale(dir, forward));
    Vector3 ad = Vector3Normalize(Vector3Scale(Vector3Perpendicular(dir), sideway));
    camera.position = Vector3Add(camera.position, Vector3Scale(ws, delta));
    camera.position = Vector3Add(camera.position, Vector3Scale(ad, delta));
```

ad rotates, and if you keep doing it, especially combined with other inputs, it gets uncontrollable, flashing images, different axes of rotation.

I forgot that we need to change the target as well to move from side to side. But I suspect there's more than just that that's wrong with it.

I'm fed up with my horrible attempts, let's go back to the [example].

Still trying to do the minimum.

At the start:
```c
typedef struct {
    Vector3 velocity;
    Vector3 dir;
} Body;

static Body player = {0};
```
Took out position (going to update camera position directly), and isGrounded (it's just for jumping).

Every frame:
```c
char sideway = (IsKeyDown(KEY_D) - IsKeyDown(KEY_A));
char forward = (IsKeyDown(KEY_W) - IsKeyDown(KEY_S));
Vector2 input = (Vector2){(float)sideway,(float)-forward};
float delta = GetFrameTime();
float rot = 0;  // normally calculated from mouse movement
Vector3 front = (Vector3){sinf(rot), 0, cosf(rot)};
Vector3 right = (Vector3){cosf(-rot), 0, sinf(-rot)};
Vector3 desiredDir = (Vector3){input.x*right.x + input.y*front.x, 0, input.x*right.z + input.y*front.z};
player.dir = Vector3Lerp(player.dir, desiredDir, delta);
float decel = 0.86f;  // friction
Vector3 hvel = (Vector3){player.velocity.x*decel, 0.0f, player.velocity.z*decel};
float hvelLength = Vector3Length(hvel);
float speed = Vector3DotProduct(hvel, player.dir);
float maxSpeed = 20;
float accel = Clamp(maxSpeed - speed, 0, 150*delta);  // 150 = max accel
hvel.x += player.dir.x*accel;
hvel.z += player.dir.z*accel;
player.velocity.x = hvel.x;
player.velocity.z = hvel.z;
camera.position.x += player.velocity.x*delta;
camera.position.y += player.velocity.y*delta;
camera.position.z += player.velocity.z*delta;
```
That was my best effort to excise most of it

At first was getting the exact same behaviour as before and had to hard refresh. Have to be careful about that as I could see it leading me to wrong conclusions, and maybe it already did.

It's not good. Sliding around uncontrollably, all the keys cause rotation

This feels beyond me. There's too much going on in this example when I haven't even understood the mechanics behind basic movements.

Maybe I should try the opposite approach; get the example working on the web, then excise from it little by little.

```c
#include "raylib.h"
#include "raymath.h"

#if defined(PLATFORM_WEB)
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#endif

#define GRAVITY          32.0f
#define MAX_SPEED        20.0f
#define CROUCH_SPEED      5.0f
#define JUMP_FORCE       12.0f
#define MAX_ACCEL       150.0f
#define FRICTION         0.86f
#define AIR_DRAG         0.98f
#define CONTROL          15.0f
#define CROUCH_HEIGHT     0.0f
#define STAND_HEIGHT      1.0f
#define BOTTOM_HEIGHT     0.5f
#define NORMALIZE_INPUT   0

typedef struct {
    Vector3 position;
    Vector3 velocity;
    Vector3 dir;
    bool grounded;
} Body;

static Vector2 sensitivity = {0.001f, 0.001f};
static Body player = {0};
static Vector2 look_rot = {0};
static float head_timer = 0;
static float walk_lerp = 0;
static float head_lerp = STAND_HEIGHT;
static Vector2 lean = {0};
static Camera camera = {0};

static void handle_frame(void);
static void update_camera(void);

int main(void) {
    InitWindow(800, 450, "cwebfps");

    camera.fovy = 60.0f;
    camera.projection = CAMERA_PERSPECTIVE;
    camera.position = (Vector3){
        player.position.x,
        player.position.y + (BOTTOM_HEIGHT + head_lerp),
        player.position.z,
    };

    update_camera();

#if defined(PLATFORM_WEB)
    emscripten_set_main_loop(handle_frame, 60, 1);
#else
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        handle_frame();
    }
#endif

    CloseWindow();
    return 0;
}

static void handle_input(void) {
    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        DisableCursor();
    }
    if (IsKeyPressed(KEY_ENTER) &&  /* M-Enter */
        (IsKeyDown(KEY_LEFT_ALT) || IsKeyDown(KEY_RIGHT_ALT))) {
#ifdef PLATFORM_WEB
        emscripten_request_fullscreen("#canvas", 0);
#else
        ToggleFullscreen();
#endif
    }
}

void update_body
(Body *body, float rot, char side, char forward, bool jump, bool crouch) {
    Vector2 input = (Vector2){(float)side, (float)-forward};

#if defined(NORMALIZE_INPUT)
    // Slow down diagonal movement
    if ((side != 0) && (forward != 0)) input = Vector2Normalize(input);
#endif

    float delta = GetFrameTime();

    if (!body->grounded) body->velocity.y -= GRAVITY*delta;

    if (body->grounded && jump) {
        body->velocity.y = JUMP_FORCE;
        body->grounded = false;
    }

    Vector3 front = (Vector3){sinf(rot), 0.f, cosf(rot)};
    Vector3 right = (Vector3){cosf(-rot), 0.f, sinf(-rot)};

    Vector3 desired_dir = (Vector3){
        input.x*right.x + input.y*front.x,
        0.0f,
        input.x*right.z + input.y*front.z,
    };
    body->dir = Vector3Lerp(body->dir, desired_dir, CONTROL*delta);

    float decel = (body->grounded ? FRICTION : AIR_DRAG);
    Vector3 hvel = (Vector3){body->velocity.x*decel, 0.0f, body->velocity.z*decel};

    float hvelLength = Vector3Length(hvel);
    if (hvelLength < (MAX_SPEED*0.01f)) hvel = (Vector3){0};

    // This is what creates strafing
    float speed = Vector3DotProduct(hvel, body->dir);

    // Whenever the amount of acceleration to add is clamped by the maximum acceleration constant,
    // a Player can make the speed faster by bringing the direction closer to horizontal velocity angle
    // More info here: https://youtu.be/v3zT3Z5apaM?t=165
    float max_speed = (crouch ? CROUCH_SPEED : MAX_SPEED);
    float accel = Clamp(max_speed - speed, 0.f, MAX_ACCEL*delta);
    hvel.x += body->dir.x*accel;
    hvel.z += body->dir.z*accel;

    body->velocity.x = hvel.x;
    body->velocity.z = hvel.z;

    body->position.x += body->velocity.x*delta;
    body->position.y += body->velocity.y*delta;
    body->position.z += body->velocity.z*delta;

    // Fancy collision system against the floor
    if (body->position.y <= 0.0f) {
        body->position.y = 0.0f;
        body->velocity.y = 0.0f;
        body->grounded = true; // Enable jumping
    }
}

static void movement(void) {
    Vector2 mouse_delta = GetMouseDelta();
    look_rot.x -= mouse_delta.x*sensitivity.x;
    look_rot.y += mouse_delta.y*sensitivity.y;

    char side = (IsKeyDown(KEY_D) - IsKeyDown(KEY_A));
    char forward = (IsKeyDown(KEY_W) - IsKeyDown(KEY_S));
    bool crouching = IsKeyDown(KEY_LEFT_CONTROL);
    update_body(&player, look_rot.x, side, forward,
                IsKeyPressed(KEY_SPACE), crouching);

    float delta = GetFrameTime();
    head_lerp = Lerp(head_lerp, (crouching ? CROUCH_HEIGHT : STAND_HEIGHT), 20.0f*delta);
    camera.position = (Vector3){
        player.position.x,
        player.position.y + (BOTTOM_HEIGHT + head_lerp),
        player.position.z,
    };

    if (player.grounded && ((forward != 0) || (side != 0))) {
        head_timer += delta*3.0f;
        walk_lerp = Lerp(walk_lerp, 1.0f, 10.0f*delta);
        camera.fovy = Lerp(camera.fovy, 55.0f, 5.0f*delta);
    } else {
        walk_lerp = Lerp(walk_lerp, 0.0f, 10.0f*delta);
        camera.fovy = Lerp(camera.fovy, 60.0f, 5.0f*delta);
    }

    lean.x = Lerp(lean.x, side*0.02f, 10.0f*delta);
    lean.y = Lerp(lean.y, forward*0.015f, 10.0f*delta);
}

static void update_camera() {
    const Vector3 up = (Vector3){0.0f, 1.0f, 0.0f};
    const Vector3 target_offset = (Vector3){0.0f, 0.0f, -1.0f};

    // Left and right
    Vector3 yaw = Vector3RotateByAxisAngle(target_offset, up, look_rot.x);

    // Clamp view up
    float max_angle_up = Vector3Angle(up, yaw);
    max_angle_up -= 0.001f; // Avoid numerical errors
    if (-(look_rot.y) > max_angle_up) look_rot.y = -max_angle_up;

    // Clamp view down
    float max_angle_down = Vector3Angle(Vector3Negate(up), yaw);
    max_angle_down *= -1.0f; // Downwards angle is negative
    max_angle_down += 0.001f; // Avoid numerical errors
    if (-(look_rot.y) < max_angle_down) look_rot.y = -max_angle_down;

    // Up and down
    Vector3 right = Vector3Normalize(Vector3CrossProduct(yaw, up));

    // Rotate view vector around right axis
    float pitch_angle = -look_rot.y - lean.y;
    // Clamp angle so it doesn't go past straight up or straight down
    pitch_angle = Clamp(pitch_angle, -PI/2 + 0.0001f, PI/2 - 0.0001f);
    Vector3 pitch = Vector3RotateByAxisAngle(yaw, right, pitch_angle);

    // Head animation
    // Rotate up direction around forward axis
    float head_sin = sinf(head_timer*PI);
    float head_cos = cosf(head_timer*PI);
    const float step_rot = 0.01f;
    camera.up = Vector3RotateByAxisAngle(up, pitch, head_sin*step_rot + lean.x);

    // Camera bob
    const float bob_side = 0.1f;
    const float bob_up = 0.15f;
    Vector3 bob = Vector3Scale(right, head_sin*bob_side);
    bob.y = fabsf(head_cos*bob_up);

    camera.position = Vector3Add(camera.position, Vector3Scale(bob, walk_lerp));
    camera.target = Vector3Add(camera.position, pitch);
}

static void draw_level(void) {
    const int floor_extent = 25;
    const float tile_size = 5;
    const Vector2 plane_size = (Vector2){tile_size, tile_size};
    const Color color1 = (Color){150, 200, 200, 255};
    const Color color2 = LIGHTGRAY;

    for (int y = -floor_extent; y < floor_extent; y++) {
        for (int x = -floor_extent; x < floor_extent; x++) {
            if ((y & 1) && (x & 1)) {
                DrawPlane((Vector3){x*tile_size, 0.0f, y*tile_size},
                          plane_size, color1);
            } else if (!(y & 1) && !(x & 1)) {
                DrawPlane((Vector3){x*tile_size, 0.0f, y*tile_size},
                          plane_size, color2);
            }
        }
    }
}

static void handle_frame(void) {
    handle_input();
    movement();
    update_camera();

    BeginDrawing();
    ClearBackground(RAYWHITE);
    BeginMode3D(camera);
    draw_level();
    EndMode3D();
    DrawText(TextFormat
             ("velocity len (%06.3f) \
camera.position (%06.3f, %06.3f, %06.3f) \
camera.target (%06.3f, %06.3f, %06.3f)",
              Vector2Length((Vector2){player.velocity.x, player.velocity.z}),
              camera.position.x, camera.position.y, camera.position.z,
              camera.target.x, camera.target.y, camera.target.z),
             15, 10, 10, BLACK);
    EndDrawing();
}
```
So far I only excised most of the draw level logic to leave only the floor.

If I leave it running for a long time, the ground disappears and velocity len is -nan. It happens on the version on the site examples as well. I wonder what causes that. Does it happen if I don't move from the starting position?

Seems like no, and you can also move the mouse without ill effects, but moving even a little with wasd causes this bug. It looks like the velocity goes to inf while it's off-screen and the camera position values get really high, or all become -nan

## fov change when moving?
I find it annoying that when moving forwards or backwards the fov changes (I think). Or maybe it's just tilting?

In the function `movement`:
```c
    if (player.grounded && ((forward != 0) || (side != 0))) {
        head_timer += delta*3.0f;
        walk_lerp = Lerp(walk_lerp, 1.0f, 10.0f*delta);
        camera.fovy = Lerp(camera.fovy, 55.0f, 5.0f*delta);
    } else {
        walk_lerp = Lerp(walk_lerp, 0.0f, 10.0f*delta);
        camera.fovy = Lerp(camera.fovy, 60.0f, 5.0f*delta);
    }
```
Let's try just removing the `camera.fovy` lines from here

and remember to hard refresh or it might give us the previous build

There's still tilt, which could just be the bobbing. It could be desireable, but I'm trying to remove features one by one until being left with the most basic movement, so let's try to remove that too.

The functions to look at are `movement` and `update_camera`. `update_body` doesn't do any leaning or bobbing, it only deals with direction, position, velocity, and jumping.

It's quite hard because there's a lot of logic for leaning and camera angle and I don't know how to decouple it.

The last two lines of `update_camera` are:
```c
    camera.position = Vector3Add(camera.position, Vector3Scale(bob, walk_lerp));
    camera.target = Vector3Add(camera.position, pitch);
```
what would happen if I just comment them out? It could be everything would break, but I think not because the camera position is already updated to the player position in `movement`. But that last line in `update_camera` is the only thing that updates `camera.target`, so that one I think is necessary. I'm only going to comment out the line before last, then.

No noticeable difference. Even after hard refresh. There may be a slight difference that I can't really see. The camera is still tilting, both forwards/backwards when you start moving, then gently left and right as you walk.

A little further up in `update_camera`, there is:
```c
    // Head animation
    // Rotate up direction around forward axis
    float head_sin = sinf(head_timer*PI);
    float head_cos = cosf(head_timer*PI);
    const float step_rot = 0.01f;
    camera.up = Vector3RotateByAxisAngle(up, pitch, head_sin*step_rot + lean.x);
```
Let's comment that out.

Blank screen.

Looking at raymath.h, Vector3RotateByAxisAngle rotates a vector around an axis using [Euler-Rodrigues formula](https://en.wikipedia.org/wiki/Euler%E2%80%93Rodrigues_formula). It takes `Vector3 v, Vector3 axis, float angle`.

camera.position (0, 1.5, 0), target (0, 1.5, -1)

If I put back the line:

camera.position (0, 1.5, 0), target (0, 1.5, -1)

Ah wait, it's camera.up that we're concerned with here. Let's add it to the onscreen text.
```c
    DrawText(TextFormat
             ("velocity len (%06.3f) \
camera.position (%06.3f, %06.3f, %06.3f) \
camera.target (%06.3f, %06.3f, %06.3f) \
camera.up (%06.3f, %06.3f, %06.3f)",
              Vector2Length((Vector2){player.velocity.x, player.velocity.z}),
              camera.position.x, camera.position.y, camera.position.z,
              camera.target.x, camera.target.y, camera.target.z,
              camera.up.x, camera.up.y, camera.up.z),
             15, 10, 10, BLACK);
```

With the line in place, it's (0, 1, 0). Without it, it's (0, 0, 0). And we already established earlier in the camera confusions section that the camera breaks when camera.up is a zero vector.

Then let's try adding just:
```c
    camera.up = (Vector3){0, 1, 0};
```

We can see and walk again

Now there is no longer the swaying motion as we walk, and moving side to side there is no tilting at all, like I want, but moving forwards or backwards, there is still a tilt. Despite the fact nothing is modifying camera.up anymore, it is now static at (0, 1, 0).

I think this tilt is in the y coordinate of camera.target. It's normally 1.5, when I walk forwards it goes down to 1.485, and when I walk backwards it goes up to 1.515.

It's probably this in `update_camera`:
```c
    // Rotate view vector around right axis
    float pitch_angle = -look_rot.y - lean.y;
    // Clamp angle so it doesn't go past straight up or straight down
    pitch_angle = Clamp(pitch_angle, -PI/2 + 0.0001f, PI/2 - 0.0001f);
    Vector3 pitch = Vector3RotateByAxisAngle(yaw, right, pitch_angle);
```
combined with the last line of `update_camera`:
```c
    camera.target = Vector3Add(camera.position, pitch);
```

We established in camera confusions that camera position and target should never be equal, or the camera breaks. But in his case they are the same other than this 0.015 tilt I'm observing.

If I do:
```c
    camera.target = camera.position;
```
then as expected, it breaks (blank screen).

I need the target to be the same as position, just a little bit further on in the right direction. I don't know how to do that.

I tried adding position and position for some reason:
```c
    camera.target = Vector3Add(camera.position, camera.position);
```
The result is weird. Rotating around.

Let's maybe try a scalar?
```c
camera.target = Vector3Scale(camera.position, 1.1);
```
I don't know what I'm doing

Still rotating around

Wait, we don't actually want them to stay the same, they only stay the same if we don't move the mouse, which I'm not doing bc I'm lazily testing with just the keyboard. We do still need his calculation of the pitch. Just instead of the tilt we need it to be a bit further on in the same direction, without tilt.

[i think the above camera is confused, the camera rotation stuff for the mouse changes position, it's not related to the calculation of target, which uses the same vector as position just with an added tilt]

In the code the only 0.015 value is in the last line of `movement`:
```c
    lean.y = Lerp(lean.y, forward*0.015f, 10.0f*delta);
```
lean.y is then used to calculate `pitch_angle`, as seen above, then pitch is yaw rotated by that much

It's silly but one way to deal with it is rotate it by a very small value
```c
    Vector3 pitch = Vector3RotateByAxisAngle(yaw, right, 0.00001f);
```

Or just use yaw directly?
```c
    Vector3 pitch = yaw;
```

This works. And if I move around without rotating the camera, position and target are the same value bar -1 on the z axis of target

Initial:
- pos: (0, 1.5, 0)
- tar: (0, 1.5, -1)

Moved forwards a bit:
- pos: (0, 1.5, -2)
- tar: (0, 1.5, -3)

Moved left a bit:
- pos: (-2, 1.5, -2)
- tar: (-2, 1.5, -3)

## Potential cause for velocity inf/-nan
Hypothesis for velocity inf/-nan bug: maybe it's caused by velocity not being at 0 yet when the tab/window loses focus/visibility?

Maybe not. I make sure it's 0 then come back some time later to find it's -nan

but maybe that's because I accidentally switched to the tab and made some kind of movement input accidentally then quickly switched to another?

No, I can confirm it goes crazy in the background without further inputs whatsoever (so long as, as I said earlier, you made at least one movement input in the window before switching to something else, even if you make sure to wait for it to go down to 0)

TODO

[example]: https://www.raylib.com/examples/core/loader.html?name=core_3d_camera_fps

{% include fin.html %}
