---
layout: post
title: 25 — Raylib C web fps first steps
date: 2026-07-10 05:49
modified_date: 2026-07-11 20:34
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

## Build the example for web
It looks already set up for the web, from all the references to `PLATFORM_WEB` both in the C source code and makefile.

At the beginning of the makefile there is:
```make
# Define target platform: PLATFORM_DESKTOP, PLATFORM_WEB, PLATFORM_DRM, PLATFORM_ANDROID
PLATFORM              ?= PLATFORM_DESKTOP
```
So do we need to just set this to web and make again?

TODO

in devlog # we already did a thing wtih emscripten (and i should check again to see what the steps were to set it up before continuing here)


{% include fin.html %}
