---
layout: post
title:  "Building Tesseract 4 with training tools on Windows"
date:   2021-04-27
modified_date: 2021-07-22 13:05
categories: ocr compilation
redirect_from:
  - /building-tesseract
---

<small>[April 22, 25, 26, 27]</small>

To train Tesseract 4, you either need a build that comes with the training tools, or build it yourself.

Getting a working build was nontrivial, so I want to share the steps I took. I built 4.1.1 which is the latest 4 version and is the version Tesseract.js is based on in the moment of writing this.

## Premade builds with training tools
For Windows, the options are limited. I am only aware of [Mannheim](https://github.com/UB-Mannheim/tesseract/wiki) who provide such premade builds, and sadly the training tools on their 4.1.1 builds are unusable due to missing ICU dll. I tried them all.

The other option is the Cygwin packages. I tried the packages `tesseract-training-core-4.00-2`, `tesseract-training-eng-4.00-2`, `tesseract-training-util-4.1.1-1`. Unfortunately I could not get past an issue with the `text2image` executable where it just spits out the help text, whatever options you give it.

So, build it myself it is then.

## 4.1.1 from source
You can download the state of the repository during the 4.1.1 release from [here](https://github.com/tesseract-ocr/tesseract/releases/tag/4.1.1).

As you can see in the release notes, in 4.1.1 they changed from the deprecated cppan to [sw](https://github.com/SoftwareNetwork/sw) to build Tesseract.

The [instructions](https://tesseract-ocr.github.io/tessdoc/Compiling.html#windows) seem simple, just install sw and run the command `sw build` in the Tesseract source folder.

## Building it with just sw
`sw` requires the latest version of Visual Studio with “Desktop development with C++ workload” which includes the MSVC compiler, toolset, and libraries for building applications. After installation, you need to restart your computer before going any further.

`sw` can be downloaded from [software-network.org](https://software-network.org/client/). Place the executable in some folder on your PATH.

You may want to configure `sw` to use a different storage directory, because by default it will be under your user folder. Create a `sw.yml` file under a `.sw` directory in your user folder. For me it is `C:\Users\P\.sw\sw.yml`. In the file write `storage_dir:` followed by the path where you’d like it to be, for example:

```yml
storage_dir: I:\p\.sw\storage
```

For more options, there is a manual [here](https://software-network.org/client/sw.pdf).

Now running `sw build` in the source folder should work for a while, eventually resulting in some built executables and dlls should be in subfolder `.sw/out/<some number>`. You can avoid the dlls / have the libraries bundled into the executables ([static linking](https://en.wikipedia.org/wiki/Static_library)) by instead building with: `sw -static build`.

However, on my machine, the resulting tesseract executable (`google.tesseract.tesseract-master.exe`) does not function properly. It outputs help text fine, but anything else (even `--list-langs`) leads to a crash. To test yours:

1. Make a folder somewhere
2. Make an environment variable called `TESSDATA_PREFIX` with a path to the folder (if you are in PowerShell and want to set it just for this session, you can do `$Env:TESSDATA_PREFIX = "A:/PATH/TO/YOUR/FOLDER"`)
3. Download some traineddata files, e.g. from [tesseract-ocr/tessdata](https://github.com/tesseract-ocr/tessdata)

Now you can run `google.tesseract.tesseract-master.exe --list-langs`, and if it works properly, say you’ve placed `eng.tessdata` and `osd.tessdata` in your folder, it should print something like:

    List of available languages (2):
    eng
    osd

For me it just crashes with no output.

<small>[The training tools built this way seem fine, so would you be able simply use a prebuilt version of Tesseract 4.11 for Tesseract itself and use that and the training tools you built?]</small>

If yours is fine, you can skip to [Training]({% post_url 2021-04-29-training-tesseract %}).

## Building it with sw and Visual Studio
The idea is to generate the Visual Studio solution from the source files with the makefile that’s included in the Tesseract repository, and then compile the solution using Visual Studio. The first step requires:
- `cmake`: Download and install [latest](https://github.com/Kitware/CMake/releases/latest) for your system, then add the path to `cmake.exe` to your PATH environment variable.
- `pkg-config`: This is normally difficult to get working on Windows, but the fork [pkg-config-lite](https://sourceforge.net/projects/pkgconfiglite/files/) _just works_. Download and extract somewhere, then add the path to `pkg-config.exe` to your PATH environment variable.

Now in the Tesseract source folder, run:

```bash
mkdir build
cd build
cmake .. -G "Visual Studio 16 2019" -A "x64" -DSW_BUILD=1
```

If you are building for a different version of Visual Studio, run `cmake --help` and it will output all the options.

If it hasn’t been fixed yet, there may then be an error complaining about `_l` in line 61 of a `sw.cpp` file in sw’s storage directory. Go there and change that to `_slib`, as Egor Pugin describes in [this issue](https://github.com/tesseract-ocr/tesseract/issues/2882).

After that is done, you should have a `tesseract.sln` in your build directory.

Switch from Debug to Release, then Build → Build Solution.

If it is successful, the executables will be in `./bin/Release`.

-----

<small>[27 July note: I went through the process again today and the following issue has since been fixed. It looks to have been fixed in [this commit](https://github.com/DanBloomberg/leptonica/commit/e5df6259aa8c38fabb84802067a1caddd05282f3) from 26 Apr 2020, so I am not sure why I was having it a year later; maybe sw was using old version of leptonica for some reason. I am leaving this here for posterity.]</small>

Building in Visual Studio in `/TC` mode you may get errors because of some files in leptonica’s source declaring arrays with variable sizes, things like `char filename[L_BUFF_SIZE]`, is not supported (see [Compiler Error C2057](https://docs.microsoft.com/en-us/cpp/error-messages/compiler-errors-1/compiler-error-c2057)). You can get this compiling by using `/TP` mode instead (C++ mode) (see [Specify Source Type](https://docs.microsoft.com/en-us/cpp/build/reference/tc-tp-tc-tp-specify-source-file-type?view=msvc-160)). Setting that via the interface: in the Solution Explorer select libtesseract, right click → Properties, `C/C++ → Advanced → Compile As`, set to `Compile as C++ Code (/TP)`. Alternatively, the example above can be rewritten `char *filename = _alloca(L_BUFF_SIZE)`, but there are declarations like that all over the code that you’d have to replace.

-----

## See also

- [tessdoc/Compiling](https://tesseract-ocr.github.io/tessdoc/Compiling): Sadly very light on information at the moment, but it could lead you in the right direction.
- [Livezingy’s guide](https://livezingy.com/compilation-tesseract4-in-vs2017-win10/) (Chinese): Things were a bit different back then, and they encountered different problems, but this was very helpful to me nontheless.
- [My Tesseract 4 training article]({% post_url 2021-04-29-training-tesseract %})

{% include fin.html %}
