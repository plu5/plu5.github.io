---
layout: post
title:  "Backwards-compatible builds with GitHub Actions"
date:   2022-06-02
modified_date: 2023-05-27 19:31
categories: ci compilation github
---

Notes on automating backwards-compatible software builds with GitHub actions for all major operating systems.

## Linux

Two approaches for backwards-compatibility:

1. build with static linking; or
2. build on the oldest version of the operating system you want to support.

What makes builds on newer Linux operating systems fail to work on older ones is typically [libc](https://en.wikipedia.org/wiki/C_standard_library#Implementations); the “de-facto system library in almost all Linux operating systems”.[^libc-quote] Because of backwards compatibility, building against an older libc will work with a new one. Hence approach 2; building against the oldest libc you can.

Statically linking libc is discouraged,[^static-libc-discouraged] so I will focus on approach 2. If you want to explore static linking, you should look at [musl libc](https://musl.libc.org/). If you are building a Python application, see also [the section on StaticX](#staticx).

GitHub Actions runners only have the few most recent versions of Ubuntu, but that shouldn’t stop you, because [you can use a Docker image](#using-container).

The following subsections outline several ways to accomplish approach 2 for your builds using GitHub Actions CI.

### Using container

```yml
name: Build

on:
  workflow_dispatch:

jobs:
  linux-build:
    name: Linux
    runs-on: ubuntu-latest  # 1
    container:
      image: ubuntu:16.04   # 2
      options: --user 0     # 3
    steps:
      - name: Check version
        run: |
          grep VERSION= /etc/os-release  # -> VERSION="16.04.7 LTS (Xenial Xerus)"
          ldd --version                  # -> ldd (Ubuntu GLIBC 2.23-0ubuntu11.3) 2.23
```

1. You can only run jobs in a container on Linux runners.[^container-linux-runners]
2. **`image: ubuntu:16.04`** specifies the image to use. `ubuntu` is the name of [this image](https://hub.docker.com/_/ubuntu) on Docker Hub, and `16.04` is the tag, giving us Ubuntu version 16.04 (a.k.a Xenial). The name can be either the name and tag of an image on Docker Hub, or a link to an image on a registry (e.g. `ghcr.io/owner/image`[^ghdocs-container])[^private-registry]. If you want to use a local image, instead of using container you can run the image yourself (see the section [Using `docker run`](#using-docker-run)), or [use a self-hosted runner](https://docs.github.com/en/actions/hosting-your-own-runners/managing-self-hosted-runners/customizing-the-containers-used-by-jobs).
3. **`--user 0`** logs you in as root, thus letting you run commands that require privileges (that would normally require running with `sudo`[^sudo]).

Downsides:

- Many GitHub Marketplace actions don’t work with containers
- Can’t use a local image ([unless using a self-hosted runner](https://docs.github.com/en/actions/hosting-your-own-runners/managing-self-hosted-runners/customizing-the-containers-used-by-jobs))

Relevant page in GitHub docs: [Running jobs in a container](https://docs.github.com/en/actions/using-jobs/running-jobs-in-a-container).

### Using `docker run`

Behind the scenes, when using container GitHub Actions uses Docker commands. Ubuntu and Windows GitHub-hosted runners come with Docker preinstalled[^preinstalled-software] so it is almost equally simple to use Docker commands directly instead of relying on the container feature, and can give more flexibility.

```yml
name: Build

on:
  workflow_dispatch:

jobs:
  linux-build:
    name: Linux
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3  # 1

      - name: Run image & check version
        run: |                     # 2-6:
          CMDS="grep VERSION= /etc/os-release
                ldd --version"

          docker run \
            -v ${% raw %}{{ github.workspace }}{% endraw %}:/usr/src/app \
            --entrypoint=sh \
            ubuntu:16.04 \
            -c "${CMDS//$'\n'/;}"
```

1. Checking out makes our repository contents available in `${% raw %}{{ github.workspace }}{% endraw %}`. Mount this path on the image to be able to access it (see 3).
2. If your image is on Docker Hub no need to `docker pull` first, because `docker run` will pull if you are trying to run a Docker Hub image that is not available locally. Otherwise you can have a `docker pull` step, or steps building your image before this one (see [Building your own image](#building-your-own-image)).
3. **`-v ${% raw %}{{ github.workspace }}{% endraw %}:/usr/src/app`:** Mounting the folder with our repository contents in `/usr/src/app` on the image. This can be any path, even if the folders don’t exist yet.
4. **`--entrypoint=sh`:** This argument specifies what executable to run when the container starts. In this example it runs some shell commands to check the OS version and ldd version (see `CMDS`). `--entrypoint=bash` would work as well.
5. **`ubuntu:16.04`:** Like in the [Using container](#using-container) example, I specified [this Ubuntu image](https://hub.docker.com/_/ubuntu), with tag 16.04.
6. **`-c "${CMDS//$'\n'/;}"`:** Passing more parameters to the entrypoint. The fancy `"${CMDS//$'\n'/;}"` is just convenience to allow us to write the commands to run over multiple lines, but in order to pass them here they need to be in one line separated by semicolons `;`. The same thing could have been passed like `-c "grep VERSION= /etc/os-release; ldd --version"` instead.

See [Docker run reference](https://docs.docker.com/engine/reference/run/).

There are also GitHub Actions you can use to simplify your workflow. With [docker-run-action](https://github.com/addnab/docker-run-action) you can do the same thing with less code on your end:

```yml
name: Build

on:
  workflow_dispatch:

jobs:
  linux-build:
    name: Linux
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run image & check version
        uses: addnab/docker-run-action@v3
        with:
          image: ubuntu:16.04
          options: -v ${% raw %}{{ github.workspace }}{% endraw %}:/usr/src/app
          run: |
            grep VERSION= /etc/os-release
            ldd --version
```

The major downside compared to [using container](#using-container) is you can only run commands on the image in a single step rather than the whole job.
[TODO: is that definitely true and there is no way to continue the same docker instance with commands? that is what i am researching at the moment] [2023-05-29 18:20 might want to look at general docker videos, maybe i will find something where via cli you can do this? like docker run but the next docker run continuing where the previous one left off]

### Next
Use shell commands on the image to install or build your dependencies, then build your application.

### Caching
caching

alternatively could use a dockerfile that installs all your dependencies and build and cache or upload that image. see the section [building your own image](#building-your-own-image).

### Building your own image

building your own docker image with all your reqs already in it

## macOS

Out of all the operating systems covered in this article, I would say macOS is the biggest pain to build backwards-compatible binaries for. Often will involve either compiling all of your dependencies yourself from source, or using older versions of them that supported the oldest version of the operating system you want to support.

Support for old versions of the operating system is cut much earlier than for Linux or Windows, as end-of-life is reached within only 3 years.[^macos-endoflife]

Unlike [Linux](#linux), you cannot run Docker images of earlier versions of this operating system; in fact, you cannot run Docker images of macOS at all.

The good news is you don’t necessarily need an old version of the operating system to make backwards-compatible builds for macOS; it is possible to make backwards-compatible builds using the `macos-latest` runner. Unfortuately there are no definite steps to achieve that that I could give you, as it depends on, well, your dependencies; if you want your application to be compatible with a given version of macOS, you need to ensure they are all built to support this version too.

**If you can get pre-built binaries for your dependencies that are either built for your target macOS version or lower, this will save you a lot of effort.**

Python, for example, provides [binaries on python.org](https://www.python.org/downloads) that are compatible back to macOS 10.9 (2013; well past its end of life). If you get your Python binaries elsewhere, though, such as brew or via [the official setup-python GitHub action](https://github.com/actions/setup-python), this will not be the case.

If you are building a Python application, it is not just your Python that needs to be backwards-compatible to your target macOS version, but also all the libraries you are using.

One way to work out the lowest version of the operating system a binary supports could be its filename. For example, [PyQt5 5.15.9](https://pypi.org/project/PyQt5/5.15.9/) macOS wheel is called 
`PyQt5-5.15.9-cp37-abi3-macosx_10_13_x86_64.whl`, 10.13—so it is not compatible with 10.9—whereas [5.15.0](https://pypi.org/project/PyQt5/5.15.0/) is `PyQt5-5.15.0-5.15.0-cp35.cp36.cp37.cp38-abi3-macosx_10_6_intel.whl`, so it is.

**Failing that, you will need to build your dependencies yourself.**

`MACOSX_DEPLOYMENT_TARGET: 10.9`

you build for a particular version by configuring a setting

deployment target

architectures

LSBackgroundOnly (2022-11-27 00:59)

## Windows

On Windows, you can build on the latest version of the operating system applications that are backwards compatible to previous versions, so happily there is no need here to keep an old machine / VM around, and you can build on `windows-latest`.

Forwards compatibility: Builds from old systems continue to work on new systems for the most part. The user might have to install some runtime that doesn’t come bundled with their version of the operating system to be able to run it.

[^ 2024-11-06 10:35 it's true for win32 api but what about COM stuff, WinRT, .NET? are they all, all versions, forwards compatible without needing anything extra? i did mention the runtime thing so that bit is covered. i read something about .net 3.5 being more compatible than 4 and on so some people sticking to it, is that something i need to worry about to do with this? just don't want to be overly positive and wrong and thus pissing people off who do have issues]

[^ 2024-12-03 09:14 and what about visual c++ redistributables? when are they needed / which versions? i mean i don't necessarily need to write about this, but i know nothing about them and didn't realise they were needed even, i thought it was a .net thing]

i came across some apis that have been deprecated in windows 11, contradicting what i said that applications are forwards compatible https://learn.microsoft.com/en-gb/windows/win32/api/winuser/nf-winuser-setwindowshookexa
in most cases forwards compatible with at worst the user having to install runtime that doesn't get included with the OS anymore, for example to run VB5 (or was it VB6?) applications in win 11 you have to install the runtime, whereas with windows 10 and some versions prior it was included. same is true with some .net versions.
however there are cases of apis getting deprecated, usually for security reasons. they stub it (do they?) so it will still run but naturally whatever you were using it for will not work as intended.

[link to the colin fincke article where he talks about .net compatibility]

2024-11-14 19:47 example of an application made in windows 2000 that broke on newer versions https://www.viksoe.dk/code/asciidesktop.htm
«It appears that in Windows 10, the tool is no longer working correctly.»

2024-11-15 20:52 and a few binaries built with mfc i have not been able to run either. e.g. https://www.viksoe.dk/code/game1.htm
but possibly would be fine if i get MSVCRTD.dll and MFC42D.DLL which it complains about

2024-11-15 21:06 also his vista photo gallery https://www.viksoe.dk/code/photothing.htm doesn't work correctly on win11; don't see text in top menu, and don't see any images. and no "glass" at the bottom. but i guess like he correctly predicted «I would hesitate to actually add glass to a production application, as I don't think the glass-look will last many generations in Windows.»

Backwards compatibility: If your application does not use the Windows API directly, then you will have to look at which operating systems the libraries you are using that do use it support back to. There may be older versions that support back farther.

For example, version 6 of the popular UI framework Qt does not support Windows 7 or 8,[^qt6-supported-platforms] while version 5 latest (5.15) does,[^qt5-supported-platforms] but does not support Windows XP,[^qt5-min-platforms] for which you would likely need to go down to 5.5.[^qt5-xp-note]

If your application does use the Windows API directly then

- header thing -- don't know what that does and if it is required
- can have code trying to load a header and if it fails do something else in order to be able to support both

I don’t know of a reliable resource to check the requirements for each part of the API. Microsoft Learn documentation ostensibly has a requirements table at the bottom of each page, but I have noticed it is wildly incorrect, and most pages just say Windows Vista even for things that I know were in place long before. Trackbar, for instance.[^win32-trackbar]

I expect this would require some legwork; your own research for each part of the API you want to use that you suspect was introduced more recently, and testing on the target platform.

A major cutoff point is Windows Vista. [or is it. seems just the docs have vista as minimum supported client everywhere erroneously]
common controls

can you theoretically build application that will work on NT? am i right thinking that is the limit as Windows 1.0 to 3.0 were a separate operating system? (i heard they were just a DOS program)
can you build applications that will work on 16-bit windows?

show colin finck's technique for conditionally loading a thing if it is supported
this is called dynamic loading

2024-09-21 13:19 i think only win32 api is very backwards compatible, other frameworks are very much not https://colinfinck.de/posts/writing-win32-apps-like-its-2020-part-1/
so if you are using them or the library you are using uses them then it will only be as backwards-compatible as that.
are there functions / other things in win32 api that if you use them it will break backwards-compatibility bc they were introduced after x version? i believe i have seen things like that on the docs but don't know what would happen if you'd use them or if there is a list / guidelines for what to avoid to retain backwards-compatibility
-- yes there are some things, see the third part of his article. he uses some things that are only defined in windows 8+ and shows how to make it conditional so it still works on 7 and xp. he targets xp.

2024-09-21 16:29 also not sure if targetver.h SDKDDKVer thing is necessary. need to research if you don't have that and only use windows.h header if the executable is then backwards-compatible or not

2024-10-07 14:42 if you use visual styles https://learn.microsoft.com/en-us/windows/win32/controls/cookbook-overview#making-your-application-compatible-with-earlier-versions-of-windows

## Python-specific considerations

As mentioned on [macOS section](#macos), if you are building Python applications for macOS the Python you are using needs to have been built to be compatible with your target system. That is also true for each one of your dependencies.

show example workflow for getting macos python from python.org, and another one just like that one but with caching the file downloaded

### StaticX

only works with onefile builds (libraries packed into executable)
doesn’t work if library files packed into the executable have rpath/runpath set
at least that’s the case at the moment of writing
have trouble getting it to work with anything but the simplest application [TODO test if i can get simplest application to work]

## Addendum 1: GitHub Actions pitfalls

- **Working directory:** Every step resets working directory, so if you do `cd` in one step, do not expect to be in the new location in the next step. The default pwd is the top level of your checked-out repo. You can set a default working directory for a step or job with [`defaults.run.working-directory`](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#defaultsrun).
- **Run a step even if previous one failed:** Add `if: success() || failure()` to it. I find this useful when testing things in the runner.

2022-11-29 18:25 problems with on tag and on release workflow triggers (breaking some actions, breaking cache), and general info about github ref

> The fully-formed ref of the branch or tag that triggered the workflow run. For workflows triggered by `push`, this is the branch or tag ref that was pushed. For workflows triggered by `pull_request`, this is the pull request merge branch. For workflows triggered by `release`, this is the release tag created. For other triggers, this is the branch or tag ref that triggered the workflow run. This is only set if a branch or tag is available for the event type. The ref given is fully-formed, meaning that for branches the format is `refs/heads/<branch_name>`, for pull requests it is `refs/pull/<pr_number>/merge`, and for tags it is `refs/tags/<tag_name>`. For example, `refs/heads/feature-branch-1`.
https://docs.github.com/en/actions/learn-github-actions/contexts#github-context

2022-11-29 22:56 mention boolean thing, see log of today
2022-12-03 13:01 quote the 'true' / 'false' in checks, see log of today
2024-04-12 12:21 this is only the case for outputs, see today's log. inputs used directly are actual booleans and so when you check them don't use the quotes around 'true'/'false'. but all outputs are strings so you do need the quotes in that case.

2022-11-30 12:02 all the github actions vars you can access with `${{ }}` https://docs.github.com/en/actions/learn-github-actions/contexts
2024-04-12 12:22 ^ i.e. from a script inside run. in if statements and other yml things you don't need the `${{ }}`
2024-04-12 12:46 actually it is needed in some places in the yml too, like outputs... see log of just now. it seems to me github actions workflow syntax in general is really inconsistent and a lot of little issues like that so you really have to test it or you are certainly going to err / it will do something unexpected. so many little idiosyncracies.

2022-11-30 12:07 To echo [JohT](https://joht.github.io/johtizen/build/2022/01/20/github-actions-push-into-repository.html) "Be prepared that it will likely need a couple of attempts to get a GitHub Actions Workflow to work as intended.". [TODO: actually just put in a quote block what he said and his recommendations, enabling debug logs etc] i strongly recommend using a repository specifically for testing github actions stuff rather than testing on your actual repository. you can access almost everything from another repository, you can check out code from there, ... so it is a good idea to test everything on your test repo and only the final modification to make it work on same repo on your real repo
you can build your other project in the test repo, as you can checkout code from another respository.
2024-04-14 17:51 you could do it in a branch, but i recommend separate repo because that way you can test releases and things like that without accidentally affecting your real repo
and you can check out code from wherever you need anyway.

## Addendum 2: Workflow optimisation

things i may want to link

- https://aschmelyun.com/blog/using-docker-run-inside-of-github-actions/
- https://github.com/addnab/docker-run-action
- https://github.com/docker/build-push-action [remember to explain context]
- https://mmeendez8.github.io/2021/04/23/cache-docker.html
- https://josh-ops.com/posts/github-container-jobs/
- [Distribution Considerations for Linux](https://gregoryszorc.com/docs/pyoxidizer/main/pyoxidizer_distributing_linux.html)
- [Distribution Considerations for macOS](https://gregoryszorc.com/docs/pyoxidizer/main/pyoxidizer_distributing_macos.html)
- [Distribution Considerations for Windows](https://gregoryszorc.com/docs/pyoxidizer/main/pyoxidizer_distributing_windows.html)

[^static-libc-discouraged]: [StackOverflow: Why is statically linking glibc discouraged?](https://stackoverflow.com/questions/57476533/why-is-statically-linking-glibc-discouraged)
[^libc-quote]: [James Archives: Running new applications on old glibc](https://www.lightofdawn.org/wiki/wiki.cgi/NewAppsOnOldGlibc)
[^macos-endoflife]: [endoflife.date: Apple macOS](https://endoflife.date/macos)
[^sudo]: `sudo` does not work out of the box in Docker containers. [There are ways to set it up](https://stackoverflow.com/questions/25845538/how-to-use-sudo-inside-a-docker-container), but running as root user is what is usually done instead.
[^ghdocs-container]: [GitHub Docs: Running jobs in a container](https://docs.github.com/en/actions/using-jobs/running-jobs-in-a-container)
[^container-linux-runners]: I haven’t been able to find a source for this in the docs, but if I try to use container with a macOS or Windows runner I get `Error: Container operations are only supported on Linux runners`.
[^private-registry]: [Since September 2020](https://github.blog/changelog/2020-09-24-github-actions-private-registry-support-for-job-and-service-containers/), using an image on a private registry is also supported.
[^preinstalled-software]: [GitHub Docs: Using GitHub-hosted runners: Preinstalled software](https://docs.github.com/en/actions/using-github-hosted-runners/about-github-hosted-runners#preinstalled-software)
[^qt6-supported-platforms]: [Qt.io: Development hosts and targets in Qt 6.0](https://www.qt.io/blog/qt6-development-hosts-and-targets)
[^qt5-supported-platforms]: [Qt.io: Qt 5.15 Supported Platforms](https://doc.qt.io/qt-5/supported-platforms.html)
[^qt5-min-platforms]: [Internet Archive 2019 snapshot of Qt Wiki PlatformSupport page](https://web.archive.org/web/20190927112325/https://wiki.qt.io/PlatformSupport)
[^qt5-xp-note]: I have not tried it, but [apparently 5.6 and 5.7 work too](https://forum.qt.io/topic/101054/mingw-on-xp), and others have got newer versions to work by compiling themselves.
[^win32-trackbar]: [Microsoft Learn: WM_HSCROLL (Trackbar) notification code](https://learn.microsoft.com/en-us/windows/win32/controls/wm-hscroll--trackbar-). At the time of writing, the minimum supported client is listed as Windows Vista. [Here](https://web.archive.org/web/20061011064722/https://chgi.developpez.com/windows/trackbar/) is a 2006 Internet Archive snapshot of an article demonstrating the usage of trackbar and this very notification code in Windows XP.

{% include fin.html %}
