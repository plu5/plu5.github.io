---
layout: post
title: 9 — Hiding windows for optimisation and prefetching mpv
date: 2026-02-25 09:22
modified_date: 2026-04-10 21:05
categories: dotfiles mpv bilibili bspwm cachage gaplessmpv queuedmpv
lang: en
redirect_from: /devlog/9
---

Beware, there be hacks here.

## Idle browser CPU usage
Today, or rather in the middle of the night, I wondered why idle bilibili.com tabs use so much CPU. With a single bilibili tab open in Firefox, not playing anything, just idling, my global CPU usage goes from 1-2% to to 11-15%.

Network requests? There are not *that* many, 4 every 30 seconds.

Service workers? There aren't any.

Video rendering? Even if I delete the video element or prevent it from ever loading I see 11%+ CPU usage.

about:processes "Profile all threads for this process for 5 seconds" button reports:
- 13 RefreshDriver ticks
- 13 setInternal handler
- 11 setTimeout handler
- 1 XPVWrappedJS method call
- 1 Minor GC

and you can see method calls in the twisties but they don't mean anything to me and I don't know how to interpret the results.

### Switch tab or hide
I found that if you switch to another tab on the same window the CPU usage reduces significantly, which I suppose is due to Firefox throttling timers of inactive tabs.

I use a script to play/pause which needs to find the window, and simply unfocusing the window or switching to another workspace does not activate Firefox's throttling. but what does is hiding the window:
```sh
wmctrl -r 哔哩哔哩 -b add,hidden
```
you can restore it with:
```sh
wmctrl -r 哔哩哔哩 -b remove,hidden
```

so I changed my toggle pause script to unhide the window before sending pause/unpause, then hide it again.
```sh
if xdotool search --name 哔哩哔哩; then
    tmp=$(xdotool getactivewindow);
    wmctrl -r 哔哩哔哩 -b remove,hidden
    xdotool windowactivate --sync $(xdotool search --name 哔哩哔哩) key --clearmodifiers space &&
        xdotool windowactivate $tmp;
    wmctrl -r 哔哩哔哩 -b add,hidden
fi
```
and now global CPU usage is down to 2-3%. and in about:processes bilibili falls below WordReference and Google Docs in CPU usage (while all idle), so it seems like an effective workaround.

Of course, if you needed to actually use the site to watch or navigate, you have to show it. Maybe with something like Picture-in-Picture you could watch while the site is hidden to benefit from the reduced CPU usage also when watching?

## bspwm window hiding shenanigans
### Hide particular window when not in current desktop
and to avoid having to unhide the window manually when we need to use the site, automatically show it when switching workspaces to it and hide it again when switching back
```sh
bspc subscribe desktop_focus | while read -r _ _ d; do
    wmctrl -r 哔哩哔哩 -b add,hidden
    for w in $(bspc query -N -n .window -d "$d"); do
        case $(xprop -id "$w" WM_NAME) in
            *哔哩哔哩*) bspc node "$w" --flag hidden=off; break;;
        esac
    done
done
```
Some explanations:
- by "workspace" all this time I actually meant "desktop" (the proper term)
- the `desktop_focus` event variables are: event name, monitor id, desktop id. `man bspc` has an events section close to the end with a list of all events and what they give.
- the reason I'm using wmctrl to add hidden and bspc to remove it is just convenience (to use bspc for both we'd have to add more code to find the window titles of nodes in previous desktop)
- bspc query syntax: `-N` give us nodes, `-n` filter by node selector, `-d` filter by desktop selector.
- `-n .window` in the query because `bspc query -N` gives some nodes that are not windows ([related /r/bspwm discussion from 2017](https://www.reddit.com/r/bspwm/comments/6or5r0/bspc_query_output_and_bspc_format/))
- in sh you must use `case` to do glob matching, it doesn't work in an `if` (but in bash with `[[ ]]` it does).
- note that `xprop -id "$w" WM_NAME` results are like `WM_NAME(COMPOUND_TEXT) = "哔哩哔哩 (゜-゜)つロ 干杯~-bilibili — Mozilla Firefox"`, not just the name

Having that running all the time just for that is not ideal. Maybe it would be better to expand it to a wider optimisation to hide windows in other workspaces? Is there some reason this is a bad idea?

{% include note.html content='
> [!NOTE]
> Yes, as I find out later. It\'s annoying with window focus. There is also flicker.
' %}

The bspc man page I am suddenly finding useful, I remember when I had started using bspwm I found it incredibly cryptic. It *is* quite cryptic.

### Restore all hidden windows
You may need this if messing about with scripts like the above and hiding the wrong windows
```sh
for n in $(bspc query -N -n .hidden); do bspc node "$n" -g hidden; done
```

Unhide on current desktop:
```sh
bspc node any.local.hidden -g hidden
```
can be useful to open a terminal (which most people that use bspwm do with super+enter because sxhkd comes with this keyboard shortcut) and run this in a given workspace to restore what was in it. It's also easier to remember in an emergency.

(note that `.local` in some contexts can mean current monitor, but in this context it means current desktop)

[why doesn't this work with `.window.local.hidden`?]
[maybe because `any` is necessary before. `any` "Selects the first node that matches the given selectors." There is also `first_ancestor`, `last`, `newest`, `older`, `newer`, `focused`, `pointed`, `biggest`, `smallest` (they are called node descriptors, whereas `window`, `local`, `hidden` are modifiers)]

[how come this unhides more than one window?]

Let's do a little test. Open in a workspace several terminals and enter in each one:
```sh
bspc node -g hidden
```
Then open another and:
```sh
bspc node any.hidden -g hidden
```
Nothing.
```sh
bspc node any.local.hidden -g hidden
```
All the terminals return.

I'm confused.

### Hide windows on other desktops
```sh
bspc subscribe desktop_focus | while read -r _ _ d; do
    for w in $(bspc query -N -n .hidden.window -d "$d"); do
        bspc node "$w" --flag hidden=off
    done
    for w in $(bspc query -N -n '.!hidden.window.!local'); do
        bspc node "$w" --flag hidden=on
    done
done
```
Unhide windows in current desktop, hide unhidden windows in other desktops. Doing it in this order reduces the flickering.

Note that we need quotes around the `!` or bash tries to interpret it. Before I understood why it doesn't work, I wrote this instead:
```sh
# Example of what not to do, but I guess it's kind of an interesting use of
# the POSIX shell :+ substitution which you don't see very often (gives the
# value after the :+ if the variable is defined and not empty)
flagnodes() { # arg1: flag, arg2 (optional): desktop to target
    bspc query -N ${2:+-d "$2"} | while read -r n; do
        bspc node "$n" --flag "$1"
    done
}
prev=""
bspc subscribe desktop_focus | while read -r _ _ cur; do
    if [ -z "$prev" ]; then
        flagnodes hidden=on
        flagnodes hidden=off "$cur"
    else
        flagnodes hidden=off "$cur"
        flagnodes hidden=on "$prev"
    fi
    prev="$cur"
done
```

It's annoying to actually use because the focused node when you come back to the workspace changes. so I guess it's better to hide only things that can benefit from it.

while testing various such functions my Xserver decided to shut down, and this is the nth time that I enter things quickly into urxvt then find myself kicked out to the vt. No crashdump, and "terminated successfully". I wonder if I am pressing something by accident. but Ctrl + Alt + Backspace doesn't do anything.

### Hide certain windows on other workspaces
```sh
bspc subscribe desktop_focus | while read -r _ _ d; do
    for w in $(bspc query -N -n .hidden.window -d "$d"); do
        bspc node "$w" --flag hidden=off
    done
    for w in $(bspc query -N -n '.!hidden.window.!local'); do
        case "$(xprop -id "$w" WM_CLASS)" in
            *firefox*|*chromium*) bspc node "$w" --flag hidden=on;;
        esac
    done
done
```
Gets me to 1% CPU usage despite 4 browser windows and hundreds of tabs open, and reduces the annoyance of focus issues because it affects less windows, but it's still an issue in all desktops where you have a browser window as not the only window.

### Trying to work around focus issues
Looking at this [2021 reddit thread](https://www.reddit.com/r/bspwm/comments/nlz5x5/toggling_fullscreen_changes_the_window_focus/) we can use xargs instead of for loops.

I thought about doing the following (make it so that the active window on each desktop is hidden last, so that it's not recalculated to being a different window active while we're doing the hiding):
```sh
bspc subscribe desktop_focus | while read -r; do
    bspc query -N -n '.local.window.hidden' | xargs -I{} bspc node {} -g hidden
    bspc query -N -n '.!local.window.!hidden.!active' | xargs -I{} bspc node {} -g hidden
    bspc query -N -n '.!local.window.!hidden.active' | xargs -I{} bspc node {} -g hidden
done
```
but active is not working as expected. It's not selecting the right nodes, so they don't get hidden. I expected it to give "nodes that are the focused node of their desktop" like man bspc indicates.
```sh
$ bspc query -N -n '.window.!hidden.!active' | xargs -I{} xprop -id {} WM_CLASS
$ bspc query -N -n '.window.!hidden.active' | xargs -I{} xprop -id {} WM_CLASS
WM_CLASS(STRING) = "urxvt", "URxvt"
$ bspc query -N -n '.window.!hidden' |
> xargs -I{} xprop -id {} WM_CLASS
WM_CLASS(STRING) = "emacs", "Emacs"
WM_CLASS(STRING) = "emacs", "Emacs"
WM_CLASS(STRING) = "urxvt", "URxvt"
WM_CLASS(STRING) = "mpvk", "mpv"
WM_CLASS(STRING) = "Navigator", "firefox"
WM_CLASS(STRING) = "Navigator", "firefox"
WM_CLASS(STRING) = "mpvk", "mpv"
WM_CLASS(STRING) = "mpvk", "mpv"
WM_CLASS(STRING) = "urxvt", "URxvt"
WM_CLASS(STRING) = "urxvt", "URxvt"
WM_CLASS(STRING) = "Navigator", "firefox"
WM_CLASS(STRING) = "urxvt", "URxvt"
$ bspc query -N -n '.window.active'
0x02400009
$ bspc query -N -n '.window.focused'
0x02400009
$ bspc query -N -n '.focused'
0x02400009
$ bspc query -N -n '.active'
0x02400009
```
Welp, I'm confused.

so this:
```sh
bspc subscribe desktop_focus | while read -r; do
    bspc query -N -n '.local.window.hidden' | xargs -I{} bspc node {} -g hidden
    bspc query -N -n '.!local.window.!hidden' | xargs -I{} bspc node {} -g hidden
done
```
same as initial issue but now it's simpler at least(?).

What if we hide in reverse order?
```sh
bspc subscribe desktop_focus | while read -r; do
    bspc query -N -n '.local.window.hidden' | xargs -I{} bspc node {} -g hidden
    # added tac                                  v
    bspc query -N -n '.!local.window.!hidden' | tac | xargs -I{} bspc node {} -g hidden
done
```
I don't notice any difference.

Can `bspc node` maybe take multiple nodes? ... No:
```sh
$ bspc node $(bspc query -N -n '.local.window') -g hidden
node: Unknown command: '0x01600547'.
$ bspc node "$(bspc query -N -n '.local.window')" -g hidden
node: Invalid descriptor found in '0x01600140
0x01600547
0x02400009'.
```

Keeping flags on the focused windows?

Maybe one of these functionalities could be useful:
```
 [!](hidden|sticky|private|locked|marked|urgent)
     Only consider windows that have the
     given flag set.

 [!](below|normal|above)
     Only consider windows in the given
     layer.
```
```sh
$ bspc query -N -n '.window.above'
```
nothing, maybe the layers can only be used relative to another.

[I think this is just for z order anyway which doesn't necessarily mean focus]

What are all the flags for? From man bspc:
- hidden : Is hidden and doesn't occupy any tiling space.
- sticky : Stays in the focused desktop of its monitor.
- private : Tries to keep the same tiling position/size.
  + avoids splitting that node
- locked : Ignores the node `--close` message.
  + prevents closing with super+w
- marked : Is marked (useful for deferring actions). A marked node becomes unmarked after being sent on a preselected node.
  + [hellmouthxyz](https://www.reddit.com/r/bspwm/comments/161o8zq/): "While the other flags will change how bwpsm treats a node, marked doesn't actually mean anything. [..] It's just an arbitrary flag that can be used for your own purposes if you want to."
- urgent : Has its urgency hint set. This flag is set externally.
  + set by certain applications, e.g. Discord when you are pinged (I think. It is the only one I could not test because I don't use any application that does it)

or to avoid interfering with any user thing we could maybe set our own 'flag'. Deleted user in [the same thread](https://www.reddit.com/r/bspwm/comments/nlz5x5/toggling_fullscreen_changes_the_window_focus/) I had already linked above:

> when hiding, set a property on each window to mark it (you can use xprop or atomx from wmutils for this.)

[I added the close parenthesis myself because he had forgot it, and we can't not have the close parenthesis!]

but the problem is when we get `desktop_focus` we are already on the new desktop, and if `active` doesn't work, how to get the active window in the other desktops (or even just previous desktop)?

`.active` only seems to work for current desktop:
```sh
$ bspc query -D | xargs -I{} sh -c "echo --d:{}; bspc query -N -n .active -d {}"
--d:0x00400004
0x02400009
--d:0x00400005
--d:0x00400006
--d:0x00400007
--d:0x00400008
--d:0x00400009
--d:0x0040000A
--d:0x0040000B
--d:0x0040000C
--d:0x0040000D
```

On a window there is no information about whether it is focused:
```sh
$ bspc query -T -n
    {"id":37748745,"splitType":"vertical","splitRatio":0.520000,"vacant":false,"hidden":false,"sticky":false,"private":false,"locked":false,"marked":false,"presel":null,"rectangle":{"x":685,"y":382,"width":595,"height":338},"constraints":{"min_width":32,"min_height":32},"firstChild":null,"secondChild":null,"client":{"className":"URxvt","instanceName":"urxvt","borderWidth":0,"state":"tiled","lastState":"tiled","layer":"normal","lastLayer":"normal","urgent":false,"shown":true,"tiledRectangle":{"x":685,"y":382,"width":595,"height":338},"floatingRectangle":{"x":200,"y":60,"width":880,"height":600}}}
```
The closest thing is `shown` but if there are several shown per desktop we are still not going to be able to return focus to the right one.

I'm stuck. Can't find a way other than listening also to `node_focus` the entire time and trying to keep track.

I guess this optimisation to hide windows not currently visible would be better built in to the window manager itself.

I wrote it anyway and it is simpler than I thought it would be (but requires bash or another shell that supports associative arrays):
```bash
declare -A f
bspc subscribe desktop_focus node_focus | while read -r event _ d n; do
    if [ "$event" = "desktop_focus" ]; then
        bspc query -N -n '.local.window.hidden' | xargs -I{} bspc node {} -g hidden
        bspc query -N -n '.!local.window.!hidden' | xargs -I{} bspc node {} -g hidden
        [ -n "${f[$d]}" ] && bspc node "${f[$d]}" -f
    else
        f["$d"]="$n"
    fi
done
```
Remarks:
1. all the events give the name of the event as the first parameter
2. `desktop_focus` parameters 2 and 3 monitor id and desktop id
3. `node_focus` parameters 2, 3, 4 are monitor id, desktop id, and node id
4. given postulations 2 and 3, taking four parameters in the read for the two events we will get monitor id (`_`), desktop id (`d`), and in `n` nothing if the event is `desktop_focus` and node id if the event is `node_focus`.

More readable and explicit version, sans subshells:
```bash
declare -A focus

while read -r event _ desktop node; do
    case "$event" in
        desktop_focus)
            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=off
            done < <(bspc query -N -n '.local.window.hidden')

            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=on
            done < <(bspc query -N -n '.!local.window.!hidden')

            [ -n "${focus[$desktop]}" ] && bspc node "${focus[$desktop]}" -f
            ;;
        node_focus)
            focus["$desktop"]="$node"
            ;;
    esac
done < <(bspc subscribe desktop_focus node_focus)
```

There are still problems.

(1) if you close the window that was focused the next one that gets focused is not what you had previously.

(2) Chromium not getting proper focus sometimes. The right node is focused but the application acts as if it's not with the way it colours the interface and keyboard keys don't work. I checked and in cases where it works and cases where it doesn't the node focused (including all the query -T information both for the node in question and the entire desktop tree) is exactly the same. `_NET_ACTIVE_WINDOW` is correct.

- If it's on a desktop on its own it doesn't happen.
- If it's on a desktop with 2 windows it only happens if it's the second window and only on monocle mode.
- If it's on a desktop with 3 windows it only happens if it's the second or third window and only on monocle mode.
- Can't seem to replicate it now but I had a situation with 3 windows side by side and Chromium in the middle (and urxvt either side) where it happened even outside of monocle mode, and even if the window that you had focused is not Chromium itself but the window on the left of it, changed desktop, then changed to chromium with super+l. but if you then changed back with super+h and back to Chromium with super+l then it focused properly. This was reproductible consistently when I had that layout.

I am puzzled by this issue.

[bspwm issue 811](https://github.com/baskerville/bspwm/issues/811) suggests there are maybe issues with Chromium focus and desktop switching? (for them it's the opposite problem, could be unrelated, but suggests something is different about Chromium vs Firefox and other applications handling of focus)

{% include note.html content='
> [!NOTE]
> Stupid workaround is to always put Chromium on the left :-p  
> `bspc rule -a Chromium split_dir=west`
' %}

### Problems if you already hide windows explicitly and don't want them unhidden when you switch desktops
The hide unhide scripts would interfere with people who already use hide for a scratchpad, unwantingly unhiding windows that were hidden explicitly when switching back to a workspace that has them. maybe use another flag in addition? while at it implement hide window but indeed how to make it stick?

We could place a flag on the windows we hide and only unhide the windows we ourselves have hidden.

To use an existing flag, like marked, it's as simple as changing the first query to `.local.window.hidden.marked` and doing `-g marked=on` on each node that we hide:
```bash
declare -A focus
while read -r event _ desktop node; do
    case "$event" in
        desktop_focus)
            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=off
            done < <(bspc query -N -n '.local.window.hidden.marked')
            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=on
                [ -n "$wid" ] && bspc node "$wid" -g marked=on
            done < <(bspc query -N -n '.!local.window.!hidden')
            [ -n "${focus[$desktop]}" ] && bspc node "${focus[$desktop]}" -f
            ;;
        node_focus)
            focus["$desktop"]="$node"
            ;;
    esac
done < <(bspc subscribe desktop_focus node_focus)
```
but this could interfere if you already use marked for something else.

Let's try then with a custom property instead.
```sh
$ wid=$(bspc query -N -n)

# Set a string property HIDE with value "t"
# (see man xprop for possible formats)
$ xprop -id "$wid" -f HIDE 8s -set HIDE t
# Get property
$ xprop -id "$wid" HIDE; echo $?
HIDE(STRING) = "t"
0
# Unfortunately exit value doesn't tell us if the property exists
$ xprop -id "$wid" NOT; echo $?
NOT:  no such atom on any window.
0

# Workaround using grep on the list of values
$ xprop -id "$wid" | grep -q '^HIDE('; echo $?
0
$ xprop -id "$wid" | grep -q '^NOT('; echo $?
1

# Maybe it's more optimised to ask for only the particular property
$ xprop -id "$wid" HIDE | grep -q '='; echo $?
0
$ xprop -id "$wid" NOT | grep -q '='; echo $?
1

# Remove the property
$ xprop -id "$wid" -remove HIDE
$ xprop -id "$wid" HIDE | grep -q '='; echo $?
1
```

Adaptation of the previous script to use a custom property:
```bash
declare -A focus
while read -r event _ desktop node; do
    case "$event" in
        desktop_focus)
            while read -r wid; do
                xprop -id "$wid" HIDE | grep -q '=' &&
                    bspc node "$wid" -g hidden=off
                xprop -id "$wid" -remove HIDE
            done < <(bspc query -N -n '.local.window.hidden')
            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=on
                [ -n "$wid" ] && xprop -id "$wid" -f HIDE 8s -set HIDE t
            done < <(bspc query -N -n '.!local.window.!hidden')
            [ -n "${focus[$desktop]}" ] && bspc node "${focus[$desktop]}" -f
            ;;
        node_focus)
            focus["$desktop"]="$node"
            ;;
    esac
done < <(bspc subscribe desktop_focus node_focus)
```

A little test:
- launch a terminal (super+enter)
- bspc query -N -n
- ^ copy the result, e.g. 0x02C00009
- bspc node 0x02C00009 -g hidden
- the terminal should now be hidden
- switch to another desktop
- switch back
- if the terminal you hid is still hidden, it worked
- bspc node 0x02C00009 -g hidden

Note that between running each of the hiding-all-the-windows-in-other-desktop scripts, like if you ran one of them and want to test another, you need to first unhide all the windows as the ones on other desktops will still be hidden after you exit the script.
```sh
for n in $(bspc query -N -n .hidden); do bspc node "$n" --flag hidden=off; done
```

Another way other than setting a custom property on the window itself would be to keep the window IDs that we have hidden in an array or in a file.

A solution with an array (`hidden`):
```bash
declare -A focus
declare -a hidden
while read -r event _ desktop node; do
    case "$event" in
        desktop_focus)
            while read -r wid; do
                printf "%s\n" "${hidden[@]}" | grep -q "^${wid}$" &&
                    bspc node "$wid" -g hidden=off  # unhide if in hidden
                hidden=("${hidden[@]/$wid}")  # remove from hidden
            done < <(bspc query -N -n '.local.window.hidden')
            while read -r wid; do
                [ -n "$wid" ] && bspc node "$wid" -g hidden=on  # hide
                [ -n "$wid" ] && hidden+=("$wid")  # add to hidden
            done < <(bspc query -N -n '.!local.window.!hidden')
            [ -n "${focus[$desktop]}" ] && bspc node "${focus[$desktop]}" -f
            ;;
        node_focus)
            focus["$desktop"]="$node"
            ;;
    esac
done < <(bspc subscribe desktop_focus node_focus)
```

{% include note.html content='
> [!NOTE]
> After left running for a long time it lags when refocusing previous node. We probably need to clear the array at some point.
' %}

[`cachage` version control link where I will put future changes](https://github.com/plu5/dotfiles/blob/main/pm/scripts/cachage)

## Use mpv instead
Another option is to not use the site. with `mpv` and `yt-dlp` installed, you can open mpv and simply paste a video link. but this is slow.

Playlists work too but you need the option:
```sh
ytdl-raw-options=yes-playlist=
```
either passed to the cli with `--` before it, or placed in `~/.config/mpv/mpv.conf`. Otherwise it loads just the first/current video.

Problems with slowness. Loading the first and every subsequent video has significant delay, and if letting it keep playing without pausing I suffer from bad buffering also.

It also doesn't seem to work with bilibili P (videos with multiple parts).

{% include note.html content='
> [!NOTE]
> Actually it does work, just only if you give the URL to the main video not to a particular P; https://www.bilibili.com/video/BV1QDCFB8Eoj rather than https://www.bilibili.com/video/BV1QDCFB8Eoj/?p=25.
' %}

### Format
To help with the buffering issue, I tried `ytdl-format=worst`. This works with YouTube but not bilibili
```sh
Requested format is not available. Use --list-formats for a list of available formats
[ytdl_hook] youtube-dl failed: unexpected error occurred
Failed to recognize file format.
```
available formats:
```
 ID    EXT RESOLUTION FPS │   FILESIZE  TBR PROTO │ VCODEC       VBR ACODEC     ABR
 ──────────────────────────────────────────────────────────────────────────────────
 30232 m4a audio only     │ ≈249.21KiB  60k https │ audio only       mp4a.40.2  60k
 30216 m4a audio only     │ ≈272.08KiB  66k https │ audio only       mp4a.40.2  66k
 30280 m4a audio only     │ ≈395.19KiB  96k https │ audio only       mp4a.40.2  96k
 30016 mp4 360x640     30 │ ≈207.27KiB  50k https │ avc1.640033  50k video only
 30032 mp4 480x852     30 │ ≈305.32KiB  74k https │ avc1.640033  74k video only
 30064 mp4 720x1280    30 │ ≈602.23KiB 146k https │ avc1.640033 146k video only
 30080 mp4 1080x1920   30 │ ≈  1.52MiB 377k https │ avc1.640033 377k video only
```

I saw [here](https://hacktivis.me/git/dotfiles/commit/53d181cccfa27c53100aa17af52987d7f94c2bb8.html) that you can do fallbacks with slash and in that way have a format that works for both bilibili and YouTube.

```sh
ytdl-format=30016+30232/best[height<=384]/bestaudio
```

{% include note.html content='
> [!NOTE]
> It\'s unreliable, sometimes no format is found even if I add `/best/worst` as fallback [because as established, that doesn\'t work with bilibili]. I thought to get rid of `ytdl-format` in the config altogether and let mpv/ytdl select the format automatically, but even that sometimes fails. I changed it to `ytdl-format=30016+30232/best[height<=384]/30032+30232/30064+30216/30080+30280/best/worst` to fall back to every possible format (though I suppose this is not every possible combination), as ridiculous as that is it fails in less situations, but sometimes it fails too, inconsistently (trying the same video again with same settings sometimes works and other times not). This may be not related to the format but server issues or getting rate-limited.
' %}

### Volume
Increase volume beyond what the keybinds/UI will let you:
1. grave to open console
2. `set volume 250`

(this is percentage, but do not input %, just the number)

### Reverse playlist
In [mpv issue 8228](https://github.com/mpv-player/mpv/issues/8228) stacyharper shares [a script](https://git.sr.ht/~stacyharper/dotfiles/tree/1828bdea3db4607d297ea7d219f4894a6a6dbb0c/item/.config/mpv/scripts/reverse-playlist.lua) to reverse a playlist upon pressing <kbd>Ctrl-e</kbd>.
```lua
local msg = require 'mp.msg'

local settings = {
	reverse_key_binding = "Ctrl+e",
}

function reverse_playlist()
	msg.debug("reverse_playlist")
	local playlist_count = mp.get_property_number("playlist/count")
	for i = 0,playlist_count,1
	do
		mp.commandv("playlist-move", 0, playlist_count-i)
	end
	mp.osd_message("Playlist reversed", 1)
end

mp.add_key_binding(settings.reverse_key_binding, "reverse_playlist", reverse_playlist)
```
This goes in ~/.config/mpv/scripts/reverse-playlist.lua

It will subsequently load when you launch mpv.

If you have an already-running instance, you can load a script with `load-script` followed by the path to the script in the console (grave).
```sh
load-script ~/.config/mpv/scripts/reverse-playlist.lua
```

Reversing is instant.

### Go to index in playlist
You can go to a particular video in the playlist with:
```sh
playlist-play-index 473
```
Note that it is 0-based; to go to video 474, input index 473.

### Prefetching
[mpv issue 6437](https://github.com/mpv-player/mpv/issues/6437) has related discussion on prefetching videos.
- ZaxonXP wrote [scripts](https://github.com/ZaxonXP/youtube-viewer-scripts) to do it by using two mpv instances and alternating between them. (I saw that in the linked issue, [7436](https://github.com/mpv-player/mpv/issues/7436))
- gabreek wrote [a fork of mpv-handler](https://github.com/gabreek/mpv-handler-queue) (a Rust program for mpv ytdl browser integration) to add "enqueue" feature to be able to use mpv's `prefetch-playlist=yes` option with yt-dlp.

#### Trying to get gabreek's thing to work (failed)
{% include note.html content='
> [!NOTE]
> I could not get it to work properly. My attempts are nevertheless documented below.
' %}

Building gabreek's fork (on Linux, 2026-03-09, [c7148fd](https://github.com/gabreek/mpv-handler-queue/commit/c7148fd)):
```sh
git clone https://github.com/gabreek/mpv-handler-queue.git
cd mpv-handler-queue
cargo build --release
cp target/release/mpv-handler ~/.local/bin/
cp share/linux/mpv-handler.desktop ~/.local/share/applications/
cp share/linux/mpv-handler-debug.desktop ~/.local/share/applications/
chmod +x ~/.local/bin/mpv-handler
xdg-mime default mpv-handler.desktop x-scheme-handler/mpv
xdg-mime default mpv-handler-debug.desktop x-scheme-handler/mpv-debug
```
Check if ~/.local/bin is in PATH:
```sh
echo $PATH
```
If not, add to "~/.bashrc, ~/.zshrc, or equivalent shell configuration file" (I personally have it in ~/.bash_profile, but it's not going to update until next login):
```sh
export PATH="$HOME/.local/bin:$PATH"
```
or
```sh
export PATH=$PATH:~/.local/bin
```
Alternatively, copy the mpv-handler executable to another location that's already in your PATH.

Add to ~/.config/mpv.conf:
```sh
input-ipc-server=/tmp/mpvsocket
prefetch-playlist=yes
cache=yes
demuxer-readahead-secs=300
demuxer-max-bytes=500M
```

You need to have the extension GreaseMonkey or one of its forks (I use ViolentMonkey) installed in your browser, and install [gabreek's userscript "Play with MPV (Enhanced)"](https://greasyfork.org/en/scripts/542145-play-with-mpv-enhanced/). Afterwards on a video page on the domains youtube.com, twitch.tv, crunchyroll.com, bilibili.com, kick.com, and vimeo.com, an mpv button appears at the bottom left. When hovered over, you can see that it links to mpv://play/[bash64hash]/?enqueue=true, and a cogwheel button appears above it that opens the settings panel. _Enqueue Mode_ is on by default. _Run With Console_ could be activated to make the links use mpv-debug:// instead of mpv://, which will make the handler run "with console window to view outputs and errors".

If clicking the mpv button does nothing, it could be because the handler is not in PATH. In that case, either:
- if you set the PATH in bashrc, restart the browser
- if you set the PATH in bash profile, restart the computer or relogin
- if you can't be arsed to do either, copy mpv-handler to another location that was already on your PATH before, and ensure the copy has execution permission (chmod +x).

but I can't get it to work properly.

with an already opened instance it says that it enqueues but no change in the open instance. I expect it to add to the playlist. I tried researching if queue means something else in mpv in case it's a different concept than playlist, and no. Both for single video and a playlist. Both for bilibili and youtube.
```sh
Using yt-dlp path: yt-dlp
Connected to existing mpv socket: /tmp/mpvsocket
Checking for mpv.conf at: /home/pm/.config/mpv/mpv.conf
Found ytdl-format in mpv.conf: 30016+30232/best[height<=384]/bestaudio
Enqueuing to existing mpv instance.
Enqueuing item [1]: [url] - [url]
Fetching direct URL for: [url]
Extracted Title: [title]
Extracted Video URL: [url]
Extracted Audio URL: [url]
Enqueued: [title]
```

with no open instance, it also says that it enqueues but doesn't seem do anything, doesn't open mpv, unless you pick "play only the first video" but it's no different then than just opening it yourself. It's roughly the same output as above, except "No existing mpv socket found or connection failed. Launching new instance." instead of "Connected to existing mpv socket", and these errors here and there:
```sh
[ipc_0] Command loadfile: argument index has incompatible type.
```

It actually just shuts the terminal immediately so I had to hack it to stay open to see the output. I will write about how in a little tangent section below.

also the fact that this solution only works with stuff launched with the handler makes it inflexible. You'd hope for a solution in mpv proper that just starts to cache the next video before the end of the current one so that you don't have to wait a million years between them.

It's also slow to launch proportional to the size of the playlist; has to resolve all the URLs before launching mpv instead of doing it in the background. Unless the enqueue thing to an existing instance is able to do it without you having to wait. I can't get it to add even a single video to an existing instance, so I can't say. Even without an existing instance (its own instance that it launches) I could only get it to work with a single video.

```sh
$ mpv --version
mpv v0.41.0 Copyright © 2000-2025 mpv/MPlayer/mplayer2 projects
 built on Feb 11 2026 22:07:06
libplacebo version: v7.360.0
FFmpeg version: n8.0.1
FFmpeg library versions:
   libavcodec      62.11.100
   libavdevice     62.1.100
   libavfilter     11.4.100
   libavformat     62.3.100
   libavutil       60.8.100
   libswresample   6.1.100
   libswscale      9.1.100
$ yt-dlp --version
2026.02.21
```

{% include note.html content='
> [!NOTE]
> I end up finding the source of the problem and fixing it in the section [Patching mpv-handler-queue](#patching-mpv-handler-queue). I have also sent [a PR](https://github.com/gabreek/mpv-handler-queue/pull/1) so hopefully it will be fixed by the time you are reading this.
' %}

#### Tangent: Hackily prevent Konsole launched by other processes from closing
```sh
$ which konsole
/usr/bin/konsole
$ sudo mv /usr/bin/konsole /usr/bin/konsole-orig
$ printf '#!/usr/bin/env sh\n/usr/bin/konsole-orig --noclose "$@"' | sudo tee /usr/bin/konsole
```

{% include note.html content='
> [!NOTE]
> Caution not to create an infinite loop here. The first time around I absent-mindedly called /usr/bin/konsole from /usr/bin/konsole and I could not recover, 99% CPU 99% RAM until Xserver crashed.
' %}

I actually use urxvt but it seems Konsole is set as the default terminal, which I won't fix, because even though I prefer urxvt, it has issues. Konsole more reliably does what you expect, as well as much better unicode support and even bidi, but at a cost of higher resource use and time to launch, so urxvt is still my choice for my own use.

#### Trying ZaxonXP's approach
Well... sometimes the simplest solution is best, but is this going to be stupid with video?

Let's try it first the way he intended it, with a hardcoded list of youtube videos played audio-only.

##### Proof of concept
Slightly simplified version of his gapless play bash script (removed the xterm monitor and added explanatory comments):
```bash
# Takes as an argument a file that contains lines in the format:
#   mm:ss | yt-id | artist | title
# where yt-id is the hash after the ?v= in YouTube urls, e.g.
# https://www.youtube.com/watch?v=zADVzO7m5qM
#   blah | zADVzO7m5qM | blah | blah
# It's irrelevant what you have in the other fields, because only the
# second one (yt-id) is used in this script.

# Files used as input-ipc-server sockets for each instance
soc1=/tmp/soc1
soc2=/tmp/soc2
# File that stores which of these ^ is currently used. is otherwise
# not used in this script, but he uses it in other scripts to be able
# to communicate with the currently running mpv instance, in
# gapless_play_pause.sh for example to be able to play/pause
curr=/tmp/soc_used

touch $soc1 $soc2

# Print stdin in green or cyan based on the value of $1 (i).
# Used to render the output of each instance in alternating
# colours, each line prefixed with $2 followed by a tab.
prefix_echo() {
  if [[ "$(($1 % 2))" == "0" ]]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

# --pause or nothing based on the value of $1 (i). The initial
# instance (i=0) does not need to be paused, but the following ones do
# as they get launched while the previous one is still running.
get_pause() {
    if [[ $1 == 0 ]]; then
        echo ""
    else
        echo "--pause"
    fi
}

# Called like: get_soc $i $soc1 $soc2
# Returns soc1 or soc2 based on the value of $1 (i), alternating.
get_soc() {
    if [[ $(( $1 % 2 )) == 1 ]]; then
        echo $3
    else 
        echo $2
    fi
}

# Initialising i
i=0

# Iterating each line and each field in the inputfile
cat "$1" | while IFS=\| read -r f1 f2 f3 f4
do
    # id is the second field, without the spaces around it
    id=${f2% }
    id=${id# }

    socket=$(get_soc $i $soc1 $soc2)
    pause=$(get_pause $i)

    # Start mpv instance with the youtube video according to id, and
    # socket as input ipc server.
    # --term-playing-msg = "Print out a string after starting playback."
    # It's me that added the --quiet.
    # Piping to prefix_echo is used to prepend mpv output with video
    # id and give the alternating instances alternating colours.
    mpv --quiet --term-playing-msg="ID:${id}" --input-ipc-server="$socket" --no-video $pause "https://www.youtube.com/watch?v=$id" | prefix_echo $i "$id" &
    # Save PID in associative array. This is only used to be able to
    # access the PID of the previous instance.
    pids[$i]=$!

    # If not initial instance
    if [[ $i > 0 ]]; then
        # Get PID of previous instance
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}

        # Wait for previous instance to exit
        tail --pid=$prev_pid -f /dev/null
        # Unpause new instance
        echo '{ "command": ["set_property", "pause", false] }' | socat - $socket
    fi
    # Update /tmp/soc_used with current socket
    echo $socket > $curr

    i=$((i + 1))
done
```

You need to have socat installed for messaging the socket. The command is similar to examples given in man mpv.

Sample input (excerpt of Zaxon's [Example_list.txt](https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/youtube-viewer-get-lists/Example_list.txt)) (intentionally picked short videos to make testing easier):
```
 blah | zADVzO7m5qM | blah | blah
   37 | 3-3gIAkYEX8 | F1 Explained: The Halo
 1:49 | w3_jG_7RmR0 | 5 Crazy Onboards | Italian Grand Prix
 1:23 | b7JKtMhVnzE | The Shortest Career in F1 History? | Marco Apicella, 1993 Italian Grand Prix
 1:36 | 3Bb4vh2urZo | Top 5 Formula 2 Moments | 2018 Belgian Grand Prix
 1:55 | s4MWU9YZlSU | Top 5 Moments | 2018 Belgian Grand Prix
 1:45 | fuRRLkc4qUo | 2018 Belgian Grand Prix |  First-Turn Crash - All The Angles
 2:22 | z7ia7FR4XQs | Lewis Hamilton's Pole Lap | 2018 Belgian Grand Prix
 1:54 | FI6KXJfQfWo | 2018 Belgian Grand Prix: FP3 Highlights
   34 | wFwMZgx0dn4 | F1 2018 - Out Now!
 1:51 | q3SR7ezGLmQ | Spa's 'Confusing' Corner Names | 2018 Belgian Grand Prix
 1:16 | NsdDRdbvpFs | F1's Most Expensive Crash Ever...? | 1998 Belgian Grand Prix
 2:36 | YWDNQJhKPG0 | Stoffel Vandoorne's Virtual Hot Lap of Spa | Belgian Grand Prix
   53 | nU5HG14_1Sk | Vettel Crashes Out At Hockenheim | 2018 German Grand Prix
 1:36 | fZi44FQD2nc | Lewis Hamilton Hits Trouble In Qualifying (360 video) | 2018 German Grand Prix
```

I find it actually quite pleasant.

##### How to control the instances
We have no control over the mpv instances, any command has to be passed onto the socket. He solves it with a keyboard shortcut to his script `gapless_play_pause.sh`:
```sh
if [ "$(echo '{ "command": ["get_property", "core-idle"] }' | socat - $(cat /tmp/soc_used))" == '{"data":false,"error":"success"}' ]; then
    echo '{ "command": ["set_property", "pause", true] }' | socat - $(cat /tmp/soc_used) > /dev/null
else
    echo '{ "command": ["set_property", "pause", false] }' | socat - $(cat /tmp/soc_used) > /dev/null
fi
```

but you wouldn't be able to do any finer control without writing scripts to send ipc commands for each one.

What if if we ran it without `--novideo`?

Then we have full control, but both instances are visible, and when a new one opens it takes focus. It should be trivial to hide the one that's waiting.

##### Hide the waiting instance
My first idea is to use the mpv property window-minimized, but it doesn't work for me, maybe because I use the window manager bspwm. You can test in the mpv console (grave) `set window-minimized yes`, or run mpv with `--window-minimized=yes`. I would have changed the if inside the loop to:
```bash
    if [[ $i > 0 ]]; then
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}

        echo '{ "command": ["set_property", "window-minimized", true] }' | socat - $socket
        tail --pid=$prev_pid -f /dev/null
        echo '{ "command": ["set_property", "pause", false] }' | socat - $socket
        echo '{ "command": ["set_property", "window-minimized", false] }' | socat - $socket
    fi
```
(I think this would not work in any case because the first ipc command happens too early, see below)

From [mpv issue 10918](https://github.com/mpv-player/mpv/issues/10918) I learned that they have added a property window-id, so we should be able to get it with:
```bash
echo '{ "command": ["get_property", "window-id"] }' | socat - $socket
```
but there is a problem of timing. I think doing this after launching the mpv instance is too early:
```sh
socat[93341] E GOPEN: /tmp/soc2: Connection refused
```
It works if I put it after the tail, not before. but that's too late, we need the window id right after launching the instance, ideally before the window even shows.

The next idea is:
```sh
xdotool search --sync --pid ${pids[$i]}
```
that I saw mr.spuratic do in his answer on [this SE question](https://unix.stackexchange.com/questions/479788/how-to-set-custom-property-with-xprop-and-open-that-program-in-one-line).

Unfortunately this never returns, because the PID of the mpv process we launch is not the same as on the windows. I got 102396 in `pids[1]` then checked with xprop `_NET_WM_PID` of both windows and they were 102391 and 102395. Here I guess we could fudge it as it seems to be 1 less:
```sh
wid=$(xdotool search --sync --pid $((${pids[$i]} - 1)))
wmctrl -ir $wid -b add,hidden
```
That does actually work but the window momentarily appears. bspwm doesn't even rearrange the tree after so there is a visible gap until I do something that forces it to recalculate (like going in and out of monocle).

{% include note.html content='
> [!NOTE]
> I think the reason the PID is off by one is the pipe to `prefix_echo`.
' %}

It's a bspwm-only solution but I guess we could create a oneshot rule just before launching mpv.
```sh
[[ $i > 0 ]] && bspc rule -a mpv -o hidden=on
```
Timing problem again. It's the first video that launches hidden instead of the second one, because it takes time until it opens.

While this horrible hack does work:
```sh
    if [[ $i > 0 ]]; then
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}
        prev_pid_minus1=$(($prev_pid - 1))
        wid=$(xdotool search --sync --pid $prev_pid_minus1)
        bspc rule -a mpv -o hidden=on
    fi
```
there has to be a better way.

mpv [can take argument `--x11-name`](https://www.reddit.com/r/qutebrowser/comments/adhypn/mpv_change_mpv_window_title/) which sets the instance name in `WM_CLASS`.

We could modify `get_pause` so that other than `--pause` it will also pass `--x11-name mpvalt` for instances other than the first one:
```sh
get_pause() {
    if [[ $1 == 0 ]]; then
        echo ""
    else
        echo "--pause --x11-name=mpvalt"
    fi
}
```
and add a oneshot rule at the end of each loop (as we only need it from the second iteration and on):
```sh
bspc rule -a mpv:mpvalt -o hidden=on
```

Since only the windows we want to hide get the class instance applied, there is no point doing a oneshot rule every time, we could just create a normal rule at the start of the script if it doesn't already exists (and might as well check also that bspc exists):
```sh
if command -v bspc >/dev/null 2>&1 &&
        ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi
```

If you want to remove it:
```sh
bspc rule -r mpv:mpvalt:*
```
and you can list existing rules with:
```sh
bspc rule -l
```
and yes, you can have many identical rules, it doesn't squash them, hence why the check that it does not already exists.

to unhide it we can do this before/after unpausing the alternate instance:
```sh
wmctrl -xr mpvalt -b remove,hidden
```
(`-x` makes it search in classes instead of titles)

but more problems with bspwm not recalculating the tree unless we force it. and also if for example we moved the video to another workspace the other instance is not going to know about it. Ideally we want to replace the window exactly in the position it had. This is possible with bspwm but is it going to be hacky getting the wid? I guess not as we can use ask for the window-id property via ipc if we need it just right before unhiding the alternate instance, not after opening a new instance. but no, because when the script comes back from the tail that pid is already dead, and the window we want to replace is already closed.

I found that using bspc to toggle hidden instead of wmctrl makes bspwm update properly at least:
```sh
bspc node $(wmctrl -lx | grep mpvalt | cut -d' ' -f1) -g hidden
```

{% include note.html content='
> [!NOTE]
> This is fragile and will break if you have several instances. Should change this to get the proper window.
' %}

Before I make changes to the input, here's the script:
```bash
#!/usr/bin/env bash
# Gapless mpv
# Modified from ZaxonXP's gapless_play
# https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play.sh
#
# Dependencies:
# - mpv
# - yt-dlp
# - wmctrl
# - socat
# - (Optional) bspwm window manager
#
# Usage:
# gaplessmpv inputfile
#
# input file format:
#   mm:ss | yt-id | artist | title
# (or anything so long as YouTube video id is in the second field)

has_bspc=$(command -v bspc >/dev/null 2>&1 && echo 1)

if [ $has_bspc ] && ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi

soc1=/tmp/soc1
soc2=/tmp/soc2

touch $soc1 $soc2

prefix_echo() {
  if [[ "$(($1 % 2))" == "0" ]]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

get_pause() {
    if [[ $1 == 0 ]]; then
        echo ""
    else
        echo "--pause --x11-name=mpvalt"
    fi
}

get_soc() {
    if [[ $(( $1 % 2 )) == 1 ]]; then
        echo $3
    else 
        echo $2
    fi
}

unhide_alt() {
    if [ $has_bspc ]; then
        bspc node $(wmctrl -lx | grep mpvalt | cut -d' ' -f1) -g hidden=off
    else
        wmctrl -xr mpvalt -b remove,hidden
    fi
}

i=0

cat "$1" | while IFS=\| read -r f1 f2 f3 f4
do
    id=${f2% }
    id=${id# }

    socket=$(get_soc $i $soc1 $soc2)
    pause=$(get_pause $i)

    mpv --quiet --term-playing-msg="ID:${id}" --input-ipc-server="$socket" $pause "https://www.youtube.com/watch?v=$id" | prefix_echo $i "$id" &
    pids[$i]=$!
    if [[ $i > 0 ]]; then
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}

        tail --pid=$prev_pid -f /dev/null
        unhide_alt
        echo '{ "command": ["set_property", "pause", false] }' | socat - $socket
    fi

    i=$((i + 1))
done
```

##### Play more than just YouTube
It's simple to modify it to accept any video URL, not just YouTube IDs. To avoid having to make a test list ourselves, let's first look at how we can get a list of videos to feed to it with yt-dlp.

```sh
$ yt-dlp --no-warnings --print "%(webpage_url)s" https://www.youtube.com/@slowgermanlistening/videos
https://www.youtube.com/watch?v=VYeSyqPfYDE
https://www.youtube.com/watch?v=IVpA7-caZko
https://www.youtube.com/watch?v=eyDOVHy7TCY
https://www.youtube.com/watch?v=O1H69qIx25E
https://www.youtube.com/watch?v=UAZen83Vdh8
..
```
Other than `webpage_url`, yt-dlp has many other fields you can use in the format string, far too many for me to list here. Check man mpv `/available fields` <kbd>Enter</kbd>. Notably we can get `playlist_index` which could be useful later to be able to make a command to jump to a particular index.

This works with any url that normally works with yt-dlp. Playlists, channels, and even ytsearch (that I talk about in a section below; I wrote this devlog out of order, or more like put off prefetching until the end because thought it would be hard)
```sh
$ yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch5:hello
https://www.youtube.com/watch?v=YQHsXMglC9A
https://www.youtube.com/watch?v=mHONNcZbwDY
ERROR: [youtube] tVlcKp3bWH8: This video is not available
https://www.youtube.com/watch?v=fazMSCZg-mw
ERROR: [youtube] fN1Cyr0ZK9M: This video is not available
```
Is there a `--no-errors`? no, and neither `--ignore-errors` nor `--quiet` do the job. but maybe this is in stderr rather than stdout so won't be a problem for redirecting anyway.

Change the loop to take just a list of URLs:
```bash
cat "$1" | while read -r link; do
    socket=$(get_soc $i $soc1 $soc2)
    mpv --quiet --input-ipc-server="$socket" $(get_pause $i) "$link" | prefix_echo $i $i &
    pids[$i]=$!
    if [[ $i > 0 ]]; then
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}
        tail --pid=$prev_pid -f /dev/null
        unhide_alt
        echo '{ "command": ["set_property", "pause", false] }' | socat - $socket
    fi
    i=$((i + 1))
done
```

Now:
```sh
gaplessmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch5:hello)
```
It's just 3 videos because of the unavailable ones, but it otherwise works.

You might think if the playlist is long this is going to take a long time before it starts playing. In fact it takes the same amount of time regardless of the length of the playlist, because the command in the pseudofile happens in parallel.
```sh
gaplessmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" scsearch1000:fjellstrom)
```

Since I did a SoundCloud search here, the results are audio only. We can add to the mpv command `--force-window=yes` or `--force-window=immediate` to have a window to be able to still control it (reminder: otherwise with gaplessmpv we cannot control the player other than by sending IPC commands).

{% include note.html content='
> [!NOTE]
> For a list of all the search prefixes you can use with yt-dlp, see [the last section of this devlog](#yt-dlp-search-prefixes).
' %}

Instead of taking a file as input, it would be nice to take what we give to yt-dlp directly (scsearch1000:fjellstrom in this case). It's as simple as changing the main loop from:
```sh
cat "$1" | while read -r link; do
    # ..
done
```
to:
```sh
while read -r link; do
    # ..
done < <(yt-dlp --no-warnings --print "%(webpage_url)s" "$1")
```
Now we can simply do:
```sh
gaplessmpv scsearch1000:fjellstrom
```
or any other yt-dlp query.

but it causes broken pipe issue if we kill gaplessmpv before the yt-dlp finishes, because the yt-dlp continues in the background.
```sh
ERROR: [Errno 32] Broken pipe
ERROR: [Errno 32] Broken pipe
ERROR: [Errno 32] Broken pipe
ERROR: [Errno 32] Broken pipe
ERROR: [Errno 32] Broken pipe
..
```
This does not happen if we use a simple pipe instead of pseudofile:
```sh
yt-dlp --no-warnings --print "%(webpage_url)s" "$1" | while read -r link; do
    # ..
done
```
with this, if we quit in the middle yt-dlp gets killed as well.

{% include note.html content='
> [!NOTE]
> Another cool thing is the yt-dlp option `--playlist-random`. with this added to the above you get a different result each time. Given that we have made a search for 1000 items and there is not that much actual Fjellström (Marcus) on SoundCloud though, you most of the time get some random weird song by a hobbyist. It seems like it only randomises between the n first results so if you do scsearch1:fjellstrom it\'s still always the same song.
' %}

Script so far:
```bash
#!/usr/bin/env bash
# Gapless mpv
# Modified from ZaxonXP's gapless_play
# https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play.sh
#
# Dependencies:
# - mpv
# - yt-dlp
# - wmctrl
# - socat
# - (Optional) bspwm window manager
#
# Usage:
# gaplessmpv QUERY
# where QUERY is any link or search accepted by yt-dlp, for example:
# gaplessmpv scsearch1000:fjellstrom
# gaplessmpv youtube.com/@slowgermanlistening/videos
# gaplessmpv youtube.com/?v=zADVzO7m5qM

has_bspc=$(command -v bspc >/dev/null 2>&1 && echo 1)

if [ $has_bspc ] && ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi

soc1=/tmp/soc1
soc2=/tmp/soc2

touch $soc1 $soc2

prefix_echo() {
  if [[ "$(($1 % 2))" == "0" ]]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

get_pause() {
    if [[ $1 == 0 ]]; then
        echo ""
    else
        echo "--pause --x11-name=mpvalt"
    fi
}

get_soc() {
    if [[ $(( $1 % 2 )) == 1 ]]; then
        echo $3
    else 
        echo $2
    fi
}

unhide_alt() {
    if [ $has_bspc ]; then
        bspc node $(wmctrl -lx | grep mpvalt | cut -d' ' -f1) -g hidden=off
    else
        wmctrl -xr mpvalt -b remove,hidden
    fi
}

i=0

yt-dlp --no-warnings --print "%(webpage_url)s" "$1" | while read -r link; do
    socket=$(get_soc $i $soc1 $soc2)
    mpv --quiet --force-window=yes --input-ipc-server="$socket" $(get_pause $i) "$link" | prefix_echo $i $i &
    pids[$i]=$!
    if [[ $i > 0 ]]; then
        prev_idx=$(($i - 1))
        prev_pid=${pids[$prev_idx]}
        tail --pid=$prev_pid -f /dev/null
        unhide_alt
        echo '{ "command": ["set_property", "pause", false] }' | socat - $socket
    fi
    i=$((i + 1))
done
```

I noticed that on the last video of the playlist, or if we have only one video, the script detaches from the terminal (returns to prompt and no longer possible to quit with keyboard interrupt). This can be avoided if we wait for the last mpv in the same way that we wait before unhiding and unpausing our alternating instance: `tail --pid=$pid -f /dev/null`. While we're at it, instead of keeping an associative array for the PIDS and having to calculate the index for the previous PID, we could simply keep two variables, `pid` and `prev_pid`. Because our loop is in a subshell, we can't know the PID outside of the loop, so instead we detect when we're on the last link by looking ahead:
```sh
pid=
prev_pid=

store_pid() {
    prev_pid=$pid
    pid=$1
}

wait_alt() {
    tail --pid=$prev_pid -f /dev/null
    unhide_alt
    echo '{ "command": ["set_property", "pause", false] }' | socat - $1
}

yt-dlp --no-warnings --print "%(webpage_url)s" "$1" | {
    read -r link || exit
    while :; do
        socket=$(get_soc $i $soc1 $soc2)
        mpv --quiet --force-window=yes --input-ipc-server="$socket" $(get_pause $i) "$link" | prefix_echo $i $i &
        store_pid $!
        [[ $i > 0 ]] && wait_alt $socket
        if read -r next; then
            link="$next"
            i=$((i + 1))
        else
            echo "@@ last $link"
            tail --pid=$pid -f /dev/null
            break
        fi
    done
}
```
`read` necessarily consumes the line, so we use each lookahead as the link for the next iteration.

{% include note.html content='
> [!NOTE]
> We can\'t use `wait $pid`, because `wait` can only wait for direct children, not forks of forks nor processes launched in a subshell or pipeline.
' %}

##### Stay compatible with the original script
Refactoring next:
- With no more associative arrays, we no longer rely on any Bash-specific features and can modify the script to be POSIX-shell compatible (`[ ]` instead of `[[ ]]`).
- We could also trivially still maintain the ability for the user to supply a list of videos themselves: check with `-f` if the user passed in a file or not. To support pseudofiles also, test `-e` (exists) or `-r` (readable) instead.
- Why not also keep compatibility with the original script; if a file is provided and it does not contain links, check if it contains YouTube IDs
- Rename `get_pause` to `get_args`

```sh
#!/usr/bin/env sh
# Gapless mpv
# Modified from ZaxonXP's gapless_play
# https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play.sh
#
# Dependencies:
# - mpv
# - yt-dlp
# - wmctrl
# - socat
# - (Optional) bspwm window manager
#
# Usage:
# gaplessmpv (QUERY|INPUTFILE)
# QUERY = any link or search accepted by yt-dlp.
# * gaplessmpv scsearch1000:fjellstrom
# * gaplessmpv youtube.com/@slowgermanlistening/videos
# * gaplessmpv youtube.com/?v=zADVzO7m5qM
# INPUTFILE = a file where each line contains a link to a video or a yt-id.
# * gaplessmpv Example_list.txt
# * gaplessmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)

has_bspc=$(command -v bspc >/dev/null 2>&1 && echo 1)

if [ $has_bspc ] && ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi

soc1=/tmp/soc1
soc2=/tmp/soc2

touch $soc1 $soc2

prefix_echo() {
  if [ "$(($1 % 2))" = "0" ]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

get_args() {
    if [ "$1" = 0 ]; then
        echo "--quiet --force-window=yes"
    else
        echo "--quiet --force-window=yes --x11-name=mpvalt --pause"
    fi
}

get_soc() {
    if [ $(( $1 % 2 )) = 1 ]; then
        echo $3
    else
        echo $2
    fi
}

unhide_alt() {
    if [ $has_bspc ]; then
        bspc node $(wmctrl -lx | grep mpvalt | cut -d' ' -f1) -g hidden=off
    else
        wmctrl -xr mpvalt -b remove,hidden
    fi
}

i=0

pid=
prev_pid=

store_pid() {
    prev_pid=$pid
    pid=$1
}

wait_alt() {  # wait until prev exits, then unhide and unpause $1
    tail --pid=$prev_pid -f /dev/null
    unhide_alt
    echo '{ "command": ["set_property", "pause", false] }' | socat - $1
}

get_link() {  # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

process_lines() {
    read -r line || exit
    while :; do
        link=$(get_link "$line")
        soc=$(get_soc $i "$soc1" "$soc2")
        mpv --term-playing-msg="$link" --input-ipc-server="$soc" $(get_args $i) \
            "$link" |
            prefix_echo $i $i &
        store_pid $!
        [ $i -gt 0 ] && wait_alt $soc
        if read -r next; then
            line="$next"
            i=$((i + 1))
        else
            tail --pid=$pid -f /dev/null
            break
        fi
    done
}

if [ -r "$1" ]; then
    process_lines < "$1"
else
    yt-dlp --no-warnings --print "%(webpage_url)s" "$1" | process_lines
fi
```

##### Caching the yt-dlp lists
Next idea is caching the lists from yt-dlp queries in ~/.cache/gaplessmpv where each filename is a checksum of the query (to avoid illegal characters). This will add code and complication, not improve speed much (since fetching the list already happens in parallel) and there will be problems with the cache being stale and if there are several instances for the same query at the same time, so the script at its current state could be better than what comes next, but I still want to implement this to be to be able to continue where I left off in a playlist and to be able to jump to a particular video rather than starting from the beginning.

I'm just going to do a naïve solution without handling the possibility of multiple instances doing the same query. I'll leave it as an exercise to the reader to come up with something more robust.

1. Rather than trying to work out ourselves when the list is stale, let the user decide by specifying `--purgecache` option.
2. We need to not save the cache if the user quit before the list was fully fetched. We already have the mechanism in the loop to tell when we are on the last line [but this is not ideal because it means that the cache won't be saved if the user didn't go all the way until the last video, even if the whole list could have been fetched by the time they quit]. We only need to save cache for yt-dlp so `process_lines` can take an argument containing path to the cache file or nothing if no caching is necessary.
3. For position just save another file with the same md5 with .pos at the end. It's simple and can be implemented in a similar way to the list cache, and purged with `--purgepos`. That way user can also keep the list cache but purge the pos or vice versa.
4. If the options `--purgecache` or `--purgepos` are given with no query or inputfile after, don't purge (an alternative approach would be to purge all in that case).
5. For saving the pos, rather than capturing keyboard interrupt or something like that, I think it's better to save it each time it passes to the next video, so that way no matter how it's exited, the user's position in the list is preserved. To restore a position, remove the first n lines from the list, and pass the number as the argument to `process_lines` as well, because if the user started from the first video then `i` is the position, but if a position was restored we need to add the position that was restored to `i` in order to work out current position each iteration.
6. If input is a file, we get the absolute path first, so that it will save and restore the same position for the given file even if supplied via a different relative path.
7. Add help text too. There are usage notes already in the header comments but a user may be running the script without access to the file.
8. Simplification: It seems like there is no need to create soc1 and soc2 in advance. Also give them less generic names to avoid clashing with something else.

```sh
#!/usr/bin/env sh
# Gapless mpv
# Modified from ZaxonXP's gapless_play
# https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play.sh
#
# Dependencies:
# - mpv
# - yt-dlp
# - wmctrl
# - socat
# - (Optional) bspwm window manager
#
# Usage:
# gaplessmpv [--purgecache] [--purgepos] (QUERY|INPUTFILE)
# QUERY = any link or search accepted by yt-dlp.
# * gaplessmpv scsearch1000:fjellstrom
# * gaplessmpv youtube.com/@slowgermanlistening/videos
# * gaplessmpv youtube.com/?v=zADVzO7m5qM
# INPUTFILE = a file where each line contains a link to a video or a yt-id.
# * gaplessmpv Example_list.txt
# * gaplessmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)
# --purgecache = remove file that caches the video list for a given QUERY
# --purgepos = remove file that stores current position in the video list

NAME="${0##*/}"

help() {
    cat <<EOF
Usage: $NAME [options] (QUERY|INPUTFILE)

QUERY = any link or search accepted by yt-dlp.
INPUTFILE = a file where each line contains a link to a video or a yt-id.

Options:

-h | --help   Show this help text and exit.
--purgecache  Remove file that caches the video list for a given QUERY.
--purgepos    Remove file that stores current position in the video list.

Examples:
* $NAME scsearch1000:fjellstrom
* $NAME youtube.com/@slowgermanlistening/videos
* $NAME youtube.com/?v=zADVzO7m5qM
* $NAME Example_list.txt
* $NAME <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)
EOF
}

has_bspc=$(command -v bspc >/dev/null 2>&1 && echo 1)

if [ $has_bspc ] && ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi

prefix_echo() {
  if [ "$(("$1" % 2))" = "0" ]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

get_args() {
    if [ "$1" = 0 ]; then
        echo "--quiet --force-window=yes"
    else
        echo "--quiet --force-window=yes --x11-name=mpvalt --pause"
    fi
}

get_soc() {
    echo "/tmp/${NAME}_soc_$(( $1 % 2 ))"
}

unhide_alt() {
    if [ $has_bspc ]; then
        bspc node "$(wmctrl -lx | grep mpvalt | cut -d' ' -f1)" -g hidden=off
    else
        wmctrl -xr mpvalt -b remove,hidden
    fi
}

i=0

pid=
prev_pid=

store_pid() {
    prev_pid="$pid"
    pid="$1"
}

wait_alt() {       # wait until prev exits, then unhide and unpause $1
    tail --pid="$prev_pid" -f /dev/null
    unhide_alt
    echo '{ "command": ["set_property", "pause", false] }' | socat - "$1"
}

get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

store_pos() {                   # $1 = pos, $2 = pos_file
    if [ -n "$1" ] && [ -n "$2" ]; then
        echo "$1" > "$2"
        echo "$NAME: Stored position $1 in $2"
    fi
}

process_lines() {
    read -r line || exit
    while :; do
        link=$(get_link "$line")
        soc=$(get_soc $i)
        mpv --term-playing-msg="$link" --input-ipc-server="$soc" $(get_args $i) \
            "$link" |
            prefix_echo $i $i &
        store_pid $!
        [ $i -gt 0 ] && wait_alt "$soc"
        [ $i -gt 0 ] && store_pos "$(($i + "$1"))" "$2"
        if read -r next; then
            line="$next"
            i=$((i + 1))
        else  # last line
            echo "$NAME: Last video"
            [ -r "$3.tmp" ] && mv "$3.tmp" "$3"  # save list cache if needed
            tail --pid="$pid" -f /dev/null  # wait for last mpv process
            break
        fi
    done
}

CACHE_DIR="$HOME/.cache/$NAME"

get_cache_file() {  # $1 = query
    echo "$CACHE_DIR/$(echo -n "$1" | md5sum | cut -d' ' -f1).txt"
}

get_pos_file() {  # $1 = query
    cache_file=$(get_cache_file "$1")
    echo "${cache_file%.txt}.pos.txt"
}

purge_cache() {  # $1 = query
    cache_file=$(get_cache_file "$1")
    if [ -f "$cache_file" ]; then
        rm "$cache_file"
        echo "$NAME: Purged cache file $cache_file"
    fi
}

purge_pos() {  # $1 = query
    pos_file=$(get_pos_file "$1")
    if [ -f "$pos_file" ]; then
        rm "$pos_file"
        echo "$NAME: Purged pos file $pos_file"
    fi
}

purge_cache_flag=0
purge_pos_flag=0

while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            help
            exit
            ;;
        --purgecache)
            purge_cache_flag=1
            ;;
        --purgepos)
            purge_pos_flag=1
            ;;
        --)
            shift
            break
            ;;
        -*)
            echo "$NAME: Unknown option $1" >&2
            exit 1
            ;;
        *)
            break
            ;;
    esac
    shift
done

# by peterh https://stackoverflow.com/a/21188136/18396947
get_abs_filename() {
  filename=$1
  parentdir=$(dirname "${filename}")
  if [ -d "${filename}" ]; then
      echo "$(cd "${filename}" && pwd)"
  elif [ -d "${parentdir}" ]; then
    echo "$(cd "${parentdir}" && pwd)/$(basename "${filename}")"
  fi
}

query=$1
[ -z "$query" ] && { help; exit 1; }
[ -f "$query" ] && query=$(get_abs_filename "$query")

cache_file=$(get_cache_file "$query")
pos_file=$(get_pos_file "$query")

[ "$purge_cache_flag" = 1 ] && purge_cache "$query"
[ "$purge_pos_flag" = 1 ] && purge_pos "$query"

if [ -r "$query" ]; then        # INPUTFILE
    if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then
        pos="$(cat "$pos_file")"
        echo "$NAME: Restoring position $pos"
        cat "$query" | tail -n "+$(($pos + 1))" |
            process_lines $pos "$pos_file"
    else
        process_lines 0 "$pos_file" < "$query"
    fi
else                            # QUERY
    mkdir -p "$CACHE_DIR"
    if [ -r "$cache_file" ]; then
        echo "$NAME: Using cached list $cache_file"
        if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then
            pos="$(cat "$pos_file")"
            echo "$NAME: Restoring position $pos"
            cat "$cache_file" | tail -n "+$(($pos + 1))" |
                process_lines $pos "$pos_file"
        else
            process_lines 0 "$pos_file" < "$cache_file"
        fi
    else
        yt-dlp --no-warnings --print "%(webpage_url)s" "$query" |
            tee "$cache_file.tmp" |
            process_lines 0 "$pos_file" "$cache_file"
    fi
fi
```

I want to solve the problem I raised in 2, as in a situation the user is reading a QUERY with a long list of results (e.g. `scsearch1000:fjellstrom`), without going all the way to the end it won't be possible to restore the position the next time, because we won't have the list cached. To resolve this we need to separate out the `yt-dlp` command. I had wanted to avoid this to avoid situations where it keeps running in the background after quitting, but now it's necessary, and we're going to have to trap INT (keyboard interrupt) and TERM (when something does [soft] `kill` on our script) and kill it. We also need another fork to do the finalisation of the cache; we can't have them both in the same fork because we can't run yt-dlp from a subshell or we won't be able to access its PID from outside to be able to kill it.

(1) Set up the cleanup traps. This needs to be done before running anything.
```sh
ytdlp_pid=
cache_pid=

cleanup() {
    [ -n "$ytdlp_pid" ] && kill "$ytdlp_pid" 2>/dev/null
    [ -n "$cache_pid" ] && kill "$cache_pid" 2>/dev/null
    exit 130
}

trap cleanup INT TERM
```

(2) The yt-dlp fork. It needs to be line-buffered to be able to read it as it comes in (otherwise it will wait until a certain number of lines before writing to file). `stdbuf -oL` before the command does the job.
```sh
stdbuf -oL yt-dlp --no-warnings --print "%(webpage_url)s" "$query" \
       > "$cache_file.tmp" & # line-buffered cache
ytdlp_pid=$!
```

(3) The cache finalisation fork, waiting on the yt-dlp PID.
```sh
(
    tail --pid="$ytdlp_pid" -f /dev/null
    if [ -s "$cache_file.tmp" ]; then
        mv "$cache_file.tmp" "$cache_file"
        echo "$NAME: Finalised cache $cache_file"
    fi
) & # finalise cache when yt-dlp is done irrespective of read loop
cache_pid=$!
```

It would be better to check the exit code of yt-dlp before finalising cache, but can't unless I rewrite it to fork `process_lines` and do the waiting for yt-dlp in the foreground, and I don't want to do that.

(4) The read pipeline. `process_lines 0 "$pos_file" < "$cache_file.tmp"` doesn't work here (file not found). `tail -f` can read data as it comes in.
```sh
tail -f "$cache_file.tmp" | process_lines 0 "$pos_file"
```

##### yt-dlp -I
We could also implement restoring position when we don't have a cache maybe with some option of yt-dlp to start at a particular video?

from man yt-dlp:
```
 -I, --playlist-items ITEM_SPEC
        Comma-separated  playlist_index   of   the
        items  to  download.   You  can  specify a
        range using "[START]:[STOP][:STEP]".   For
        backward compatibility, START-STOP is also
        supported.   Use negative indices to count
        from the right and negative STEP to  down‐
        load   in   reverse   order.    E.g.   "-I
        1:3,7,-5::2" used on a playlist of size 15
        will   download   the   items   at   index
        1,2,3,7,11,13,15
```
which is a little obtuse, but further down in the list of "redundant options" there are examples:
```
 --playlist-start NUMBER          -I NUMBER:
 --playlist-end NUMBER            -I :NUMBER
 --playlist-reverse               -I ::-1
```
Add `--playlist-start` with the pos if we have a position saved:
```sh
ytdlp_args="--no-warnings"
if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then
    pos="$(cat "$pos_file")"
    ytdlp_args="$ytdlp_args --playlist-start $(($pos + 1))"
    echo "$NAME: Restoring position $pos"
fi
stdbuf -oL yt-dlp $ytdlp_args --print "%(webpage_url)s" "$query" \
       > "$cache_file.tmp" & # line-buffered cache
ytdlp_pid=$!
```
As usual we have to add 1 to it because it's 1-based whereas our position is 0-based (it's like that with `tail -n` too).

Don't forget that we have to pass `$pos` to `$process_lines` after too, in order for position saved after pos was restored to be calculated correctly. Define `pos=0` along with the `ytdlp_args="--no-warnings"` and then pass `$pos` to `$process_lines` instead of 0.

but hang on a minute... Is this going to produce a cache for the query that will always have the first n videos missing?

I guess in that case we have to change the cache file we save to.
```sh
cache_file="${cache_file%.txt}-start${pos}.txt"
```

##### Reverse list
I also want an option to reverse playlist, but how to handle pos? One way would be to work out how many videos there are total and subtract `i` from it to find the non-reversed pos, but we're going to have to pass flags around because saving and restoring position both need to know whether it is reversed or not. we also don't have an easy and robust way to tell the number of videos. So let's handle this in the same as as I handled the cache for non-cached queries with non-0 starting position above, by having a different pos file for the reversed case.

The cached list will be reversed, so we need to have a different cache file also.

- Add `--reverse` to usage comments and help text
- Add a reverse flag and set it in args consumption loop if corresponding argument is provided
- If reverse flag is set, change the cache file name to [md5]-reverse.txt and the pos file name to [md5].pos-reverse.txt
- If we already have a non-reversed cache, write a reversed version of it to [md5]-reverse.txt
- If we already have a non-reversed pos and a reversed or non-reversed cache, we could calculate the reversed pos
- For inputfiles, likewise write a reversed version to cache if not already there, and do this before the calculation of reversed pos so that it could use this, then in the pipeline use this cached list instead of the actual file. This is not going to work for pseudofiles, I think there's no way to reverse those without consuming the entire thing first, so I'll explicitly only do the reverse things if it's a real file (`-f`) and note in the comments and help text that `--reverse` doesn't work with pseudofiles.
- For queries without a cached list, if reverse flag is set, add `--playlist-reverse` to `ytdlp_args`. Here I discovered that `--playlist-reverse` and `--playlist-start` don't work together. but as we've seen earlier, the former does `-I NUMBER::` and the latter does `-I ::-1`, so we can combine them in the `-I` argument. Negative number for the start index to specify a position from the end.
- We don't need the functions `purge_cache` and `purge_pos`, it complicates things because we can now have `-startn` or `-reverse` in the path, we only call them once so just `rm` the paths we've calculated already for the cache and pos files.

```sh
#!/usr/bin/env sh
# Gapless mpv
# Modified from ZaxonXP's gapless_play
# https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play.sh
#
# Dependencies:
# - mpv
# - yt-dlp
# - wmctrl
# - socat
# - (Optional) bspwm window manager
#
# Usage:
# gaplessmpv [--reverse] [--purgecache] [--purgepos] (QUERY|INPUTFILE)
# QUERY = any link or search accepted by yt-dlp.
# * gaplessmpv scsearch1000:fjellstrom
# * gaplessmpv youtube.com/@slowgermanlistening/videos
# * gaplessmpv youtube.com/?v=zADVzO7m5qM
# INPUTFILE = a file where each line contains a link to a video or a yt-id.
# * gaplessmpv Example_list.txt
# * gaplessmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)
# Options:
# --reverse     Read video list in reverse order. Does not support pseudofiles.
# --purgecache  Remove file that caches the video list for a given QUERY.
# --purgepos    Remove file that stores current position in the video list.

NAME="${0##*/}"

help() {
    cat <<EOF
Usage: $NAME [options] (QUERY|INPUTFILE)

QUERY = any link or search accepted by yt-dlp.
INPUTFILE = a file where each line contains a link to a video or a yt-id.

Options:

-h | --help   Show this help text and exit.
--reverse     Read video list in reverse order. Does not support pseudofiles.
--purgecache  Remove file that caches the video list for a given QUERY.
--purgepos    Remove file that stores current position in the video list.

Examples:
* $NAME scsearch1000:fjellstrom
* $NAME youtube.com/@slowgermanlistening/videos
* $NAME youtube.com/?v=zADVzO7m5qM
* $NAME Example_list.txt
* $NAME <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)
EOF
}

has_bspc=$(command -v bspc >/dev/null 2>&1 && echo 1)

if [ $has_bspc ] && ! bspc rule -l | grep -q 'mpv:mpvalt.*hidden=on'; then
    bspc rule -a mpv:mpvalt hidden=on
fi

prefix_echo() {
  if [ "$(("$1" % 2))" = "0" ]; then
      color=green
  else
      color=cyan
  fi
  perl -sne 'use Term::ANSIColor;$|=1; print colored("[$pre]\t$_", $col)' -- -col="$color" -pre="$2"
}

get_args() {
    if [ "$1" = 0 ]; then
        echo "--quiet --force-window=yes"
    else
        echo "--quiet --force-window=yes --x11-name=mpvalt --pause"
    fi
}

get_soc() {
    echo "/tmp/${NAME}_soc_$(( $1 % 2 ))"
}

unhide_alt() {
    if [ $has_bspc ]; then
        bspc node "$(wmctrl -lx | grep mpvalt | cut -d' ' -f1)" -g hidden=off
    else
        wmctrl -xr mpvalt -b remove,hidden
    fi
}

i=0

pid=
prev_pid=

store_pid() {
    prev_pid="$pid"
    pid="$1"
}

wait_alt() {       # wait until prev exits, then unhide and unpause $1
    tail --pid="$prev_pid" -f /dev/null
    unhide_alt
    echo '{ "command": ["set_property", "pause", false] }' | socat - "$1"
}

get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

store_pos() {                   # $1 = pos, $2 = pos file
    if [ -n "$1" ] && [ -n "$2" ]; then
        echo "$1" > "$2"
        echo "$NAME: Stored position $1 in $2"
    fi
}

process_lines() {     # main work loop. $1 = pos offset, $2 = pos file
    read -r line || exit
    while :; do
        link=$(get_link "$line")
        soc=$(get_soc $i)
        mpv --term-playing-msg="$link" --input-ipc-server="$soc" $(get_args $i) \
            "$link" |
            prefix_echo $i $i &
        store_pid $!
        [ $i -gt 0 ] && wait_alt "$soc"
        [ $i -gt 0 ] && store_pos "$(($i + "$1"))" "$2"
        if read -r next; then
            line="$next"
            i=$((i + 1))
        else  # last line
            echo "$NAME: Last video"
            tail --pid="$pid" -f /dev/null  # wait for last mpv process
            break
        fi
    done
}

CACHE_DIR="$HOME/.cache/$NAME"
mkdir -p "$CACHE_DIR"

get_cache_file() {  # $1 = query
    echo "$CACHE_DIR/$(echo -n "$1" | md5sum | cut -d' ' -f1).txt"
}

get_pos_file() {  # $1 = query
    cache_file=$(get_cache_file "$1")
    echo "${cache_file%.txt}.pos.txt"
}

reverse_flag=0
purge_cache_flag=0
purge_pos_flag=0

while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            help
            exit;;
        --reverse)
            reverse_flag=1;;
        --purgecache)
            purge_cache_flag=1;;
        --purgepos)
            purge_pos_flag=1;;
        --)
            shift
            break;;
        -*)
            echo "$NAME: Unknown option $1" >&2
            exit 1;;
        *)
            break;;
    esac
    shift
done

# by peterh https://stackoverflow.com/a/21188136/18396947
get_abs_filename() {
  filename=$1
  parentdir=$(dirname "${filename}")
  if [ -d "${filename}" ]; then
      echo "$(cd "${filename}" && pwd)"
  elif [ -d "${parentdir}" ]; then
    echo "$(cd "${parentdir}" && pwd)/$(basename "${filename}")"
  fi
}

query=$1
[ -z "$query" ] && { help; exit 1; }
[ -f "$query" ] && query=$(get_abs_filename "$query") # normalise file path

cache_file=$(get_cache_file "$query")
pos_file=$(get_pos_file "$query")

if [ "$reverse_flag" = 1 ]; then # calculate reverse cache and pos if possible
    reverse_cache_file="${cache_file%.txt}-reverse.txt"
    reverse_pos_file="${pos_file%.txt}-reverse.txt"
    len=
    if [ -f "$query" ]; then    # INPUTFILE and non-pseudofile
        if [ ! -e "$reverse_cache_file" ]; then
            tac "$query" > "$reverse_cache_file"
            echo "$NAME: Wrote reversed list to cache $reverse_cache_file"
        fi
    else                        # QUERY
        if [ -r "$cache_file" ] && [ ! -e "$reverse_cache_file" ]; then
            tac "$cache_file" > "$reverse_cache_file"
            echo "$NAME: Wrote reversed cached list to $reverse_cache_file"
        fi
    fi
    if [ -e "$reverse_cache_file" ]; then
        len=$(wc -l < "$reverse_cache_file")
    elif [ -e "$cache_file" ]; then
        len=$(wc -l < "$cache_file")
    fi
    if [ -e "$pos_file" ] && [ ! -e "$reverse_pos_file" ] && [ -n "$len" ]; then
        pos="$(cat "$pos_file")"
        reverse_pos=$(($len - $pos - 1))
        echo $reverse_pos > "$reverse_pos_file"
        echo "$NAME: Calculated reverse pos $reverse_pos > $reverse_pos_file"
    fi
    cache_file=$reverse_cache_file
    pos_file=$reverse_pos_file
fi

if [ "$purge_cache_flag" = 1 ] && [ -f "$cache_file" ]; then
    rm "$cache_file"
    echo "$NAME: Purged cache file $cache_file"
fi
if [ "$purge_pos_flag" = 1 ] && [ -f "$pos_file" ]; then
    rm "$pos_file"
    echo "$NAME: Purged pos file $pos_file"
fi

ytdlp_pid=
cache_pid=

cleanup() {                     # kill forked processes
    [ -n "$ytdlp_pid" ] && kill "$ytdlp_pid" 2>/dev/null
    [ -n "$cache_pid" ] && kill "$cache_pid" 2>/dev/null
    exit 130
}

trap cleanup INT TERM

if [ -r "$query" ]; then        # INPUTFILE
    inputfile=$query
    [ "$reverse_flag" = 1 ] && [ -r "$cache_file" ] && inputfile=$cache_file
    if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then # pos
        pos="$(cat "$pos_file")"
        echo "$NAME: Restoring position $pos"
        cat "$inputfile" | tail -n "+$(($pos + 1))" |
            process_lines $pos "$pos_file"
    else                        # no pos
        process_lines 0 "$pos_file" < "$inputfile"
    fi
else                            # QUERY
    if [ -r "$cache_file" ]; then # cache
        echo "$NAME: Using cached list $cache_file"
        if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then # pos
            pos="$(cat "$pos_file")"
            echo "$NAME: Restoring position $pos"
            cat "$cache_file" | tail -n "+$(($pos + 1))" |
                process_lines $pos "$pos_file"
        else                    # no pos
            process_lines 0 "$pos_file" < "$cache_file"
        fi
    else                        # no cache
        ytdlp_args="--no-warnings"
        pos=0
        if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then # pos
            pos="$(cat "$pos_file")"
            if [ "$reverse_flag" = 1 ]; then
                ytdlp_args="$ytdlp_args -I -$(($pos + 1))::-1"
            else
                ytdlp_args="$ytdlp_args --playlist-start $(($pos + 1))"
            fi
            cache_file="${cache_file%.txt}-start${pos}.txt"
            echo "$NAME: Restoring position $pos"
        else                    # no pos
            [ "$reverse_flag" = 1 ] && ytdlp_args="$ytdlp_args -I ::-1"
        fi
        stdbuf -oL yt-dlp $ytdlp_args --print "%(webpage_url)s" "$query" \
               > "$cache_file.tmp" & # line-buffered cache
        ytdlp_pid=$!
        ( # finalise cache when yt-dlp is done irrespective of read loop
            tail --pid="$ytdlp_pid" -f /dev/null
            if [ -s "$cache_file.tmp" ]; then
                mv "$cache_file.tmp" "$cache_file"
                echo "$NAME: Finalised cache $cache_file"
            fi
        ) &
        cache_pid=$!
        tail -f "$cache_file.tmp" | process_lines $pos "$pos_file" # stream
    fi
fi
```

##### Problems remain
It's still got fundamental usability problems, like if you change any setting in the player on one of the videos (like change the volume) they will be gone when it goes to the next video because it's a new instance. I put the volume (`volume=200`) and playback speed (`speed=1.5`) I usually use in mpv.conf to get around this. but if you use mpv to also listen to music or series of videos where you need different volumes or speeds I guess you would need several configs or to pass the options as cli arguments (or I could imagine a solution where you would observe properties like volume using IPC and if they change apply those things to the next instance). You also can't use `keep-open` or it will not pass automatically to the next video, because it relies on the player quitting at the end of each video.

I have also not handled the situation where the user advances too quickly or closes the wrong window.

A different approach could be to keep two mpv instances open the entire time and alternate between them, instead of opening a new instance each time. We would need a way to know when a video ended.

More problems (inexhaustive):
1. In a bilibili playlist if there are files that have Ps in them, reverse reverses the order of them as well. On the site when you reverse playlist this doesn't happen. It could be worked around if you have cache by reversing the order of P sequences in the cache [see next subsection where I wrote a function to do this]. The index will also be off compared to on the site, because each P is its own video on its own line. On mpv proper when you load a bilibili playlist it does seem to take account of it, `playlist-play-index` with the same index minus 1 takes you to the right video.
2. `unhide_alt` does not robustly get the window and fails to unhide if you have several gaplessmpv instances
3. New instances open in current workspace, ideally should open in the same location as the original instance. If you want to dump them all to a particular workspace, you can add that to the mpvalt rule.

I will leave it here. See if you can iterate and improve on this, if you wish. Thanks to [ZaxonXP](https://github.com/ZaxonXP) for the cool scripts.

[`gaplessmpv` version control link where I will put future changes](https://github.com/plu5/dotfiles/blob/main/pm/scripts/gaplessmpv)

#### Elisp function to correct order of P links
Corrects order of ?p= links in region (with the cache files, just select the entire buffer with <kbd>C-x h</kbd>). Turns a region like:
```
 link
 link?p=2
 link?p=1
 link
 link?p=3
 link?p=2
 link?p=1
 link
```
into:
```
 link
 link?p=1
 link?p=2
 link
 link?p=1
 link?p=2
 link?p=3
 link
```
(but not vice versa)
```elisp
(defun correct-order-of-p-links (beg end)
  "Correct order of ?p= links in region."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (begp begpline begpos curp curpline endpos)
      (while (re-search-forward "?p=\\([0-9]+\\)" nil t 1)
        (setq curp (string-to-number (match-string 1)))
        (setq curpline (line-number-at-pos (match-beginning 1)))
        (message "P:%s L:%s" curp curpline)
        (if (and begp (= curp (1- begp)) (= curpline (1+ begpline)))
            (progn
              (message "reverse lines %s to %s" begpline (1- (+ begp begpline)))
              (end-of-line (- (+ begp begpline) (line-number-at-pos (point))))
              (setq endpos (point))
              (reverse-region begpos endpos)
              (goto-char endpos)
              (setq begp nil))
          (when (> curp 1)
            (setq begp curp)
            (setq begpline curpline)
            (beginning-of-line)
            (setq begpos (point))
            (end-of-line)))))))
```
The idea here is search for a ?p=, then if the next one is on the next line and is 1 less, assume we have a series of P links here in declining order and it reverses the region from the first P it found until n lines after, where n is the value of the first P. For example if it finds ?p=39 and then ?p=38, it reverses from the ?p=39 39 lines down, so that it will have hopefully reversed from ?p=39 to ?p=1.

After `reverse-region` the point seems to end up before the region. The `(goto-char endpos)` that follows is an optimisation to avoid going through all the lines we just reversed.

#### Command to toggle the oldest mpv window
Window IDs are not necessarily in order of creation, but PIDs are.
```sh
# toggle the oldest window (sorted by PID)
xdotool key --window $(
    xdotool search --name ' - mpv' |
        while read -r wid; do
            pid=$(xprop -id "$wid" _NET_WM_PID 2>/dev/null | cut -d ' ' -f3)
            [ -n "$pid" ] && echo "$pid $wid"
        done |
        sort -n |
        head -1 |
        cut -d ' ' -f2
        ) --clearmodifiers space
```
Instead of space, you could toggle pause via IPC [like ZaxonXP does it](https://github.com/ZaxonXP/youtube-viewer-scripts/blob/master/gapless_play_pause.sh). You would have to also restore to the script setting a file to track current socket which I had got rid of, but it's trivial (just add `echo "$soc" > /tmp/somefiletostorecurrentsocket` to the end of each iteration of the main work loop). What happens if you have several instances?

#### Loose end: Observe property over IPC
While researching I came across this [mpv-toolbox PR](https://codeberg.org/NRK/mpv-toolbox/pulls/13), where I saw:
```lua
-- HACK: mpv doesn't open the window instantly by default.
-- so wait for 'vo-configured' to be true before trying to
-- grab the xid.
mp.observe_property('vo-configured', 'native', grab_xid)
```

Note also [mpv issue 4410](https://github.com/mpv-player/mpv/issues/4410) and this warning in man mpv:

> If the connection is closed, the IPC client is destroyed internally,
and the observed properties are unregistered. This happens for example
when sending commands to a socket with separate `socat` invocations.
This can make it seem like property observation does not work. You must
keep the IPC connection open to make it work.

I thought maybe if you can use observe property from IPC we could use this after launching the instance to wait for a window. but this is not going to work because IPC doesn't work right after launching the instance, we need a way to be woken up when it's ready, that's the whole problem.

Maybe something like this:
```sh
        soc=$(get_soc $i)
        mpv --term-playing-msg="$link" --input-ipc-server="$soc" $(get_args $i) \
            "$link" |
            prefix_echo $i $i &
        while [ ! -S "$soc" ]; do
            sleep 0.05
        done
        echo '{ "command": ["get_property", "window-id"] }' | socat - $soc
```
no, I still get connection refused.

{% include note.html content='
> [!NOTE]
> Maybe you\'re over complicating it, a simple `sleep 1` after launching an instance seems to do the job.
' %}

#### Attempt at some sort of combination of the two approaches
I noticed that the yt-dlp option `--get-url` gives a long mirror link kind of like the ones mpv-handler-queue outputs. I wonder if using this and IPC loadfile append:
```sh
echo '{ "command": ["loadfile", '"\"$link\""', append] }' | socat - "$soc"
```
we could get the list of videos in a real mpv playlist that works with `prefetch-playlist=yes`.

Test script called "queuedmpv"
```sh
get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

file="$HOME/Example_list.txt"
i=0
soc="/tmp/queuedmpv-soc"
while read -r line; do
    link=$(get_link "$line")
    link=$(yt-dlp --no-warnings --get-url "$link" | head -1)
    if [ $i = 0 ]; then
        mpv --input-ipc-server="$soc" "$link" &
    else
        echo '{ "command": ["loadfile", '"\"$link\""', "append"] }' | socat - "$soc"
    fi
    i=$((i + 1))
done < "$file"
```
It does work, but video only. `--get-url` gives me two links, I guess one for the video and one for the audio. How to load them both as a combined video?

{% include note.html content='
> [!NOTE]
> This is the case for YouTube videos, but maybe not universally.
' %}

There is the command `audio-add` but I can't do it to videos in the playlist, only the currently playing video.

I also worry that the links we get are temporary, so if you have the instance open for a long time they will expire. I see `expire=1773675815` in the url which is in 6 hours at the time I'm writing this.

It's not impossible to manage this in the script. There is the command `playlist-remove <index>` to remove a video from the playlist, so we could keep track of the link lifetimes and remove and regenerate them when needed. Since the video list could be very long, maybe the amount of links we keep in the playlist at a time should be limited to a certain number.

I guess this shows that even if you can get mpv-handler-queue to work, it's in no way "the proper solution" either. It would have suffered from expiring links if nothing else.

This is not documented in the manual but in mpv-handler-queue (see [commit b0a0dc3](https://github.com/gabreek/mpv-handler-queue/commit/b0a0dc3)) he adds audio-file in the options to loadfile. The format seems to be:
```sh
{ "command": ["loadfile", "[video_url]", "append", { "audio-file": "[audio_url]", "title": "[title]" }] }
```
so I tried:
```sh
while read -r line; do
    links=$(yt-dlp --no-warnings --get-url "$(get_link "$line")")
    video=$(echo "$links" | head -1)
    audio=$(echo "$links" | sed -n 2p)
    if [ $i = 0 ]; then
        mpv --input-ipc-server="$soc" "$video" --audio-file="$audio" &
    else
        echo '{"command": ["loadfile", '"\"$video\""', "append", {"audio-file": '"\"$audio\""'}]}' | socat - "$soc"
    fi
    i=$((i + 1))
done < "$file"
```
The IPC command fails with:
```sh
[ipc_0] Command loadfile: argument index has incompatible type.
```
This is familiar. It's the same error I got with mpv-handler-queue [earlier](#trying-to-get-gabreeks-thing-to-work-failed).

Formatting the string differently does not help:
```sh
printf '{"command": ["loadfile", "%s", "append", {"audio-file": "%s"}]}\n' "$video" "$audio" | socat - "$soc"
```

Did this used to be supported and now isn't? (mpv version: v0.41.0)

Ah hang on a minute, the error is quite clear actually. From the [manual](https://mpv.io/manual/master/), loadfile format:

> `loadfile <url> [<flags> [<index> [<options>]]]`

> The third argument is an insertion index, used only by the insert-at action. When used with those actions, the new item will be inserted at the index position in the playlist, or appended to the end if index is less than 0 or greater than the size of the playlist. This argument will be ignored for all other actions. This argument was added in mpv 0.38.0.

> Since mpv 0.38.0, an insertion index argument is added as the third argument. This breaks all existing uses of this command which make use of the argument to include the list of options to be set while the file is playing. To address this problem, the third argument now needs to be set to -1 if the fourth argument needs to be used.

```sh
printf '{"command": ["loadfile", "%s", "append", -1, {"audio-file": "%s"}]}\n' "$video" "$audio" | socat - "$soc"
```
It succeeds, but when I go to the next video in the playlist, the audio is still the one from the first video. The second audio track is there, but doesn't get selected automatically.

I don't see any logic in mpv-handler-queue to change audio tracks.

Ideas:
- observe property to tell when video changes and swap to last audio track
- patch mpv-handler-queue to fix its IPC command to see if it would have the same issue with audio (and if yes, then how did it work for him? possibly other things changed between mpv versions)

#### Patching mpv-handler-queue
The file concerned is [src/plugins/play.rs](https://github.com/gabreek/mpv-handler-queue/blob/main/src/plugins/play.rs)

```diff
diff --git a/src/plugins/play.rs b/src/plugins/play.rs
index 3ec62aa..2c94978 100644
--- a/src/plugins/play.rs
+++ b/src/plugins/play.rs
@@ -198 +198 @@ pub fn exec(proto: &Protocol, config: &Config) -> Result<(), Error> {
-                    let load_command = json!({ "command": ["loadfile", video_url, "append", options_obj] });
+                    let load_command = json!({ "command": ["loadfile", video_url, "append", -1, options_obj] });
@@ -356 +356 @@ fn handle_playlist_in_new_instance(
-            let first_cmd = json!({ "command": ["loadfile", first_url, "replace", { "title": first_title }] });
+            let first_cmd = json!({ "command": ["loadfile", first_url, "replace", -1, { "title": first_title }] });
@@ -369 +369 @@ fn handle_playlist_in_new_instance(
-                let load_cmd = json!({ "command": ["loadfile", video_url, "append", opts] });
+                let load_cmd = json!({ "command": ["loadfile", video_url, "append", -1, opts] });
```

Now I have to build it again and replace the previous executable:
```sh
cargo build --release
cp target/release/mpv-handler ~/.local/bin/
chmod +x ~/.local/bin/mpv-handler
```

With an mpv instance already open, [clicking the purple button,] the same behaviour as before, same output, no errors but despite the fact it says it enqueues, it doesn't add anything to the playlist of the instance. Verified I do have `input-ipc-server=/tmp/mpvsocket` in my config, and that's the socket it says it connects to. Even tried to explicitly open an instance with `--input-ipc-server=/tmp/mpvsocket`.

Without mpv open, clicking the purple button just opens the playlist normally in the instance that it opens, as if you loaded it yourself into mpv. No "enqueueing" direct URLs.

{% include note.html content='
> [!NOTE]
> "The purple button" refers to the button in the corner that the "Play with MPV (Enhanced)" userscript that you\'re supposed to use adds.
' %}

Left clicking one of the videos in the playlist instead opens the zenity dialog and it succeeds to enqueue. Both to an existing instance and without an existing instance.

The audio tracks work fine, 1 per video, not 2 when you pass to the second one like in my script.

Submitted [a PR](https://github.com/gabreek/mpv-handler-queue/pull/1) to fix the index issue.

#### cont attempts to fix queuedmpv audio tracks issue
The only thing that's different in his IPC command is he also passes the title in the options. I can't see in the source anything different with the audio tracks.

We get add `--get-title` to the yt-dlp command:
```sh
while read -r line; do
    info=$(yt-dlp --no-warnings --get-title --get-url "$(get_link "$line")")
    title=$(echo "$info" | head -1)
    video=$(echo "$info" | sed -n 2p)
    audio=$(echo "$info" | sed -n 3p)
    if [ $i = 0 ]; then
        mpv --input-ipc-server="$soc" "$video" --audio-file="$audio" &
    else
        printf '{"command": ["loadfile", "%s", "append", -1, {"title": "%s", "audio-file": "%s"}]}\n' "$video" "$title" "$audio" | socat - "$soc"
    fi
    i=$((i + 1))
done < "$file"
```
Now it should be the exact same IPC command as he sends, but sadly it has made no difference for the audio tracks problem.

Oh hang on a minute. Is it because I'm passing `--audio-file` when I launch the instance? Maybe it then affects the entire playlist. I did see that in mpv-handler-queue for the "initial video" he just passes the url of the video page instead of the direct links. In function `handle_playlist_in_new_instance`:
```rust
// 1. Load the first video (don't pre-extract, let mpv do it)
let (first_title, first_url) = &playlist_entries[0];
```

It was that.
```sh
get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

file="$HOME/Example_list.txt"
i=0
soc="/tmp/queuedmpv-soc"
while read -r line; do
    link=$(get_link "$line")
    info=$(yt-dlp --no-warnings --get-title --get-url "$link")
    title=$(echo "$info" | head -1)
    video=$(echo "$info" | sed -n 2p)
    audio=$(echo "$info" | sed -n 3p)
    if [ $i = 0 ]; then
        mpv --input-ipc-server="$soc" "$link" &
    else
        printf '{"command": ["loadfile", "%s", "append", -1, {"title": "%s", "audio-file": "%s"}]}\n' "$video" "$title" "$audio" | socat - "$soc"
    fi
    i=$((i + 1))
done < "$file"
```

#### Make queuedmpv usable
In theory I could take gaplessmpv, and instead of juggling two different processes, use one and keep two videos loaded in the playlist. Or rather load one video ahead. The previous ones may expire too though and the user might go back... I don't want to have to get new links every 6 hours for the entire playlist because it could be large.

Maybe only worry about the next and previous videos. Observe property to tell when the position in the playlist changed, get new URLs for next and previous videos if it's been over 6 hours.

- mpv sends playlist-pos position changed right where we launch it for the first video, so no need to preload anything initially, let observe handle it
- I guess we have to keep track to avoid loading the same one several times, e.g. when the user is going back and forth between videos in the playlist. Just the indices would be sufficient. but IIRC it's annoying in POSIX shell to check existence in an array. Abandon POSIX-compatibility and use Bash associative array? Could keep them in a string with delimination for start and end of the index, like `[0][2][9][20]`, that way we could search it without false positives. If we need to store timestamps too, they can each be like `[0:timestamp]`.
- Sometimes the whole video could be in cache and to refresh the URL would be wasteful. I don't know if I have a way to tell this and avoid refreshing URL in that case even if it's been 6 hours. For the moment I won't implement the refreshing the URL after 6 hours logic, and add debug prints each time it passes to the next video with current timestamp and video duration, so that I would be able to see if it skipped.

```sh
NAME="${0##*/}"
file="$HOME/Example_list2.txt"
soc="/tmp/queuedmpv-soc"
mpvargs="--quiet --prefetch-playlist=yes"

get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

get_link_at_line_number() {     # $1 = line number
    n=$(($1 + 1))
    line=$(sed -n ${n}p "$file")
    get_link "$line"
}

ipc() { # $1 = command, rest = passed as the remaining arguments to printf
    cmd="$1\n"
    shift
    printf "$cmd" "$@" | socat - "$soc"
}

preload_link() {                # $1 = link
    info=$(yt-dlp --no-warnings --get-title --get-url "$1")
    title=$(echo "$info" | head -1)
    video=$(echo "$info" | sed -n 2p)
    audio=$(echo "$info" | sed -n 3p)
    ipc '{"command": ["loadfile", "%s", "append", -1, {"title": "%s", "audio-file": "%s"}]}' "$video" "$title" "$audio"
}

get_data() {                    # $1 = ipc output
    echo "$1" | sed -n 's/.*"data":*\([0-9]\+\).*/\1/p'
}

loaded="[0:$(date +%s)]"

handle_playlist_pos_changed() { # $1 = line
    pos=$(get_data "$line")
    if [ -n "$pos" ]; then
        echo "$NAME: playlist-pos changed to $pos [$(date '+%F %T')]"
        pos=$((pos + 1))
        if ! echo "$loaded" | grep -q "\[$pos:"; then
            preload_link "$(get_link_at_line_number $pos)"
            loaded="$loaded[$pos:$(date +%s)]"
        fi
    fi
}

observe() {
    fifo=$(mktemp -u)
    mkfifo "$fifo"
    socat "$soc" - < "$fifo" |
        while read -r line; do
            case "$line" in
                *'"name":"playlist-pos","data"'*)
                    handle_playlist_pos_changed "$line";;
                *'"name":"duration","data"'*)
                    echo "$NAME: $(get_data "$line") seconds duration";;
            esac
    done &
    exec 3>"$fifo"
    printf '{"command": ["observe_property", 1, "playlist-pos"]}\n' >&3
    printf '{"command": ["observe_property", 1, "duration"]}\n' >&3
}

mpv $mpvargs --input-ipc-server="$soc" "$(get_link_at_line_number 0)" &

sleep 1
observe
wait
```
We keep the timestamps but don't do anything with them yet.

This is functional. Now what I will do is combine it with gaplessmpv in order to have all its features, so it will be essentially the same interface other than using a single mpv instance, except for the following changes:

- Something that annoys me is the mpv output (I'm looking at you, endless ``[vo/gpu-next/libplacebo] Masking `storable` from wrapped texture because the corresponding format 'bgra8' does not support PL_FMT_CAP_STORABLE``). There are no mpv options to suppress this without turning off useful output. I'm going to use Zaxon's Perl thing that he uses to colour and preface output (`prefix_echo`) to suppress messages I don't want, and I guess add an option `--rawoutput` for those that don't want this.
- Change the cache and position filename to have a human-readable component. Maybe by using the A-Za-z0-9 parts of the query/filename, up to a certain number of characters.
- If not starting in position 0, we also need to load to the playlist the video before the current video. `handle_playlist_pos_changed` needs to have logic to also load pos-1 video if not loaded yet, not just pos+1.
  + so we have to use `insert-at` instead of `append` with mpv's `loadfile`.
  + There are not negative indices, but passing the value of the current `playlist-pos` as the index loads it before the current video. This triggers a `playlist-pos` change, so in order to avoid an infinite loop I had to add a flag `ignore_playlist_pos_change` to set when we load a video before current one, and just unset and return in `handle_playlist_pos_changed` if it is set.
    - Renamed `ignore_playlist_pos_change` to `playlist_pos_bump_only`, because rather than ignore we need to update `prev_playlist_pos` so that on the next real playlist-pos change the offset will not be off.
- When using `--reverse` with INPUTFILE, we should verify the date on INPUTFILE and regenerate the reversed list if it's more recent than the cache in order to reflect changes to the file.
- Add `--pos=N` option to force starting position irrespective of pos file.
- At the start, `playlist-pos` is always going to be 0, but `pos` might not be. These variables need to be kept separate. `pos` keeps track of the line number in the actual file so we should use that one to decide what to load, not `playlist-pos`. `playlist-pos` should be used to check where the user navigated in the playlist (next, previous, or it could be even jumping several, but not beyond the contiguous range we've already loaded) by checking its new value against its previous value.
- If there are double quotes `""` in the video title, it breaks our IPC command JSON. I don't want to introduce another dependency, so I just escape quotes and backslash in the title and hope that it's enough.
- Handling end of list: It suffices to just do nothing when `get_link_at_line_number` stdout is empty.
- Having a human-readable component to the cache and pos filenames made me realise that we should not be saving anything for pseudofiles. It's not going to be able to distinguish one from the other. `devfd63........463ecdb87a44dc7.pos.txt`
- Add `--nosave` option to optionally avoid saving position and cache.
- The sleep before the call to `observe` is fragile. How long the socket takes to be set up depends on how long mpv takes to load the first video. Open it on idle then before trying to load anything. Then preload the first video in the way we preload others, just with "replace" instead of "insert-at", so that it starts playback.
  + When opening it on idle, the first `playlist-pos` I get is -1 rather than 0. We need to ensure then that we ignore it when it's less than 0.
- There is no indication in the terminal of the title of the video playing, just the really long mirror links. The options object we are passing in the loadfile IPC is per-file CLI options, and `--title` sets the _window_ title. There is no `--media-title`, but there _is_ `--force-media-title`. Setting that instead of `title` still affects the window title and also the video title in the playlist and media info (<kbd>i</kbd> key). It doesn't, however, affect the mirror title gore we get in the terminal output. I thought about adding `--term-playing-msg='${media-title}'` to the arguments passed to the mpv instance, but it just outputs `'title'`, I guess because `--force-media-title` sets it too late? Observe `media-title` then just to be able to echo it, like we do with `duration`.
- With a pseudofile or query, the line with the link could still be empty when `get_link_at_line_number` gets called. Put a loop in there reattempting a few times.

```sh
#!/usr/bin/env sh
# Queued mpv
# Based on the work of ZaxonXP (gapless_play.sh) and gabreek (mpv-handler-queue)
#
# Dependencies:
# - mpv > 0.38.0
# - yt-dlp
# - socat
#
# Usage:
# queuedmpv [--rawoutput] [--reverse] [--purgecache] [--purgepos] [--pos=N] (QUERY|INPUTFILE)
# QUERY = any link or search accepted by yt-dlp.
# * queuedmpv scsearch1000:fjellstrom
# * queuedmpv youtube.com/@slowgermanlistening/videos
# * queuedmpv youtube.com/?v=zADVzO7m5qM
# INPUTFILE = a file where each line contains a link to a video or a yt-id.
# * queuedmpv Example_list.txt
# * queuedmpv <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)

NAME="${0##*/}"
soc="/tmp/queuedmpv-soc"
mpvargs="--quiet --prefetch-playlist=yes --force-window=yes"
pos=
file=
loaded=
should_save=1
mpvoutput_colour="magenta"
mpvoutput_filter='^\[vo\/gpu-next\/libplacebo|^MESA-INTEL:|^\[osd\/libass'
max_read_attempts=5
CACHE_DIR="$HOME/.cache/$NAME"
mkdir -p "$CACHE_DIR"

help() {
    cat <<EOF
Usage: $NAME [options] (QUERY|INPUTFILE)

QUERY = any link or search accepted by yt-dlp.
INPUTFILE = a file where each line contains a link to a video or a yt-id.

Options:

-h | --help   Show this help text and exit.
--rawoutput   Don't filter and colour mpv output.
--reverse     Read video list in reverse order. Does not support pseudofiles.
--purgecache  Remove file that caches the video list for a given QUERY.
--purgepos    Remove file that stores current position in the video list.
--pos=N       Force starting position N.

Examples:
* $NAME scsearch1000:fjellstrom
* $NAME youtube.com/@slowgermanlistening/videos
* $NAME youtube.com/?v=zADVzO7m5qM
* $NAME Example_list.txt
* $NAME <(yt-dlp --no-warnings --print "%(webpage_url)s" ytsearch2:cat)
EOF
}

get_link() {    # if $1 contains a yt-id return its link, $1 otherwise
    id=$(echo "$1" | sed -n \
's/.*\(^\|[ |]\)\([A-Za-z0-9_-]\{11\}\)\([ |]\|$\).*/\2/p')
    [ -z "$id" ] && echo "$1" || echo "https://www.youtube.com/watch?v=$id"
}

get_link_at_line_number() {     # $1 = 0-based line number
    [ ! -e "$file" ] && file=${file%.tmp}
    n=$(($1 + 1))
    line=$(sed -n ${n}p "$file")
    attempt=0
    while [ -z "$line" ]; do
        [ "$attempt" -gt "$max_read_attempts" ] && break
        sleep 1
        line=$(sed -n ${n}p "$file")
        attempt=$((attempt + 1))
    done
    get_link "$line"
}

store_pos() {                   # $1 = pos, $2 = pos file
    if [ -n "$1" ] && [ -n "$2" ]; then
        echo "$1" > "$2"
        echo "$NAME: Stored position $1 in $2"
    fi
}

ipc() { # $1 = command, rest = passed as the remaining arguments to printf
    cmd="$1\n"
    shift
    printf "$cmd" "$@" | socat - "$soc"
}

json_escape() {
    if [ $# -gt 0 ]; then
        printf '%s' "$1" |
            sed -e 's/\\/\\\\/g' \
                -e 's/"/\\"/g'
    else                        # stdin
        sed -e 's/\\/\\\\/g' \
            -e 's/"/\\"/g'
    fi
}

preload_link() { # $1 = link. optional: $2 = index, $3 = loadfile flags
    info=$(yt-dlp --no-warnings --get-title --get-url "$1")
    title=$(echo "$info" | head -1 | json_escape)
    video=$(echo "$info" | sed -n 2p)
    audio=$(echo "$info" | sed -n 3p)
    i=-1
    [ -n "$2" ] && i=$2
    if [ -z "$video" ]; then
        "$NAME: Preload failed, empty video. yt-dlp output: $info"
        return 1
    fi
    flags="insert-at"
    [ -n "$3" ] && flags=$3
    ipc '{"command": ["loadfile", "%s", "%s", %s, {"force-media-title": "%s", "audio-file": "%s"}]}'\
        "$video" "$flags" "$i" "$title" "$audio"
}

get_data() {                    # $1 = ipc output
    echo "$1" | sed -n 's/.*"data":\(.*\)}/\1/p'
}

maybe_load_pos() {              # $1 = pos, $2 (optional) = index
    if [ "$1" -gt -1 ] && ! echo "$loaded" | grep -q "\[$1:"; then
        echo "$NAME: Preloading pos $1"
        i=-1
        [ -n "$2" ] && [ "$2" -gt -1 ] && i=$2
        link=$(get_link_at_line_number $1)
        if [ -n "$link" ]; then
            preload_link "$link" "$i"
            loaded="$loaded[$1:$(date +%s)]"
            # if index is before current, loadfile will bump playlist-pos
            [ $i -gt -1 ] && [ ! $i -gt $playlist_pos ] && playlist_pos_bump_only=yes
        else
            echo "$NAME: No link at pos $1"
        fi
    fi
}

playlist_pos=
prev_playlist_pos=

handle_playlist_pos_changed() { # $1 = line
    playlist_pos=$(get_data "$line")
    if [ -n "$playlist_pos_bump_only" ]; then
        playlist_pos_bump_only=
        prev_playlist_pos=$playlist_pos
        return
    fi
    if [ "$playlist_pos" -gt -1 ]; then
        if [ -n "$prev_playlist_pos" ]; then
            offset=$((playlist_pos - prev_playlist_pos))
            pos=$((pos + offset))
            [ -n "$should_save" ] && store_pos "$pos" "$pos_file"
        fi
        echo "$NAME: playlist-pos changed to $playlist_pos (pos $pos) [$(date '+%F %T')]"
        maybe_load_pos $((pos + 1))
        maybe_load_pos $((pos - 1)) $((playlist_pos))
        prev_playlist_pos=$playlist_pos
    fi
}

observe() {
    fifo=$(mktemp -u)
    mkfifo "$fifo"
    socat "$soc" - < "$fifo" |
        while read -r line; do
            case "$line" in
                *'"name":"playlist-pos","data"'*)
                    handle_playlist_pos_changed "$line";;
                *'"name":"duration","data"'*)
                    echo "$NAME: $(get_data "$line") seconds duration";;
                *'"name":"media-title","data"'*)
                    echo "$NAME: $(get_data "$line") media-title";;
            esac
    done &
    exec 3>"$fifo"
    printf '{"command": ["observe_property", 1, "playlist-pos"]}\n' >&3
    printf '{"command": ["observe_property", 1, "duration"]}\n' >&3
    printf '{"command": ["observe_property", 1, "media-title"]}\n' >&3
}

handle_output() {               # $1 = output file
  perl -sne "
use Term::ANSIColor;
\$|=1;
if (/$mpvoutput_filter/) { }
else { print colored(\$_, '$mpvoutput_colour') }
" < "$1"
}

get_cache_file() {  # $1 = query
    pad=..............          # padding for human-readable component
    h=$(echo -n "$1" | sed 's/[^a-zA-Z0-9]*//g' | rev | cut -c -14 | rev)
    #     [14 chars human readable].[15 chars md5sum].txt
    echo "$CACHE_DIR/$h${pad:${#h}}.$(echo -n "$1" | md5sum | cut -c -15).txt"
}

get_pos_file() {  # $1 = query
    cache_file=$(get_cache_file "$1")
    echo "${cache_file%.txt}.pos.txt"
}

rawoutput_flag=
reverse_flag=
purgecache_flag=
purgepos_flag=

while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            help
            exit;;
        --rawoutput)
            rawoutput_flag=1;;
        --reverse)
            reverse_flag=1;;
        --nosave)
            should_save=;;
        --purgecache)
            purgecache_flag=1;;
        --purgepos)
            purgepos_flag=1;;
        --pos=*)
            pos="${1#--pos=}"
            echo "$NAME: Forcing position $pos";;
        --)
            shift
            break;;
        -*)
            echo "$NAME: Unknown option $1" >&2
            exit 1;;
        *)
            break;;
    esac
    shift
done

# by peterh https://stackoverflow.com/a/21188136/18396947
get_abs_filename() {
  filename=$1
  parentdir=$(dirname "${filename}")
  if [ -d "${filename}" ]; then
      echo "$(cd "${filename}" && pwd)"
  elif [ -d "${parentdir}" ]; then
    echo "$(cd "${parentdir}" && pwd)/$(basename "${filename}")"
  fi
}

query=$1
[ -z "$query" ] && { help; exit 1; }
[ -f "$query" ] && query=$(get_abs_filename "$query") # normalise file path

cache_file=$(get_cache_file "$query")
pos_file=$(get_pos_file "$query")
reverse_cache_file="${cache_file%.txt}-reverse.txt"
reverse_pos_file="${pos_file%.txt}-reverse.txt"

reverse_pos() {
    len=
    if [ -e "$reverse_cache_file" ]; then
        len=$(wc -l < "$reverse_cache_file")
    elif [ -e "$cache_file" ]; then
        len=$(wc -l < "$cache_file")
    fi
    if [ -e "$pos_file" ] && [ ! -e "$reverse_pos_file" ] && [ -n "$len" ]; then
        pos="$(cat "$pos_file")"
        reverse_pos=$(($len - $pos - 1))
        echo $reverse_pos > "$reverse_pos_file"
        echo "$NAME: Calculated reverse pos $reverse_pos > $reverse_pos_file"
    fi
}

is_newer() {                   # $1 and $2 = paths to files or folders
    find "$1" -prune -newer "$2" | grep -q . && return 0 || return 1
}

if [ "$reverse_flag" = 1 ]; then # calculate reverse cache and pos if possible
    if [ -f "$query" ]; then    # INPUTFILE and non-pseudofile
        if [ ! -e "$reverse_cache_file" ] ||
               is_newer "$query" "$reverse_cache_file"; then
            tac "$query" > "$reverse_cache_file"
            echo "$NAME: Wrote reversed list to cache $reverse_cache_file"
        fi
    else                        # QUERY
        if [ -r "$cache_file" ] && [ ! -e "$reverse_cache_file" ]; then
            tac "$cache_file" > "$reverse_cache_file"
            echo "$NAME: Wrote reversed cached list to $reverse_cache_file"
        fi
    fi
    [ -z "$pos" ] && reverse_pos
    cache_file=$reverse_cache_file
    pos_file=$reverse_pos_file
fi

if [ "$purgecache_flag" = 1 ] && [ -f "$cache_file" ]; then
    rm "$cache_file"
    echo "$NAME: Purged cache file $cache_file"
fi
if [ "$purgepos_flag" = 1 ] && [ -f "$pos_file" ]; then
    rm "$pos_file"
    echo "$NAME: Purged pos file $pos_file"
fi

ytdlp_pid=
cache_pid=

cleanup() {                     # kill forked processes
    [ -n "$ytdlp_pid" ] && kill "$ytdlp_pid" 2>/dev/null
    [ -n "$cache_pid" ] && kill "$cache_pid" 2>/dev/null
    exit 130
}

trap cleanup INT TERM

try_restore_pos() {
    if [ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]; then
        pos="$(cat "$pos_file")"
        echo "$NAME: Restoring position $pos"
    fi
}

finalise_cache_fork() {
    (    # finalise cache when yt-dlp is done irrespective of read
        tail --pid="$ytdlp_pid" -f /dev/null
        if [ -s "$cache_file.tmp" ]; then
            mv "$cache_file.tmp" "$cache_file"
            echo "$NAME: Finalised cache $cache_file"
        fi
    ) &
    cache_pid=$!
}

if [ -r "$query" ]; then        # INPUTFILE
    file=$query
    if [ -f "$query" ]; then    # real file (not pseudofile)
        [ "$reverse_flag" = 1 ] && [ -r "$cache_file" ] && file=$cache_file
        [ -z "$pos" ] && try_restore_pos
    else
        echo "$NAME: Cache and position cannot be saved for pseudofiles"
        should_save=
    fi
else                            # QUERY
    if [ -r "$cache_file" ]; then # cache
        echo "$NAME: Using cached list $cache_file"
        file=$cache_file
        [ -z "$pos" ] && try_restore_pos
    else                        # no cache
        ytdlp_args="--no-warnings"
        if [ -n "$pos" ] || ([ -r "$pos_file" ] && [ "$(cat "$pos_file")" -gt 0 ]); then
            [ -z "$pos" ] && pos="$(cat "$pos_file")"
            if [ "$reverse_flag" = 1 ]; then
                ytdlp_args="$ytdlp_args -I -$(($pos + 1))::-1"
            else
                ytdlp_args="$ytdlp_args --playlist-start $(($pos + 1))"
            fi
            cache_file="${cache_file%.txt}-start${pos}.txt"
            echo "$NAME: Restoring position $pos"
        else                    # no pos
            [ "$reverse_flag" = 1 ] && ytdlp_args="$ytdlp_args -I ::-1"
        fi
        if [ -n "$should_save" ]; then
            file="$cache_file.tmp"
        else
            file=$(mktemp -u)
            mkfifo "$file"
        fi
        stdbuf -oL yt-dlp $ytdlp_args --print "%(webpage_url)s" "$query" \
               > "$file" &  # line-buffered cache
        ytdlp_pid=$!
        [ -n "$should_save" ] && finalise_cache_fork
    fi
fi

[ -z "$pos" ] && pos=0

# Run mpv
mpvargs="$mpvargs --idle=once --input-ipc-server="
if [ "$rawoutput_flag" = 1 ]; then
    mpv $mpvargs"$soc" &
else
    mpv_output=$(mktemp -u)
    mkfifo "$mpv_output"
    mpv $mpvargs"$soc" > "$mpv_output" 2>&1 &
    handle_output "$mpv_output" &
fi

sleep 1
observe
preload_link "$(get_link_at_line_number "$pos")" 0 replace
wait
```

I'm going to stop there for now. Ideas for where to take it from there:
- Link expiry handling
- Maybe it would be nice to rewrite as a C program to try to make it work on Windows as well.

After all the testing I did, YouTube doesn't let me watch anymore without authentication. I thought let's try bilibili then... which I just assumed would work the same as YouTube, but it doesn't:

> [ffmpeg] https: HTTP error 403 Forbidden

The mirror links that yt-dlp gives with `--get-url` for bilibili links don't work, unlike the YouTube ones. This is a pretty fatal problem. but there must be a way for it to work, given that it does work with mpv's `ytdl_hook` (when you load a bilibili video in mpv directly or with something like gaplessmpv; webpage link given to mpv rather than mirror link), I'm [looking at it](https://github.com/mpv-player/mpv/blob/master/player/lua/ytdl_hook.lua) but don't understand, there is not even any mention of bilibili so there is no specific handling for it. If there are specific headers or something, the mirror links are loaded with `ytdl_hook` too, so why would it be different?

It might be because the headers need to be the same when getting the URLs. Check how the hook is calling yt-dlp.

Sometimes the link to the video works and the audio 403s, sometimes the other way around.

In the JSON `yt-dlp -J` output, for each link there are http headers. but there are a lot of formats and it's hard to manage that output. For now instead of going too deeply into it let's just get a video and audio, doesn't matter which format, and the headers for them.

With:
```sh
yt-dlp --print "%(requested_formats)j" URL
```
we get a JSON list with an object for the video and audio formats it selects by default (best), each structured like this:
```json
{"url": "[..]", "ext": "mp4", "fps": 25.0, "width": 1280, "height": 720, "vcodec": "hev1.1.6.L120.90", "acodec": "none", "dynamic_range": "SDR", "tbr": 594.923, "filesize": null, "quality": 64, "format_id": "30066", "format": "720P \u51c6\u9ad8\u6e05", "protocol": "https", "video_ext": "mp4", "audio_ext": "none", "abr": 0, "vbr": 594.923, "resolution": "1280x720", "aspect_ratio": 1.78, "filesize_approx": 172130335, "http_headers": {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36", "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "Accept-Language": "en-us,en;q=0.5", "Sec-Fetch-Mode": "navigate", "Referer": "https://www.bilibili.com/video/av33807412"}}
```
They each have an headers object, but they are identical.

With the loadfile options object we can pass user-agent and http-header-fields. In the manual, the example for how to pass the latter is:
```sh
--http-header-fields='Field1: value1','Field2: value2'
```

Attempt:
```sh
get_video_info() {              # $1 = link
    fmt='{"title": "%(title)s", "formats": %(requested_formats)j}'
    py="import sys,json; data=json.load(sys.stdin)
print(data['title'])            # (1) title
for format in data['formats']: print(format['url']) # (2,3) urls
# (4) headers in the format --http-header-fields expects
headers=[]
for k,v in data['formats'][0]['http_headers'].items():
    headers.append(f'\'{k}: {v}\'')
print(','.join(headers))
# (5) user agent
print(data['formats'][0]['http_headers']['User-Agent'])"
    yt-dlp --print "$fmt" "$1" | python3 -c "$py"
}
```
The output is video title in the first line, video url in the second line, audio url in the third, headers in the fourth, user agent in the fifth.

Resulting headers formatting:
```sh
'User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/139.0.0.0 Safari/537.36','Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8','Accept-Language: en-us,en;q=0.5','Sec-Fetch-Mode: navigate','Referer: https://www.bilibili.com/video/av33807412'
```

Then we can rewrite our `preload_link` function to use this:
```sh
preload_link() { # $1 = link. optional: $2 = index, $3 = loadfile flags
    info=$(get_video_info "$1")
    title=$(echo "$info" | head -1 | json_escape)
    video=$(echo "$info" | sed -n 2p)
    audio=$(echo "$info" | sed -n 3p)
    headers=$(echo "$info" | sed -n 4p)
    useragent=$(echo "$info" | sed -n 5p)
    i=-1
    [ -n "$2" ] && i=$2
    if [ -z "$video" ]; then
        "$NAME: Preload failed, empty video. yt-dlp output: $info"
        return 1
    fi
    flags="insert-at"
    [ -n "$3" ] && flags=$3
    ipc '{"command": ["loadfile", "%s", "%s", %s, {"force-media-title": "%s", "audio-file": "%s", "http-header-fields": "%s", "user-agent": "%s"}]}'\
        "$video" "$flags" "$i" "$title" "$audio" "$headers" "$useragent"
}
```
I've also changed the `ipc` function to echo the command so I can verify it is formatted as expected. It's correct, but sadly I still get 403. I also tried in a different order, like putting the user agent and headers first in the options, but it made no difference.

I am not sure http-header-fields needs to be passed in a string. I also tried a JSON array (with also changing the headers to be wrapped in double quotes `""` instead of single quotes `''` to be valid JSON), but got "argument options has incompatible type."

Possibilities:
- I am not passing it in correctly / it's not being taken into account
- Maybe test with curl to remove IPC syntax / ytdl hook doing something to it later out of the equation
- Something else [`ytdl_hook`](https://github.com/mpv-player/mpv/blob/master/player/lua/ytdl_hook.lua) is doing that I'm not

I've been reading ytdl hook but it's also not one-to-one, like it's not using IPC it's setting properties directly so it's not going to be that easy to try to follow the exact steps, and even if I do it might not be the same.

I tire of this. If I do continue with this it will have to be in another devlog.

[`queuedmpv` version control link where I will put future changes](https://github.com/plu5/dotfiles/blob/main/pm/scripts/queuedmpv)
(if I ever figure this out)

### yt-dlp search prefixes
While researching for this I came across [this SE question](https://unix.stackexchange.com/questions/750166/yt-dlp-mpv-chained-media-playback) by Gabe Devs, 2023.

Thanks to Gabe Devs, a comment by xebeche, and a 2021 comment by nathanfranke on [this askubuntu question](https://askubuntu.com/questions/1039040/how-to-download-multiple-videos-in-one-search):
```sh
mpv --ytdl-format=ba --no-video --msg-level=all=no,input=status,statusline=status --term-status-msg=$'\n''${time-pos}/${duration} [${file-format}] ${metadata/by-key/artist:}: ${media-title:${filename}} (${playlist-pos-1}/${playlist-count})' 'ytdl://ytsearch5:téléphone'
```
plays the 5 top YouTube search results for Téléphone, which unfortunately includes Lady Gaga, not just songs from the late 70s band. but anyway I thought it was pretty cool. Most of the things in the command are to show you information about what's playing in the terminal and the `--ytdl-format=ba` is to get the audio only, in best available format (bestaudio).

to just watch video search results:
```sh
mpv 'ytdl://ytsearch100:programmation de jeux vidéo'
```
There, 100 crap videos on game programming in French.

I guess it's kind of like a "do you feel lucky" mode for YouTube without having to launch the site and pollute your brain with all the horrible thumbnails of people looking shocked all the time. Instead, you pollute your brain with the actual videos which are likely to be not what you intended to find. so, on further thought, I don't recommend using this.

I kind of enjoy doing things like this:
```sh
mpv 'ytdl://ytsearch:ludum dare mais en français'
```
it worked, it really did give me a video about Ludum Dare in French.
("Ludum Dare - #2 : Dans une Galaxie Lointaine... - mpv")

```sh
mpv 'ytdl://ytsearch:ludum dare 中国'
```
It's indeed a video in Chinese, this time of playing Ludum Dare games rather than developing them.
("一款令人窒息的电车游戏 Let's Play ludum dare 42 Games! STUCK! - mpv")

et si ?
```sh
mpv 'ytdl://ytsearch:ludum dare mais en chinois'
```
no, obviously not, it's not a bloody LLM. It gave me a video in English.
("I Made a Game in 48 Hours! - mpv")

I will try to stop playing with this.

{% include note.html content='
> [!NOTE]
> There exist other search prefixes, see [yt-dlp/supportedsites.md](https://github.com/yt-dlp/yt-dlp/blob/master/supportedsites.md) <kbd>Ctrl-f</kbd> `prefix`.
> - minitv:season:
> - minitv:series:
> - bilisearch: (bilibili)
> - netsearch: (netverse)
> - nicosearch: (nico video)
> - nicosearchdate: (nico video newest first)
> - prxseries:
> - prxstories:
> - rkfnsearch: (rokfin)
> - scsearch: (soundcloud)
> - trovoclip:
> - trovovod:
> - gvsearch: (google)
> - ytarchive: (web.archive.org youtube)
> - yvsearch: (yahoo)
> - ytsearch: (youtube)
> - ytuser: (youtube user)
' %}

{% include fin.html %}
