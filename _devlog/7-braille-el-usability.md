---
layout: post
title: 7 — braille.el usability
date: 2026-02-22 06:13
modified_date: 2026-02-22 21:00
categories: braille.el
lang: en
redirect_from: /devlog/7
---

## braille.el
I did continue working, just without writing. so let me write.

### Gif
I wanted to make a little gif for the readme.

I used [Peek](https://github.com/phw/peek) to record the gif.

to position it precisely I do as I describe in my [linux notes](/notes/k/linux) (C-f peek)

to see current position of a bspwm floating window:
```bash
[pm@pos ~]$ wid=$(wmctrl -l | grep Peek | awk '{print $1}'); bspc query --node $wid --tree | jq .client.floatingRectangle
{
  "x": 635,
  "y": 339,
  "width": 657,
  "height": 377
}
```
to reposition (one of many ways):
```bash
wmctrl -r Peek -e 0,635,339,657,400
```

I wanted to show the entire process of launching emacs -Q and loading the package. So before starting to recrd the gif I used this oneshot rule:
```bash
bspc rule -a '*' -o state=floating rectangle=657x360+635+300
```
Then <kbd>C-l</kbd> to clear terminal then <kbd>C-M-r</kbd> to start recording (same combination to stop).

Here is the gif if you want to see it (versioned link so if I improve it in the future you will still see the one I recorded today):

![The gif I made for braille.el](https://github.com/plu5/braille.el/blob/23f36828fef8d9f535a627c7d5ee0da7b63e19b2/demo.gif?raw=true)

It's a bit shit and the dimensions are wrong on the Emacs window but I guess it's better than nothing. Eventually I want to record a proper demo.

That's all I wanted to do before leaving braille.el for now and moving on with my life, but working on the gif made me realise some bugs and usability improvements I could make, which I then had to do immediately.

### Bug in erase
I noticed that `braille-e-stroke-erase` in I forgot somehow changing the call to `braille-mouse-draw` to `braille-e-stroke`. Bug from the refactoring in the last devlog. It should not have happened and I'm ashamed and I fixed it and other than that everything is fine ok??????????

(You can see the slow decline here, at the start you have bugs that you really must fix now because wtf, then bugs that are less wtf but still let's fix this asap, then quality of life stuff that I have an idea for how to implement easily and is only going to take a couple of minutes, and then: remember animations? and the fixation is back.)

### point-max bounds
braille.el had out-of-bounds calculation, but I noticed an error when trying to draw below the end of the buffer:

> let: Wrong type argument: number-or-marker-p, nil

The first thing I tried to do was add to `braille-in-bounds-p` a check like this:
```elisp
(<= (posn-point posn) (point-max))
```
but it was ineffective.

I discovered that when clicking anywhere past the last point, `char-pos` (i.e. `(posn-point)`) is always equal to `(point-max)`.

I thought `braille-in-bounds-p` in any case should detect it, because it checks `click-xy` against `char-xy`. but it seems these positions get normalised also.

I think it's impossible to have a character on `(point-max)`, so we would never be able to draw on it anyway. so just add this to the `braille-in-bounds-p` verifications:
```elisp
(< (posn-point posn) (point-max))
```

Now it's fine past the end of the buffer, but if you drag towards the minibuffer or towards another window, the drag stops and we get a similar error:

> +: Wrong type argument: number-or-marker-p, nil

or:

> /: Wrong type argument: number-or-marker-p, nil

This I don't know how to solve yet. It seems the `posn` in the events still has the old window before the error, so the naïve idea of just checking `posn-window` is not going to do the job.

### Undo and redo keybindings
Another thing that was broken was the undo and redo rebindings.
- `undo` in emacs -Q sometimes does redo (It does [some weird shenanigans with the undo stack](https://www.gnu.org/software/emacs/manual/html_node/emacs/Undo.html). Few people understand how it works, it's one of the eccentric things Emacs has by default that everyone changes in their configuration). Use `undo-only` instead to only undo.
- `redo` doesn't exist, it should have been `undo-redo`

### Mouse keybindings
Other than <kbd>mouse-1</kbd> and <kbd>down-mouse-1</kbd>, there is apparently also <kbd>drag-mouse-1</kbd> (and same for <kbd>mouse-2</kbd> and so on). Whenever binding one of them, we should ignore all the others.

Despite having ignored them all, I still have an issue with the mouse. If we press a mouse button for long time, we get this message in the minibuffer:
```
down-mouse-1- (C-h for help)
```
and after letting go:
```
down-mouse-1 mouse-1
```

This actually doesn't happen without braille-mode, so there must be a way to avoid it. Maybe it's because `ignore` makes it be treated as a prefix? but <kbd>down-mouse-1</kbd> is actually not bound to ignore, that's the one I have bound to stroke, so why does it think it's a prefix?

Here are all the mouse bindings:
```elisp
    ([down-mouse-1] . braille-e-stroke)
    ([mouse-1] . ignore)
    ([drag-mouse-1] . ignore)
    ([C-down-mouse-1] . braille-e-stroke-erase)
    ([C-drag-mouse-1] . ignore)
    ([C-mouse-1] . ignore)
    ([M-mouse-1] . undo-only)
    ([M-down-mouse-1] . ignore)
    ([M-drag-mouse-1] . ignore)
    ([M-S-mouse-1] . undo-redo)
    ([M-S-down-mouse-1] . ignore)
    ([M-S-drag-mouse-1] . ignore)
```

I tested on several fresh instances and it still happens, including on emacs -Q :-(

### Auto canvas size
As obvious in the gif, it's a little annoying to work out the canvas size you want when you want to just fill the whole screen. I was always vaguely aware of this but thought it's only a one-time annoyance as you figure out what size you like and set that as the default and then just use <kbd>C-c C-v</kbd>. but I think it's going to be a common desire, and certainly is for me for demos, to just fill the screen.

and this is actually very easy to do, because `(window-width)` and `(window-height)` give the results in lines and columns.

{% include note.html content='
> [!NOTE]
> but these functions sadly do not account for text scale and give results too large if the scale is >0 and too small <0.
' %}

How to give the possibility for automatic calculation without breaking the current canvas size interface? The solution I came to is to take 0 to be "calculate it for me", as there could never be any reason someone would want to have a canvas of width/height 0.

``` elisp
(defun braille-create-canvas-at-point (size)
  "Create an area of whitespace with given dimensions."
  (interactive
   (list (split-string
          (read-string "Canvas size (0=auto): " braille-default-canvas-size)
          "x")))
  (unless (= (length size) 2)
    (user-error "Expected canvas size format: WxH (ex. 50x20)"))
  (let ((w (braille-or0 (string-to-number (car size)) (window-width)))
        (h (braille-or0 (string-to-number (cadr size)) (window-height)))
        (c (braille-empty-char)))
    (dotimes (i h)
      (insert (make-string w c) "\n"))
    (message "Created %sx%s canvas with %s character" w h
             (braille-char-name-or-char c))))
```
Only two lines here changed, the `w` and `h` in the let [actually 3, I also changed the prompt in the interactive `read-string` to explain what 0 now means]. because 0 in elisp is sadly truthy, I'm using this utility function instead of `or` to avoid unnecessarily complex logic in there:
``` elisp
(defun braille-or0 (v default)
  "Return V if V is nonzero, DEFAULT otherwise."
  (if (and (numberp v) (zerop v))
      default
    v))
```

I also, with a bit of a heavy heart, changed the default canvas size to 0x0, because I think an automatically calculated canvas size better matches user expectations. Afterwards they might wish to change it, but when they first test this package they just want to draw and they probably expect it to fill the screen.

### Pointer
It's also evident in the gif that the mouse pointer is nonideal. In my actual Emacs configuration it never bothered me much because the pointer is a little hand rather than the giant `I` icon (but I don't actually know why that is, I never change the pointer anywhere in my configuration, nor does the theme. Some package changes it?)

My first thought was to apply text properties on the canvas but it's actually a bad idea because we would have to do it every draw because we're constantly replacing the characters as we're drawing.

I noticed that there are a lot of `x-pointer-*` variables, like `x-pointer-boat`, `x-pointer-bogosity` (the fuck is that?), `x-pointer-dot`, `x-pointer-crosshair`. They're integers. For example the ones I mentioned are respectively 8, 10, 38, 34. You can set `x-pointer-shape` to them, and then... nothing happens. because:

> Changing the value does not affect existing frames
unless you set the mouse color.

so you have to do something like `(set-mouse-color "red")` as well, and then yes, it updates.
```elisp
(setq x-pointer-shape x-pointer-crosshair)
(set-mouse-color "red")
;; ^ pointer still black after this with the crosshair, but some of
;; the other pointers do change to red
```
much better -- for drawing, anyway, not outside of that. so we have to be very careful to change it back and also have this be optional.

but does this even work outside of Xserver?

Too late, I already implemented and committed the logic plus a couple of fixes before having the thought "hang on a minute, x..." and I've still not tested it in other environments so I don't know. (but what would happen? at worst nothing? I certainly hope so)

On emacs -Q `x-pointer-shape` is nil by default. and this is a valid value, it corresponds to the `I` seemingly. I don't really understand how this works, maybe it suggests that this is actually a way to override the pointer, and if it's nil it's something else that handles it?

and what happens when you `(set-mouse-color nil)`? seemingly nothing.

```elisp
(defcustom braille-pointer x-pointer-crosshair
  "Mouse pointer in `braille-mode'.
`braille-mode' sets `x-pointer-shape' to this value when activated, and
back to what it was set to before when deactivated. For possible values
see `x-pointer-*' variables like `x-pointer-arrow',
`x-pointer-crosshair', `x-pointer-dot', `x-pointer-circle'. Set this to
nil to not change the pointer."
  :type 'natnum
  :group 'braille)

(defcustom braille-mouse-color "red"
  "Color to give `set-mouse-color'.
It is used to make `x-pointer-shape' update without having to create a
new frame. This only changes the color with some pointer shapes."
  :type 'string
  :group 'braille)

(defvar braille-prev-pointer nil
  "Storage for previous value of `x-pointer-shape'.")
(defvar braille-prev-mouse-color nil
  "Storage for previous value of mouse color in `frame-parameters'.")
(defvar braille-needs-to-reset-pointer-flag nil
  "Whether the pointer had been changed by braille.el and not yet reset.")

(defun braille-set-pointer ()
  "Set `x-pointer-shape' to `braille-pointer', saving its previous value.
Mouse color is changed as well because it's required for it to update."
  (setq braille-prev-pointer x-pointer-shape)
  (setq x-pointer-shape braille-pointer)
  (setq braille-needs-to-reset-pointer-flag t)
  (setq braille-prev-mouse-color (frame-parameter nil 'mouse-color))
  (set-mouse-color braille-mouse-color))

(defun braille-reset-pointer ()
  "Reset `x-pointer-shape' and mouse color.
Using `braille-prev-pointer' and `braille-prev-mouse-color'."
  (setq x-pointer-shape braille-prev-pointer) ; nil is a valid value
  (setq braille-prev-pointer nil)
  (setq braille-needs-to-reset-pointer-flag nil)
  ;; we have to always set it, even if it's nil, as otherwise the
  ;; pointer will not update. but even on emacs -Q it's not nil, it's
  ;; "black"
  (set-mouse-color braille-prev-mouse-color)
  (setq braille-prev-mouse-color nil))
```

and now at the end of the `define-minor-mode` macro you can put stuff that will execute when it gets toggled.
```elisp
  (if braille-mode
      (if braille-pointer (braille-set-pointer))
    (if braille-needs-to-reset-pointer-flag (braille-reset-pointer)))
```

Some explanation is maybe needed for the `braille-needs-to-reset-pointer-flag`. We can't check `braille-pointer` for that because if the user turned it off while braille-mode is on, when they turn it off the pointer will not reset. before making a flag I tried making a function `braille-should-reset-pointer-p` but the logic is too complicated (if prev-pointer is nil, then either it was really nil before or it was not changed. if pointer is nil user disabled changing the pointer. if prev-mouse-color is nil... i don't know, at that point i gave up and just went with a flag)

and the reason I called it flag is a recommendation I saw in the manual conventions that I read a few devlogs ago. that boolean functions are, as we all will have encountered enough times to know, `*-p`, and boolean variables should be `*-flag`, which I had actually never encountered, but yes it seems other packages use this convention.

This logic is imperfect because setting the pointer only affects the current frame. If the user turns on braille-mode, switches frame, and turns it off there, the pointer will never be reset in the original frame.

### Docs
Added sections:

(1) customisation examples, for both vanilla and use-package users. to show how to change or disable bindings as I realise my choice of default ones are invasive, and common things users will probably want to customise like `braille-default-canvas-size` and `braille-use-blank-grid`.

(2) known issues, which is actually to list problems I don't want to put on the roadmap because I'm not sure if I should fix them. which, at the moment of writing this, are the following:

- Doesn't work in terminal Emacs (emacs -nw)
  + This is due to dependence on pixel positions. Does terminal have a way to tell where relative to the character a click has occurred? If not, then it's impossible to make braille.el work in terminal.
- Changing pointer only affects the current frame
  + Associated issue: If the user turns braille-mode on on one frame and turns it off on another it will not reset on the first
- Automatic canvas size calculation doesn't take account of text scale
  + If your text scale is negative, the automatic canvas will be too small, and if it's positive it will too large -- and it's this last one which could be a big issue if there are a lot of people who use Emacs with a permanently large text scale rather than changing font size directly + use word wrap, as their canvases will be broken if they use automatic calculation for the size (which is currently the default).
  + Being able to do just `(window-width)` and `(window-height)` for the calculation is so elegant and convenient that I am hesitant to fix this.

### Feelings on current state
I have drawn with it a lot [cf [notes/pers/art-ascii](/notes/pers/art-ascii)] and I have mixed feelings. It's annoying to try to erase or add a particular dot. I don't know if I can improve this. I think it would help to be able to change brush size, and we'll like edge into it. but it might be the lack of mouse events for small slow movements. a click should work. maybe we need some kind of feedback for when it "registers"?

I still use it a tonne and in at least one way overly-optimistic me from the start was right that the fact that it's so "cheap", so easy to make drawings, modify, share, track changes in git, the light-weightedness of it makes me want to use it more than real painting software. Obviously we barely have any resolution or colour, but these limitations are kind of fun, and it captures the "essence" of drawing. The big shapes, composition, concentration of detail, noise vs signal.

Right now it really feels like pencil drawing with the way you have to plan out your canvas to be able to fit what you want to draw :-p

I really want selection, transformations, animation.

You can't make any serious images, part of my brain is saying this is a waste of time compared to making something serious in even for example svg like people on Behance. Like why are you drawing with dots, when you could be making stuff that looks sharp and colourful for a portfolio and maybe be able to get a job with it? On the other hand, a lot of imagemaking is exploration of shapes, studying things, repetition. Most of the time drawing gets wasted, very little of it is actual productive "portfolio piece". so in a way it's kind of natural and this kind of "timewasting" will happen regardless. One isn't limited to a particular tool and it can be interesting to use different ones with different limitations, different ways of looking at things. This one, as I said, is really effective in teaching signal vs noise because with the low resolution and only two tones, every dot matters.

I remember Adrian Bush talking about starting his paintings in very few tones (3-4) and "recycling" them, like erasing out instead of putting down another tone, which is essentially what you have to do in braille. with the idea that if the picture doesn't read in that number of tones it is not going to read when you add more.

Another problem is line height: oh the pain when you have a key detail fall directly on the separation between lines! This can be adjusted in Emacs, and also on the web with CSS, so we could technically completely overcome this issue, but then a piece will not be quite correct in all environments (if you reduce line height then in an environment with a standard line height it will be stretched vertically). Maybe this is an inseparable part of this art form anyway, because as it is already everything is going to vary from environment to environment (all the different variables of text, line height included, are not some kind of standardised size everywhere) and font to font, it is displayed in different colours, stretched or narrowed.

Another problem is canvas size. It's both font and line height related. If you zoom out the points are barely visible so in practice you're very limited by the size of your window / screen for the resolution of your drawing and how much detail you can have. Also, less critical is, as I mentioned earlier in known issues, that the automatic canvas size calculation I just implemented doesn't take account of text scale so that if your scale is negative (zoomed out) it is not large enough and if it's positive (zoomed in) it's broken if you have word wrap on.

## Other
Emptiness and void. After today I will again attempt to avoid working on braille outside of weekends. If I do work on it, I will try not to put off writing like I did this time, it was like I was trying to convince myself it's small what I'm doing, it's not really work, so it doesn't need a devlog. but even small things I'd rather write to dump it off my brain and it's easier to do it right after instead of having to catch up.

As you see my capitalisation is a bit inconsistent because it feels weird to me to capitalise words like 'and', 'so', 'but' when they start a sentence

is it better to avoid capitalisation altogether, like this? this is how i write naturally to be honest and i force myself to capitalise for the devlogs hence why i am shit at capitalising properly. i also usually don't use much punctuation as you can see in the last sentence and in this one. if i go full out like this it might be very offputting for some people, but aren't devlogs for me? yes and no.

I think I'm going to make some effort to write properly anyway. but I don't know what to do about the 'and', 'so', 'but'. Maybe it's ok to just not capitalise those ones.

{% include fin.html %}
