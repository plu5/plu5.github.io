---
layout: post
title: 3 — Braille display concerns
date: 2026-02-15 10:18
modified_date: 2026-02-16 07:09
categories: site braille.el dotfiles
lang: en
redirect_from: /devlog/3
---

## Site
### Pygments custom language
To make braille blocks have their own class so I can set the font separately, I put `braille` after the three ticks that start a markdown code block which then rendered the blocks in this structure:
```html
<pre>
  <code class="language-braille">
  </code>
</pre>
```
CSS specifier `.language-braille` suffices. This is different than what it does with a language that's actually recognised, like `plaintext` (the default if no language is specified after the three ticks); in that case it puts the class on a container div:
```html
<div class="language-plaintext highlighter-rouge">
  <div class="highlight">
    <pre class="highlight">
      <code class="language-braille">
      </code>
    </pre>
  </div>
</div>
```
and in that case the CSS specifier needs to be `.language-plaintext pre`.

By the way I initially tried `dot` but that language exists, and caused all the braille to be placed in error spans, because presumably it's not valid syntax in that language.

### Braille display support
On yesterday's devlog you might have had the braille blocks out of alignment on your device and thought "it's that bad, the interpolation?"

I tried to fix it but it isn't that easy. The problem is not all fonts contain the braille block of characters and even fewer monospace fonts. Since I use real spaces for spaces instead of the empty grid braille character it breaks the alignment if a font that renders space with a different width is used, or two different fonts with different widths for the space and the braille. If you highlight the lines in the blocks you can check if this is the case for you; they should be all of the same length and if they are not, a different width is used for the space characters.

I see now why the empty braille character is usually used instead of spaces. That would also mean you can use braille in non-monospace fonts, so in environment that don't render text in monospace, like social media... I see now.

I could load an external font but I don't want to impose that.

### Testing on iOS
I don't have a web inspector on iOS nor access to a machine with macOS, so I use Shortcuts with action Safari 'Run JavaScript on Shortcut Input':
```javascript
const style = document.createElement('style');
style.type = 'text/css';
style.innerHTML = '.language-braille {color: red; font-family: "Courier";}';
document.body.appendChild(style);
completion();
```
with 'Show in Share Sheet' checked on in the shortcut settings, and then when viewing a web page on safari you can click on the share button, scroll down and have the option to run this shortcut.

Another useful one is edit webpage:
```javascript
document.body.contentEditable = "true";
document.designMode = "on";
completion();
```
This helped me [last week](https://github.com/plu5/plu5.github.io/commit/185d9947b6ad890495fa93f78d9f42b6ab82537c) when I was trying to work out why the disabling of word wrap on ASCII art blocks is broken in Safari iOS. I had the idea to try putting a space at the beginning of each line (I used this shortcut then edited directly on the page) and that somehow fixes it. Since then I always ensure there is a space at the beginning of each line in plaintext blocks, and I did the same in the braille blocks.

(I guess bookmarklets can also be used to achieve the same thing)

## braille.el
### Empty grid braille character
I would not really like to use the empty grid character because in most fonts you can't easily distinguish it from space and you would end up in situations where people have these characters strewn around in their buffers unknowingly. It's also a waste of bits!

Other than export considerations described in [Site:Braille display support](#braille-display-support), since [it's possible to use variable-width fonts in Emacs](https://emacs.stackexchange.com/questions/2774/using-both-fixed-width-and-variable-width-font-in-org-mode) we clearly need the option.

```elisp
(defcustom braille-use-blank-grid nil
  "Whether to use the blank grid character '⠀' instead of space.
This can be useful if you intend to use your artwork in an environment that
is not going to display it a monospace font.")
```

Example:
```braille
 ⠀⡰⠀⠀⠀⠀⠀⡄⢠⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⢀⠀⢰⠀⠀⠀⡀⠀
 ⠀⡧⢄⠀⣠⡀⠀⡇⢸⠀⢰⡖⡄⠀⠀⢸⢠⡀⡎⡤⡖⢎⡧⠂⡇⢠⢴⠁⠀
 ⠀⠁⠈⡀⠑⠉⠀⠁⠈⠀⠈⠒⠁⠀⠀⠈⠃⠉⠃⠘⠉⠈⠁⠀⠁⠘⠃⢀⠎
 ⢰⠀⢠⠃⠀⡀⠀⣀⡀⠀⠀⠀⡄⠀⠀⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⢸⠉⢹⠒⡆⡇⢀⢣⠀⠀⠀⠐⢳⡂⠀⡇⢰⢻⠎⡇⢰⡝⠂⠀⠀⠀⠀⠀⠀
 ⠈⠀⠈⠀⠁⠁⠈⠁⠀⠀⠀⠀⠀⠁⠀⠁⠈⠀⢀⠁⡀⠙⠂⠀⠀⠀⠀⠀⠀
 ⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⠒⠀⢀⠀⡀⡸⠀⡇⠀⡀⠀⠀⠀⠀⠀⠀
 ⠀⡏⢱⠄⢸⣣⠀⡏⡇⢸⡝⠀⢸⠒⠂⢸⣠⠃⡇⠀⡇⠀⢣⡎⠁⠀⠀⠀⠀
 ⠀⠁⠈⢠⠈⠁⠀⡏⠁⠀⠉⠁⠀⠁⠀⠈⠁⡀⠁⠈⠀⠀⠎⠀⠀⠀⠀⠀⠀
 ⢀⢤⡀⢸⠀⡅⢀⣁⠀⢀⣀⡀⢀⠤⣀⠀⣀⣹⠀⠀⢠⢀⡠⡄⠀⠀⠀⠀⠀
 ⠸⠎⢇⠘⠀⠃⠸⠎⡇⠸⠀⠇⠸⢍⡁⠘⠤⠎⠀⠀⠘⠸⠀⠇⠀⠀⠀⠀⠀
 ⠀⠀⠈⠁⠀⠀⠠⠒⠁⢠⠀⠀⠀⠀⠀⠀⢀⣀⣀⠀⡠⢄⢀⡀⢠⠄⡠⠤⠀
 ⠀⢠⠔⡜⢣⠀⢸⠉⡆⢸⡔⠁⡼⠂⠀⠀⢸⣛⡁⠀⡇⢸⠀⢣⡎⢀⣩⠃⠀
 ⠀⠈⠀⠁⠈⠀⠈⠉⠀⠈⠀⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
```

{% include note.html content="
> [!NOTE]
> This is using empty grid characters instead of space, but remember, self, we still need a real space at the beginning of each line to avoid word wrap on WebKit!
" %}

### Undo
Undoing in braille.el undid several strokes if drawn one after another without pause. To make each stroke its own undo like real painting applications, I put `(undo-boundary)` at the beginning of the interactive functions.

Before that I had tried `atomic-change-group` around the function, but as [jennykwan](https://emacs.stackexchange.com/questions/35454/atomic-undo-blocks#comment69750_35464) points out on SE, that's not what it does. It is for undoing the changes in the case of an error.

### Undo and redo keybindings
```elisp
(define-minor-mode braille-mode
  "Toggles global braille-mode.
Lets you draw in the buffer with braille dots using your mouse."
  :global t
  :lighter " ⣿"
  :keymap
  '(([down-mouse-1] . braille-mouse-draw)
    ([M-mouse-1] . undo)
    ([M-down-mouse-1] . ignore)
    ([M-S-mouse-1] . redo)))
```
I had to ignore <kbd>M-down-mouse-1</kbd> to get <kbd>M-mouse-1</kbd> to work, as otherwise it does secondary selection.

I think I may need to ignore <kbd>mouse-1</kbd> as well?

### Bounds
Added verifying that we are on a character, preventing drawing on a newline character, and optionally preventing drawing on other characters like non-braille characters, and not on space also for those that are using empty grid characters instead of space and don't want to be able to draw off-canvas.

I am checking this in `braille-insert-at-xy`
```elisp
 (if (eq char ?\n)
     (message "braille: out of bounds (newline character)")
   (if (or (and braille-consider-text-out-of-bounds
                (null d) (not (eq char ?\s)))
           (and braille-consider-space-out-of-bounds
                (eq char ?\s)))
       (message "braille: out of bounds (text)")
     (delete-char 1)
     (insert (+ #x2800 new-dot-value))))
```

And for checking we are on a character, I compare the xy coordinates of the initial posn to the coordinates of the posn obtained from the `char-pos` in it:
```elisp
(defun braille-in-bounds-p (posn)
  "If POSN is in bounds for braille drawing return t, nil otherwise."
  (let ((click-xy (posn-x-y posn))
        (char-xy (posn-x-y (posn-at-point (posn-point posn))))
        (rel-wh (posn-object-width-height posn)))
    ;; (message "bounds calc %s %s %s" click-xy char-xy rel-wh)  ; debug
    (and (<= (car click-xy) (+ (car char-xy) (car rel-wh)))
         (<= (cdr click-xy) (+ (cdr char-xy) (cdr rel-wh))))))
```
but calling this in `braille-insert-at-xy`, the posn we give is calculated from the argument received, which is normalised, not the posn from the click event. I think this can probably be removed in favour of only checking the character.

I also considered checking this in the interactive drawing functions but maybe it's desireable to be able to start a stroke from outside the canvas.

### Custom group
To be able to customise our defcustoms in <kbd>M-x</kbd> `customize-group`, we have to define a group and attach our defcustoms to it:
```elisp
(defgroup braille nil
  "Braille drawing engine."
  :group 'mouse)

(defcustom braille-use-blank-grid nil
  "Whether to use the blank grid character '⠀' instead of space.
This can be useful if you intend to use your artwork in an environment that
is not going to display it a monospace font."
  :type 'boolean
  :group 'braille)

(defcustom braille-consider-text-out-of-bounds t
  "Avoid drawing on characters that are not either braille or space."
  :type 'boolean
  :group 'braille)

(defcustom braille-consider-space-out-of-bounds nil
  "Avoid drawing on space.
Expected to be used in combination with `braille-use-blank-grid' t."
  :type 'boolean
  :group 'braille)
```

I chose parent group `mouse` to be consistent with `artist-mode`, who puts its defcustoms under there also.

### Problem in `markdown-mode`
I noticed yesterday that in `markdown-mode` code blocks braille.el is broken. Clicking works, but dragging (so, interpolation) puts down a vertical line and a message:

> min: Arithmetic error

In `org-mode` code blocks there isn't this problem.

### Performance problems
Drawing a long line quickly lags behind in certain situations.

```braille
 ⠘⢇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠘⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢣⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⢣⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢆⠀⠸⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⠘⡄⠀⠀⠀⣀⢄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢆⠀⢱⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⠀⢣⠀⠀⢠⠃⠈⢆⠀⠀⠀⠀⠀⠀⣀⡀⠀⠀⠀⠀⠈⢆⠀⢣⠀⠀⠀⠀⠀⠀⠀⣀⠤⣴⠀⠀⠈⢢⠀⠀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⠀⠘⡄⠀⢸⠀⠀⠈⢆⠀⠀⠀⠀⢸⠀⠈⢆⠀⠀⠀⠀⠈⡆⠀⢣⠀⠀⠀⠀⢀⠎⠀⡰⠁⢣⠀⠀⠀⠱⡀⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⠀⠀⢣⠀⠸⡀⠀⠀⠈⢆⠀⠀⠀⠸⡀⠀⠀⢇⠀⠀⠀⠀⠘⡄⠈⢆⠀⠀⠀⡎⠀⡜⠀⠀⠀⢇⠀⠀⠀⢣⠀⠀⠀⠀⠀⠀⠀
 ⠀⠀⠀⠀⠀⢇⠀⡇⠀⠀⠀⠈⢆⠀⠀⠀⢱⠀⠀⠸⡀⠀⠀⠀⠀⠘⡄⠈⢆⠀⠀⢘⡎⠀⠀⠀⠀⠈⢆⠀⠀⠈⢆⠀⠀⠀⠀⠀⠀
 ⠀⠀⠀⠀⠀⠘⡄⢣⠀⠀⠀⠀⠈⢆⠀⠀⠀⢣⠀⠀⡇⠀⠀⠀⠀⠀⡜⡄⠈⢆⠀⢎⠘⢄⡀⠀⠀⠀⡜⠀⠀⠀⠈⢢⠀⠀⠀⠀⠀
 ⠀⠀⠀⠀⠀⠀⠘⡜⡄⠀⠀⠀⠀⠈⢆⠀⠀⠀⠣⣠⠃⠀⠀⠀⠀⡸⠀⠘⡄⠈⠢⡈⠂⠀⠈⠒⠢⠴⠁⠀⠀⠀⢀⣨⠗⠒⢆⠀⠀
 ⠀⠀⠀⠀⠀⠀⠀⠘⣧⠀⠀⠀⠀⠀⠈⢆⠀⠀⣀⠼⢄⠀⢀⣀⠔⠁⠀⠀⠈⢆⠀⠑⢄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢅⠀⠀⢠⠃⠀
 ⠀⠀⠀⠀⠀⠀⠀⠀⠘⣆⠀⠀⠀⠀⠀⠈⠒⠉⠀⠀⠀⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠁⠀⠀
 ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
```
in this case it didn't but I noticed it in the previous test `(╯°□°)╯︵ ┻━┻`

## Dotfiles
### mpv stdout
I have a [script](https://github.com/plu5/dotfiles/blob/main/pm/scripts/soncontrol) I wrote in January to control audio files playing in the background, for which I used backgrounded mpv processes.

Yesterday when checking my system logs I saw my window manager logs were really huge, and found out it was because mpv prints out position messages like:
```
A: 01:04:21 / 10:09:32 (11%) Cache: 3649s/150MB
```
every second. Argument [`--quiet`](https://mpv.io/manual/master/#options-quiet) sorts it. There is also `--really-quiet`.

And to be honest there has never been anything useful in my window manager (bspwm) logs, it doesn't seem to display any messages of its own, I can only see in there backgrounded processes like polybar complaining about characters it doesn't like and other such useless things. If I cared about a backgrounded process output I would be redirecting its output myself, or running it in a terminal. I should maybe stop saving it. I'm leaving it for now.

(Maybe if there was an error to do with it then there _would_ be a message. Presumably in stderr so maybe save only stderr?)

### Elisp return
In Emacs Lisp there isn't a return, [apparently people throw errors for this](https://emacs.stackexchange.com/questions/7823/how-do-you-return-from-a-function-at-an-arbitrary-point), but I have not used that so far. So far I have just written my functions in a way to avoid having to return, using conditions instead, but at some point there may be a situation where I can't avoid it.

Previously I could use `return` with no consequence, even returning a value, like `(return (car j))`. I didn't realise it wasn't part of Elisp. Not sure what changed, but it stopped working recently, throwing error `No catch for tag: --cl-block-nil--`. Just avoid using it.

{% include fin.html %}
