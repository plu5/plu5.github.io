---
layout: post
title: 5 — braille.el spring cleaning
date: 2026-02-17 18:47
modified_date: 2026-02-20 18:32
categories: braille.el dotfiles site
lang: en
redirect_from: /devlog/5
---

## braille.el
### Spring cleaning
It isn't spring but the core functionality of braille.el is done and I think it is a good time to try to clean up the API a bit.

#### Header
Firstly, because I read manual page [Library Headers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html), I added more to the header, like lexical binding, author, URL, "Keywords: mouse", "This file is not part of GNU Emacs."

Now for refactoring.

#### Split `braille-insert-at-xy`, posn utility functions
The main logic for placing or erasing a point (which is the meat and potatoes of this) is called `braille-insert-at-xy`. Part of me wants to change it to something shorter due to the importance of this function, and due to it being used for erasing also, but the way erasing is done is also by using `insert`, so insert is correct. But it doesn't just insert, it does other things like check bounds (but it doesn't check the bounds itself, it calls another function), get `char-pos` with `xy`, `goto-char`, figure out new char based on existing and bitwise operations. Should this be split for clarity? I know about "separation of concerns", but I don't act out of dogma.

Since I'm doing quite a lot of `(posn-at-x-y (car xy) (cdr xy))` all over the code, I'm going to write a function for this, `braille-posn-at-xy`.

I also do several times `(posn-x-y (posn-at-point (posn-point posn)))` to get the xy pixel coordinates of a character. It's a way to get the top left coordinates of the object, as otherwise `(posn-x-y posn)` gives you the event coordinates (where on the window the user clicked), and `(posn-object-x-y posn)` gives you relative coordinates (where on the object the user clicked). We use the top left character position (1) to calculate dotspace (`braille-posn-to-dot-xy`), (2) to check if the click happened in bounds (are we on a character?) (`braille-in-bounds-p`). I will call this `braille-posn-char-xy`. Even though I think it doesn't have to be a character, in our use it is.

Now for the gutting of `braille-insert-at-xy`. Split into:
1. `braille-legal-char-p` : takes `char`, returns t or nil based on whether we consider it in bounds (braille.el avoids replacing newline characters and can avoid drawing on other characters depending on the values of `braille-consider-text-out-of-bounds` and `braille-consider-space-out-of-bounds`)
2. `braille-onto` : takes `char-pos`, `dot-bit`, and `erase`, calls `braille-legal-char-p` to check if character at `char-pos` is one we should change, and if so does the calculation and replacement. I thought to make it just return the character to change to, or nil, but it needs access to all the information necessary to also do the replacement and it is only two extra lines, so it makes no sense to separate that logic. Only if it was necessary sometimes to know what the new caracter would be without doing the replacement, but I can't imagine such a situation.
3. and now `braille-insert-at-xy` just gets `char-pos` and `dot-bit`, checks `braille-in-bounds-p`, and calls `braille-onto`. For coherence I'm going to rename it `braille-onto-xy`.

#### What to do about change hooks
Another thing I'm going to do is remove setting `inhibit-modification-hooks` to t. Saw in the help for this variable:

> To delay change hooks during a series of changes, use
‘combine-change-calls’ or ‘combine-after-change-calls’ instead of
binding this variable.

In the help for `combine-change-calls`:

> Additionally, the buffer modifications of BODY are recorded on
the buffer’s undo list as a single (apply ...) entry containing
the function ‘undo--wrap-and-run-primitive-undo’.

so it seems like we can replace `undo-boundary` with this in `braille-draw-line` and `braille-mouse-draw`.

It's not as simple as that because `combine-change-calls` takes two other arguments: BEG and END to "bound the region the change hooks will be run for." That isn't that simple for us to calculate, and is it going to be worth it?

`combine-after-change-calls` only takes BODY, but might be ineffective:

> I think it might be safe, but it probably
won't make a difference: combine-after-change-calls is ignored when
before-change-functions is non-nil.  
—[Stephan Monnier, emacs-devel 2019](https://lists.endsoftwarepatents.org/archive/html/emacs-devel/2019-04/msg00754.html)

Help:

> If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

and `combine-change-calls` does not have this restriction:

> One thing which might help in the future is the macro
combine-change-calls, which is a bit like combine-after-change-calls,
but also works when before-change-functions isn't nil.
—[Alan Mackenzie, emacs-devel 2019](https://lists.endsoftwarepatents.org/archive/html/emacs-devel/2019-04/msg01107.html)

but will make us dependent on 27.1 which came out in 2020. As well as having to work out beg and end.

Welp I guess I will use `inhibit-modification-hooks` after all, with a defcustom to control this in case this was really undesireable in some usecases. The effects of this:
- "‘before-change-functions’ and ‘after-change-functions’, as well as hooks attached to text properties and overlays" won't happen at all
- inhibits file locks and related checks
- inhibits "handling of the active region per ‘select-active-regions’."

```elisp
(defcustom braille-inhibit-modification-hooks t
  "Whether to set `inhibit-modification-hooks' to t while drawing.
This improves performance at the cost of ignoring
`before-change-functions', 'after-change-functions', hooks attached to
text properties and overlays, file locks and related checks, and
handling of the active region per `select-active-regions'."
  :type 'boolean
  :group 'braille)

(defun braille-inhibit-modification-hooks-p ()
  "Return value `inhibit-modification-hooks' should be set to while drawing."
  (if braille-inhibit-modification-hooks t inhibit-modification-hooks))
```

Then in the lets in stroke and line drawing functions:
```elisp
 (inhibit-modification-hooks (braille-inhibit-modification-hooks-p))
```

#### E
The next thing I have a problem with is the functions with 'click' in the name. It technically doesn't have to be a click, `read-event` accepts any input.

One approach would be to rename e.g. `braille-click` to `braille-input`, but I find that could be a bit confusing. I want to name it `braille-e`, though I realise it is even more obtuse at first encounter, it could be a consistent labelling for the functions that are `(interactive "e")` and need to be bound to a mouse button or keyboard key.

{% include note.html content='
> [!NOTE]
> As I notice later, it doesn\'t actually work to bind an `(interactive "e")` function to a keyboard key.
' %}

Since `braille-click` is not actually used for anything—it was the first interactive function I had implemented but has since been supplanted by the stroke drawing function—I will rename it `braille-e-single` rather than just `braille-e` (I could remove that function altogether but I'm keeping it because there could still be usecases for wanting to bind a function that places a single dot). We probably don't want a function called `braille-e` because (1) it's obtuse, (2) we want to be able to say "the `braille-e` functions are the ones you bind to a key" without having to specify "all the `braille-e-*`, not specifically the one called `braille-e`."

`braille-click-debug` is now `braille-e-debug`. `braille-draw-line` is now `braille-e-line`. `braille-mouse-draw` is now `braille-e-stroke`. `braille-mouse-erase` is now `braille-e-stroke-erase` (because that's what it does, the same thing as stroke just with erase set to t).

and what of all the references to the mouse in docstrings, should they be renamed to be agnostic? and all the "E should be a mouse down event"? The reason it is there is those functions have to be bound to, for example, `down-mouse-1`, not `mouse-1`, or the drag will not work. Would that also work with a keyboard key?

I tried:
```elisp
(global-set-key [down-kp-next] (lambda () (interactive) (message "hi")))
(global-set-key [kp-next] #'ignore)
```
but nothing happens.

and I guess it's not going to work anyway with anything other than a mouse because we are doing `track-mouse` too to tell while it is dragged and when it is let go.

Even something like:
```elisp
(global-set-key [kp-next] #'braille-e-debug)
```
doesn't work:

> braille-e-debug must be bound to an event with parameters

so only binding to mouse buttons works with `(interactive "e")`?

#### Misc
Reword docstrings for consistency.
- First line should always end in a period
- Refer to arguments in all caps

Rename `braille-create-canvas-at-point-without-asking` to `braille-create-canvas-at-point-unprompted` because it's too long. and yes that 4-characters saving is significant, helps us get the line under 80 characters in the keymap (exactly 79) without adding a linebreak! hurrah

Rename `braille-insert-at-dot-xy` to `braille-onto-dot-xy` for consistency.

We have a function called `braille-line` and a function called `braille-dotspace-line`. `braille-line` had the interpolation in the days before dotspace, but nowadays just converts pixel coordinates to dotspace and calls `braille-dotspace-line`. Rename it to `braille-xy-line`, and `braille-dotspace-line` to `braille-dot-xy-line`.

### Tangent reading some manual pages
I read about [double hyphen convention](https://emacs.stackexchange.com/questions/42286/double-hyphen-in-elisp-function-names) for naming internal functions. I have an aversion to this, but maybe it will help clarify which functions are "top level" and which are "helpers".

[I decided not to use this convention. I find the distinction arbitrary.]

Then read the following pages of the manual:
- [Coding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)
  + "Please put a copyright notice and copying permission notice on the file if you distribute copies. See [Conventional Headers for Emacs Libraries](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html)." : This is something I haven't been doing until now and I need to work out how to do.
  + "For variables holding (or functions returning) a file or directory name, avoid using path in its name, preferring file, file-name, or directory instead" : This is an eccentric convention. I will continue calling it path because it's more widely understood.
- [Packaging Basics](https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html)
- [Tips for Defining](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips-for-Defining.html)
- [Minor Mode Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html)
- [Major Mode Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html)
  + "The documentation string may include the special documentation substrings, ‘`\[command]`’, ‘`\{keymap}`’, and ‘`\<keymap>`’, which allow the help display to adapt automatically to the user’s own key bindings. See [Substituting Key Bindings in Documentation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Keys-in-Documentation.html)."
  + "Consider adding a mode-specific menu to the menu bar. This should preferably include the most important menu-specific settings and commands that will allow users discovering the main features quickly and efficiently." : I should do this, especially considering that braille.el is a mode largely operated by mouse.
  + "Consider adding mode-specific context menus for the mode, to be used if and when users activate the context-menu-mode (see [Menu Mouse Clicks](https://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Mouse-Clicks.html#Menu-Mouse-Clicks) in _The Emacs Manual_). To this end, define a mode-specific function which builds one or more menus depending on the location of the mouse-3 click in the buffer, and then add that function to the buffer-local value of context-menu-functions."
- [Autoloads](https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html)
  + I don't really understand if I'm meant to be using this and for what and for what reason. I have seen `;;;###autoload` a lot in other people's packages. Is it to speed up initialisation time for people who `require` your package?
- [When to Autoload](https://www.gnu.org/software/emacs/manual/html_node/elisp/When-to-Autoload.html) : still unclear
- [File Local Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html)
- [Keymaps and Minor Modes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps-and-Minor-Modes.html)
- [Key Binding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) : I am going to ignore these conventions.
- [Selecting Lisp Dialect](https://www.gnu.org/software/emacs/manual/html_node/elisp/Selecting-Lisp-Dialect.html) lexical-binding
- [Converting to Lexical Binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Converting-to-Lexical-Binding.html) : byte-compile and check warnings
- [Byte Compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html) : already used it before
- [Lexical Binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html) : cool ticker example
- [Dynamic Binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html)

### Performance
A thought for why braille is slow sometimes and other times fast: speed of character insertion. It's slow in markdown-mode, because [markdown-mode has input lag](https://github.com/jrblevin/markdown-mode/issues/799). In lisp-interaction-mode or org-mode it seems to be fast. But I need to do more testing on long buffers like do some drawing in my ascii note, but change to org-mode first, then change to markdown-mode if you want to verify that that is the source of slowness.

That suggests that it's the speed of inserting characters that is the bottleneck, not the interpolation, which is good news.

Also need to work out what seems to cause division by 0 sometimes (Arithmetic error)

### Ideas
(1) for how to do the animation. separate canvases on the same buffer with a particular line above. then changing the view to each frame.

(2) canvas creation should not be on C-c n bc i will probably need the n bindings to go between frames. i thought about C-c c but it's going to interfere with my doentry bindings and i don't want to have to remember to switch off braille mode.
maybe C-c v and C-c C-v? like CanVas.

{% include note.html content='
> [!NOTE]
> The [manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) does not mince words in imploring us to comply with the convention to avoid C-c letter bindings, which are reserved for users. But what\'s left? braille.el takes over mouse-1, it\'s an invasive mode. I want to have painting program ergnomics.
' %}

(3) yesterday i had the idea for changing shapes it would be like C-c q to normal, w to line, e to rectangle, r to sphere. kind of like 3d applications select/translate/scale/rotate bindings.
we already have normal drawing and line, so first implement the functionality to change between forms to be able to change between these two, then i could add rectangle and ellipse.

(4) speaking of which it would be useful to have a translate tool and possibly the others. maybe then it would be like C-c w for the translate tool, C-c C-w for the line tool? or would this be annoying? and i don't see how selection would work tbh. maybe only rectangular selection and marking the start and end dot (the dot of each corner)

## Dotfiles
### Leading numbers string sort order in Emacs
To get the number of the next devlog automatically I get the last file sorted lexicographically, `car` `split-string` on hyphen to get out just the number, `string-to-number`, then increment. It occurred to me that once we exceed devlog 9 this will break, and indeed:
```elisp
(car (sort '("1-a" "2-b" "3-c" "9-b" "10-a") #'string-greaterp))  ; → 9-b
```

`sort -n` on Linux by the way is intelligent enough to deal with leading numbers properly. Hoped there would be a way in Emacs also but was not able to find one.

There is the function `version<` but it's strict over which strings it will accept. It works for the little example:
```elisp
(car (sort '("1-a" "2-b" "3-c" "9-b" "10-a") :lessp #'version< :reverse t))  ; → "10-a"
```
but not for my actual usecase:

> (error "Invalid version syntax: ‘2-braille-in-the-dumps.md’")

So it seems we have to write our own comparison. But it's easy because `string-to-number` is very nice and permissive:

> Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
STRING cannot be parsed as an integer or floating point number.

```elisp
(car (sort '("1-a" "2-b" "3-c" "9-b" "10-a")
           (lambda (a b)
             (> (string-to-number a) (string-to-number b)))))  ; → "10-a"
```

In use:
```elisp
(defun last-file-by-leading-number (path &optional match nameonly)
  "Return path of last file in PATH by leading number in filename.
If MATCH is non-nil, consider only files whose name matches regexp pattern.
If NAMEONLY is non-nil, return only name of the file, not complete path."
  (let ((full (if nameonly nil 'full)))
    (car (sort (directory-files path full match t)
               (lambda (a b)
                 (> (string-to-number a) (string-to-number b)))))))
```

### Yasnippet value based on a previous expansion in the buffer
I have a note with sections like:
```
*** Semaine 1
*** Semaine 2
*** Semaine 3
```

To be able to calculate the number for the next week automatically, I wrote this function:
```elisp
(defun gtd-semaine-n ()
  "Renvoie le numéro de la semaine prochaine selon la dernière.
Renvoie 1 si la section de la semaine dernière n'est pas trouvée."
  (save-excursion
    (end-of-buffer)
    (re-search-backward "^*** Semaine \\([0-9]+\\)" nil t)
    (1+ (string-to-number (or (match-string-no-properties 1) "0")))))
```
If there is not a previous week, like if we are writing our first week, it will return 1 so it will still function correctly.

Note that despite the permissiveness of `string-to-number`, which returns 0 if it doesn't succeed to convert to a number, one thing it really doesn't like is receiving nil as its argument, so we have to explicitly guard for that case in order to avoid an error when we have no matches.

> Debugger entered--Lisp error: (wrong-type-argument stringp nil)

To use this in a yasnippet, the function call goes between backticks:
```snippet
*** Semaine `(gtd-semaine-n)`
```

Or if we want it to have a tabstop and be able to change the value interactively when we expand the snippet:
```snippet
*** Semaine ${1:`(gtd-semaine-n)`}
```
It can also then be mirrored in the usual way:
```snippet
This is week ${1:$(yas-text)}
```

### Date n days after given date
```elisp
(defun date-n-days-after (datestr n-days)
  (format-time-string
   "%F"
   (time-add (org-time-string-to-time datestr)
             (* n-days 24 3600))))

;; (date-n-days-after "2026-02-18" 7) ; → "2026-02-25"
```
Based on [2018 code by `xu_chunyang` on /r/emacs](https://www.reddit.com/r/emacs/comments/849ycn/).

My usecase is a buffer with datestrings like:
```
<2026-02-15 à 2026-02-21>
```
to give the date of the week after that.
```elisp
(defun gtd-semaine-date ()
  "Renvoie la date de début de la semaine prochaine selon la dernière.
Renvoie la date actuelle si celle de la semaine dernière n'est pas trouvée."
  (save-excursion
    (end-of-buffer)
    (re-search-backward
     "^<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) " nil t)
    (let ((res (match-string-no-properties 1)))
      (format-time-string
       "%F"
       (if res
           (time-add (org-time-string-to-time
                      (match-string-no-properties 1))
                     (* 7 24 3600)))))))
```
If `format-time-string` doesn't receive a date or receives null for the date, it gives the current date. Which is taken advantage of here here to return current date if we don't find a matching date in the buffer.

### Yasnippet dynamic placeholders
As in [one of the previous sections](#Yasnippet value-based-on-a-previous-expansion-in-the-buffer), dynamic placeholders can be defined with backtick elisp expressions in place of a static placeholder, and this works also for mirrors. Where it breaks is the function `yas-field-value`.

`yas-field-value` is used for accessing the value of another field that we are not in the mirror of. This is useful if you need to access the value of a field while in the mirror of another one.

When trying to get the value of field with a dynamic placeholder value, it's nil until the user types to replace the placeholder.

Workaround to be able to do both; have the placeholder when the user doesn't type something, or the user input when they do type something:
```elisp
(or (yas-field-value 2) (function-that-gives-the-placeholder-value))
```

My usecase is a note like this:
```
<2026-02-15 à 2026-02-21>
- _Dimanche_ (15):
- _Lundi_ (16):
- _Mardi_ (17):
..
```
where for each day I call a function with the day's date, like 15-02 for Dimanche, 16-02 for Lundi, etc. So we need to be able to get the month from a previous field.

The dates on the first line are set up like this in the snippet:
```snippet
<${1:`(gtd-semaine-annee)`}-${2:`(gtd-semaine-mois)`}-${3:`(gtd-semaine-jour)`} à ${1:$(yas-text)}-${2:$(yas-text)}-${3:$(format "%02d" (+ 6 (string-to-number yas-text)))}>
```
where the placeholder functions are taking from the function I wrote in the [previous section](#Date-n-days-after-given-date):
```elisp
(defun gtd-semaine-annee ()
  "Renvoie l'année de `gtd-semaine-date'."
  (car (split-string (gtd-semaine-date) "-")))

(defun gtd-semaine-mois ()
  "Renvoie le mois de `gtd-semaine-date'."
  (nth 1 (split-string (gtd-semaine-date) "-")))

(defun gtd-semaine-jour ()
  "Renvoie le jour de `gtd-semaine-date'."
  (nth 2 (split-string (gtd-semaine-date) "-")))
```

Then each day is set up like this:
```snippet
- _Dimanche_ (${3:$(+ 0 (string-to-number yas-text))}):$0${3:$(p-str-taches-predefinies-pour-date (concat (or (yas-field-value 2) (gtd-semaine-mois)) "-" (format "%02d" (+ 0 (string-to-number yas-text)))))}
```
where the `+ 0` is `+ 1` for Lundi, `+ 2` for Mardi, etc.

## Site
### Devlog titles
The devlog titles make me cringe, maybe I should not have started this practice because it creates a bit of extra work and upsets the reader (me). Even this one, why did I call it spring cleaning? I think it's some attempt at wittiness. No, it's not, it's just what came to mind when I had the idea to create it, as every devlog starts with an idea of something to I want to write and it is usually named based on that first idea.

As for "creates a bit of extra work", very small amount of work and saves work in the future when trying to find a particular entry.

Going forwards maybe make the titles strictly technical, like the [git-annex devblog by Joey Hess](https://git-annex.branchable.com/devblog/) (which is what inspired this devlog in the first place) (though now that I look back into the past he had titles like "ill" which is not technical, but is still better than mine because it's not some play on words it's saying literally is what it is, what he was doing that day)

### Inline code overflow
I noticed in yesterday's devlog that the list with function names breaks on small devices because inline code, i.e. stuff between graves `like this` overlaps and doesn't word wrap.

In the DOM it's inside a:
```html
<code class="language-plaintext highlighter-rouge">
```

It's like ASCII code blocks which I explicitly don't wordwrap in order for ASCII art to work. It's not exactly the same actually, blocks are in:
```html
<div class="language-plaintext highlighter-rouge">
  <div class="highlight">
    <pre class="highlight">
      <code>
```

and in the CSS I'm targetting pre, so it shouldn't be affecting inline code:
```html
/* Don't wrap plaintext (to avoid breaking ascii art) */
.language-plaintext pre {
  white-space: pre;
  overflow-x: auto;
}
```

I guess I will just add:
```html
/* Do wrap inline code (like stuff between single grave) */
code.language-plaintext {
  white-space: pre-wrap;
  word-break: break-all;
  word-wrap: break-word;
}
```
though it breaks in the middle of camel-cased words unintelligently but I guess it's better than overflow? :-/

{% include fin.html %}
