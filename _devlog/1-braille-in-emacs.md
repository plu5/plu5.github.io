---
layout: post
title: 1 — Braille in Emacs
date: 2026-02-12 15:50
modified_date: 2026-02-13 05:47
categories: braille.el
lang: en
redirect_from: /devlog/1
---

## braille.el
### What is it
For the past two days I have been fixated on creating a drawing engine in Emacs using braille characters. Unicode provides 256 such characters, where the first one is just a blank grid (rendered the same as whitespace in most fonts), and the rest are every possible combination of the 8 points on the 2x4 braille grid. This makes it powerful for representing images, each dot as a sub-character pixel. Further aided by the fact most fonts are taller than they are wide, just like the braille grid, so each dot can be close to a square (exactly how close will depend on the font's character height).

The repository: [plu5/braille.el](https://github.com/plu5/braille.el/tree/main)

What I've got so far:
- [x] Add dot to braille character grid according to where in the character the click occurred ([cf](#how-placing-down-a-dot-works))
- [x] Mouse drag draw
- [x] Draw line
- [x] Interpolation / Don't skip when drawing quickly

I've got really high hopes for this, I even daydream being able to make animations, getting ahead of myself. I know artist-mode exists but this is 8 times more resolution and almost seamless. You can *draw* with this, you can design things, put down visual ideas, you can write with your own handwriting, integrate it seamlessly with your text notes, and spend such a tiny amount of disk space in so doing that it's negligeable next to an image file. Theoretically you could go as far as a photorealistic monochrome picture, especially with a font that renders the dots as blocks.

I let myself get carried away.

### Precision problem
Today there has been a setback because I noticed that I don't receive mouse move events sometimes when moving small amounts slowly. Which I think means that no matter how fancy of an interpolation I do, it will never be good, it will never feel precise.

### Interpolation
At present my interpolation is the simplest thing my brain could conceive, by at first putting a point in the middle of a line (`x0 + dx × 1/2`) then in the middle of that then in the middle of that... or in other words, in N steps (amount of points in between) `x0 + dx × i/steps` (`i` being the current step).

This works in pixel-space, which leads to excessive or not enough points depending the number of steps. I want to change this into dot-space, as in doing the interpolation calculation with the braille dot grid we are working with. I am reading about [Bresenham's line algorithm](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm) for this.

But as I said is it even going to help with the central problem?

### Hysteresis
Another idea I had is inspired from game design, like in platform games if the player is trying to jump onto a platform to let them succeed if they are "close enough" even if not exact is considered good game design. So we could maybe place a point down even if the mouse is not quite over its area if they are sufficiently close to it and going in that direction.

A problem with this is again not that many mouse events. We maybe can't tell that much about the user's intention.

And certainly for the case of a small stroke where we get just 1 mouse event it's just going to be a dot and there is nothing I can do about it with all the will in the world :-(

### How placing down a dot works
The start of the Unicode braille block is at 0x2800. As previously stated, the first character, 0x2800, is an empty grid, rendered as whitespace in most fonts. I ignore that one. The subsequent characters:
```elisp
(insert (+ #x2800 1))  ; ⠁ (top left dot)
(insert (+ #x2800 2))  ; ⠂ (2nd left dot)
(insert (+ #x2800 3))  ; ⠃ (^ these two together)
(insert (+ #x2800 4))  ; ⠄ (3rd left dot)
(insert (+ #x2800 5))  ; ⠅ (top and 3rd left dots)
(insert (+ #x2800 6))  ; ⠆ (2nd and 3d)
(insert (+ #x2800 7))  ; ⠇ (top three left)
```
So far the pattern is each separate dot is like a bit flag:
- `① topLeft 1 0b001`
- `② 2ndLeft 2 0b010`
- `④ 3rdLeft 4 0b100`

and bitwise ORing (`|`) them together combines them:
- `③ topLeft+2ndLeft = 0b001 | 0b010 = 0b011 = 3`
- `⑤ topLeft+3rdLeft = 0b001 | 0b100 = 0b101 = 5`
- `⑥ 2ndLeft+3rdLeft = 0b010 | 0b100 = 0b110 = 6`
- `⑦ topLeft+2ndLeft+3rdLeft = 0b001 | 0b010 | 0b100 = 0b111 = 7`

but:
```elisp
(insert (+ #x2800 8))  ; ⠈ (top right dot)
```

Where is the fourth left dot?

It's not until 64 (0b1000000)
```elisp
(insert (+ #x2800 64))  ; ⡀ (4th left)
```

The dots in braille have numbers associated. Don't confuse them with the bit flags above. They go 1237 4568. This is because originally braille was a 2x3 grid, and the fourth row was added later. The unicode characters are organised in the same way.
```
 1 0b00000001   4 0b00001000
 2 0b00000010   5 0b00010000
 3 0b00000100   6 0b00100000
 7 0b01000000   8 0b10000000
```
This means we can't just put these bitflags in a list and go off an index.
```elisp
(defun braille-bit-from-colrow (colrow)
  "Get braille dot bit at COLROW.
COLROW is (col . row) for the dot position in the 2x4 braille grid, 0-based."
  ;; unfortunately this can't be a simple data structure because
  ;; the order in braille is 1237 4568
  (let ((col (car colrow))
        (row (cdr colrow)))
    (cond
     ;; left
     ((and (= col 0) (= row 0)) #b00000001)
     ((and (= col 0) (= row 1)) #b00000010)
     ((and (= col 0) (= row 2)) #b00000100)
     ((and (= col 0) (= row 3)) #b01000000)
     ;; right
     ((and (= col 1) (= row 0)) #b00001000)
     ((and (= col 1) (= row 1)) #b00010000)
     ((and (= col 1) (= row 2)) #b00100000)
     ((and (= col 1) (= row 3)) #b10000000))))
```

Now that we have a way to get the bitflag for each dot, we can OR any number of them together to get any combination of dots.

What we want is each time the user draws get the character they are pointing at, then if it's a character in the braille block, OR the new dot to add based on which part of the character they are pointing at (left side or right side? at the top of it, 2nd row, 3rd row, 4th row?). If it's not a braille character then put the new dot as is.

The logic is as follows:

First a function to tell us if a given character is in the braille block, and if it is, the delta of it from the `braille-base` (0x2800)
```elisp
(defun braille-char-p (char)
  "If CHAR is a braille character return its delta, otherwise return nil."
  (let ((delta (- char braille-base)))
    ;; empty braille character (delta==0) intentionally omitted
    ;; (i prefer to just have space but maybe it could be configurable)
    (if (and (> delta 0) (< delta 256))
        delta)))
```
Then:
```elisp
      (save-excursion
        (goto-char char-pos)
        (let* ((char (char-after))
               (d (braille-char-p char))
               (new-dot-value
                (if d (logior d dot-bit) dot-bit)))
          (delete-char 1)
          (insert (+ #x2800 new-dot-value))))
```
The `save-excursion` is just to avoid visually moving the text cursor, which makes for a better drawing experience.

To go from there to being able to draw is just a matter of tracking the mouse after a click until release, and placing a dot on the click and on every subsequent mouse movement until release. This leads to gaps due to not receiving many mouse movement events, hence the need for interpolation.

## What else have I been doing?
Actually nothing, and I'm not supposed to be doing even that, I'm supposed to be moving out. I have a list of things that needs to get done and this isn't one of them.

{% include fin.html %}
