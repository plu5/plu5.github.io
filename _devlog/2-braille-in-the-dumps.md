---
layout: post
title: 2 — Braille in the dumps
date: 2026-02-14 11:16
modified_date: 2026-02-15 06:21
categories: braille.el
lang: en
redirect_from: /devlog/2
---

## braille.el
### Difficulties
I oscillate always between over-optimistic and over-pessimistic and I am in the down-in-the-dumps stage of my relationship with this project. I already felt it last time, that's why I wanted to capture it before it dies out.

Dot-space and Bresenham made things worse. The conversion to dot-space is too rough maybe. I don't think I have a better way because it's either converting from pixels, which is fast but involves having to make some assumptions, or Emacs line-column which is slow and expensive to work out accurately.

Also the idea "you could go as far as a photorealistic monochrome picture" seems ridiculous now, it doesn't work well zoomed out, it barely works well zoomed in.

I also finally made a minor mode for it but that was very easy to implement. Next time I am do something like this I would do that first thing instead of messing about with temporary global bindings.

### Oversights
I had written the above early in the day and since found out a lot of problems in my dot-space implementation. The major, shameful one, is reversing rows and columns in the calculation of the size of a dot; I was doing `w/nrows` and `h/ncols` and it should be the opposite. This is very basic but it's kind of confusing, isn't it, that rows are things that go along horizontally, but we count them vertically, and vice versa with columns:
```
          ← col number →
        -----------------
 ↑      ← row → |  ↑  |
 row    --------  col  --
 number         |  ↓  |
 ↓      -----------------
```
Similarly, I keep getting thrown off by the term `colrow` because we're used to width being before height all the time, thus wanting to say `rowcol`, but it refers to the column index (=number) and row index thus the width-height order is indeed `colrow`.

Astonishingly, as I was writing the section [Difficulties](#difficulties) above, despite not being able to enter any dot on certain rows due to this error, I was completely ignorant of the problem, I just felt it is less precise without noticing that we are missing some rows! I even have some tests from earlier:
```
    ⣀⣀⡀       ⣀⢀⡀⣀⡀      ⢀⣀⣀⡀  
      ⠒⡀    ⣀⣐⠒⠒⠒ ⠐⣀⡀  ⣀⡀⠒  ⢒⣀⡀
  ⠐⡀⣀⣀⣐⠂    ⡒⠂    ⢀⣀⡂  ⡂     ⢀⡂
   ⠒⠂ ⠂     ⠐⢂   ⢀⡐    ⠒⣂ ⣀⣀⣀⡐ 
              ⠒⠒⠒⠒      ⠐⠒⠂ ⠒  
  ⡀  ⡀    ⡂      ⣀⣀⣀⣀⣀⣀⣀⡀      
  ⡂  ⠒⡂   ⡂   ⠒⠒⠒⠂      ⠒⠒⠒⠒⠒⠒⠂
  ⡂   ⡂   ⡂   ⢀⣀⣀⣀⣀⡀  ⣀⣀⣀⣀⣀⣀⣀  
  ⡂   ⡂   ⢒   ⠐    ⠐⠒⠒⠂        
  ⡂   ⡂   ⢐   ⢀⣀⣀⣀⣀⡀           
  ⡂   ⡂   ⢐  ⠒⠒    ⠐⠒⠒⢂⣀⣀⣀⣀⣀⡀  
  ⠅   ⠅   ⠨                 ⠈⠉⠁
```
As you see (especially if you highlight or place your cursor in the text to see the bounds of a character) we're missing every other row. The only thing I had remarked is it's hard to draw circles, it's hard to draw horizontal lines, it's hard to handwrite.

Even after noticing the problem I thought it was something I did later that introduced it, that the tests earlier didn't have it, because clearly I had tested it and "didn't see anything like that before", like I could not believe I would not have noticed that and the only way I can believe it now is I have the proof because I had saved the tests and could go back to verify. I will endeavour to always save a few tests!

After correcting the problem:
```
           ⢀⣀       ⣀⣀⠤⠤⣀     ⢀⣀⣀⡀ 
 ⢀⡘⠉⠑⢆    ⡐⠁⠈⠑⢆    ⡜⠈    ⢣   ⢠⠋  ⠈⢆
  ⠣⣀⣀⠔⠁   ⠱⣀ ⢀⠎    ⢱⣀⣀  ⢀⠎   ⠸⣀   ⡜
  ⡄  ⢀  ⢠   ⠉⠁        ⠉⠉⠁     ⠈⠒⠒⠉ 
  ⡇  ⢸  ⢸    ⠐⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠂  
  ⢇  ⢸  ⢸    ⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠂
  ⢸  ⢸  ⢸   ⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠔⠒⠂ 
  ⢸  ⢸  ⢸    ⠤⠤⠤⠤⠤⠤⠤⠤⠤⣀⣀⣀⣀⣀⠤⠤⢄⣀⣀⣀⣀⡀
  ⠈  ⠈  ⠈                          
  ⡰    ⢀ ⢰⡄                ⡄  ⡀  ⢰ 
  ⡇⡀ ⢀ ⢸ ⢸⠁      ⡀  ⣀⡀ ⢄⢀⡀⢀⠇ ⢠⠃  ⡸ 
  ⡟⠈⣆⡮⢃⣾⢀⢾⢀⠾⡉⢣   ⢇⡀⢸⢸⣩⠃⢸⠁⠁⢸ ⡸⡎   ⠁ 
  ⠁ ⠁⠈⠁⠈⠃⠈⠃ ⠱⠊   ⠈⠈⠁      ⠈ ⠉⠁  ⠈⠁ 
```
Better but still not as fluid as I want, information is lost when handwriting quickly. I think this is due to lag reducing the amount of mouse events we receive, so it's hard to deal with it but if interpolation can be optimised further it could help.

### Pixels or line-column
Which leads me to the pixels versus line-column discussion that I touched on earlier. Essentially to be able to insert in Emacs we need the character position in the buffer. In Emacs jargon this refers to as `point`, but for clarity I call it `char-pos`. For example, to get the `char-pos` from `posn` we use the function `posn-point`. To get the `posn` from `char-pos` we use the function `posn-at-point`.

`posn` is a list with some information about our position:
```elisp
(#<window 6 on braille.el> 8034 (483 . 161) 0 nil 8034 (43 . 6) nil (10 . 11) (11 . 25))
```
You can see in <kbd>C-h f</kbd> `event-start` what each of them means (but not necessarily in order...). The important ones for braille.el are:
- `8034` : the character position. Obtained with `posn-point`. I call this `char-pos`.
- `(483 . 161)` : x and y in pixels relative to the window. Obtained with `posn-x-y`. I call this `xy`.
- `(10 . 11)` : the x and y in pixels relative to the object/glyph clicked. Obtained with `posn-object-x-y`. I call this `rel-xy`. This is used for finding out the appropriate braille point depending on where on the character the click occurred.

It's returned from functions like `event-start` and `event-end`, which I use in braille.el to tell where the mouse was clicked and where it was released, and you can also get it with the functions `posn-at-point` and `posn-at-x-y`.

braille.el works in pixels because of this. It's easy to find the `char-pos` for an `xy` pixel position with `posn-at-x-y`, and vice versa.

There are disadvantages to this approach because to interpolate properly I want to do it in dot-space, not pixels. This is the work done today, to change the interpolation to be done in dot-space, but to do this I'm just calculating the width and height of a dot based on character height and width (obtained with `window-font-width` and `window-font-height`. Before today I had used `frame-char-width` and `frame-char-height` which made it break if you change the text scale because it doesn't take that into account). Not only is this fragile, it's also a float.

Floating-point numbers are:
- the enemies of performance
- the enemies of rasterisation (we can't place down floats)

In the end I must have integers so we have to round or floor. There are a lot of places this can go wrong. Small changes can render whole rows and columns of braille inaccessible.

In elisp unless at least one operand is a float, doing an operation that would result in a floating number gets floored implicitly. In my initial calculation for the dot width and height it was getting floored implicitly, leading to it being subtly off to place a character in particular coordinates in dot-space. [When I realised and fixed it](https://github.com/plu5/braille.el/commit/72ecf50023936ad868442c78d3a794d80fdc5b29), it again rendered some rows and columns of braille dots inaccessible. Changing `ncols × x / w` to `x / w / ncols` and `nrows × y / h` to `y / h / nrows` fixed it and it now seems fairly reliable. But this is really counter-intuitive to me. There are two implicit floors here. `floor(x / floor(w / ncols))`.

It feels fragile and stupid and making too many assumptions (like dividing xy by a calculated dot wh, it's naïve!), but what is the alternative?

`posn` also gives `posn-col-row`, relative line and column position of the character from the top of the window. This is usually wrong but there is also `posn-actual-col-row`. Testing with ``(posn-actual-col-row (posn-at-point (point)))`, it seems to be pretty reliable.

> This function does not account for the width on display, like the
number of visual columns taken by a TAB or image.  If you need
the coordinates of POSITION in character units, you should use
‘posn-col-row’, not this function.
(<kbd>C-h f</kbd> `posn-actual-col-row`)

But the major problem is actually getting the `char-pos` from this. There isn't a performant way as far as I can see. The only way I know of is to actually navigate to that line and column then check `(point)`. Doing this for every single point we place is painful. Think about it, it's also every single point in an interpolation. Surely this can't be faster than the pixel calculations, floats or not. It's worth benchmarking it but I don't know how to benchmark stroke performance.

Maybe instead of writing a separate version and testing two separate versions I should include both codepaths, maybe there are situations where one method works and not the other, like in terminal Emacs we don't have pixel positions; `posn-x-y`, `posn-col-row`, `posn-actual-col-row` all return the same position.

## ... anything else?
still no

{% include fin.html %}
